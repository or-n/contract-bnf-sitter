import gleam/bool
import gleam/list
import gleam/result
import gleam/string
import indent

pub type BNF(i) {
  Id(String)
  Drop(i)
  End(i)
  Seq(List(BNF(i)))
  Opt(BNF(i))
  Rep(BNF(i))
}

pub type LBNF(i) =
  List(#(String, #(String, BNF(i))))

pub type AST {
  Node(String, List(AST))
}

pub fn quote(text) {
  "\"" <> text <> "\""
}

pub fn show_ast(ast) {
  let Node(label, children) = ast
  let label_str = label
  use <- bool.guard(children |> list.is_empty, label_str |> quote)
  let children_str = children |> list.map(show_ast) |> string.join(", ")
  label_str <> "(" <> children_str <> ")"
}

pub type Context(a) {
  Context(
    drop_fn: fn(a, a) -> Result(a, Nil),
    empty: a,
    to_string: fn(a) -> String,
  )
}

pub fn eat_rules(i: List(indent.Token), grammar, ctx) {
  grammar
  |> list.fold_until(Error(i), fn(acc, rule) {
    case eat_rule(i, rule, grammar, ctx) {
      Ok(ok) -> ok |> Ok |> list.Stop
      _ -> acc |> list.Continue
    }
  })
}

pub fn eat_rule(i: a, rule, grammar, ctx) {
  let #(label, #(_variant, bnf)) = rule
  echo label
  use #(i, asts) <- result.try(drop_bnf(i, bnf, grammar, ctx))
  #(i, Node(label, asts)) |> Ok
}

pub fn drop_bnf(i: a, bnf, grammar: LBNF(a), ctx: Context(a)) {
  case bnf {
    Id(label) -> {
      let bnfs = grammar |> list.key_filter(label)
      bnfs
      |> list.fold_until(Error(Nil), fn(acc, variant_bnf) {
        let #(_variant, other_bnf) = variant_bnf
        case drop_bnf(i, other_bnf, grammar, ctx) {
          Ok(r) -> {
            let #(i, asts) = r
            #(i, [Node(label, asts)]) |> Ok |> list.Stop
          }
          _ -> acc |> list.Continue
        }
      })
    }
    Drop(drop_i) -> {
      echo drop_i
      use i <- result.try(i |> ctx.drop_fn(drop_i))
      let drop_i_str = drop_i |> ctx.to_string
      #(i, [Node(drop_i_str, [])]) |> Ok
    }
    End(drop_i) -> {
      echo drop_i
      use i <- result.try(i |> ctx.drop_fn(drop_i))
      use <- bool.guard(i != ctx.empty, Error(Nil))
      #(ctx.empty, []) |> Ok
    }
    Seq(bnfs) ->
      bnfs
      |> list.try_fold(#(i, []), fn(acc, other_bnf) {
        let #(i, labels) = acc
        use #(i, labels2) <- result.try(drop_bnf(i, other_bnf, grammar, ctx))
        #(i, labels |> list.append(labels2)) |> Ok
      })
    Opt(other_bnf) ->
      drop_bnf(i, other_bnf, grammar, ctx)
      |> result.unwrap(#(i, []))
      |> Ok
    Rep(other_bnf) ->
      case drop_bnf(i, other_bnf, grammar, ctx) {
        Ok(#(i, labels)) -> {
          use #(i, labels2) <- result.try(drop_bnf(i, bnf, grammar, ctx))
          #(i, labels |> list.append(labels2)) |> Ok
        }
        _ -> Ok(#(i, []))
      }
  }
}
