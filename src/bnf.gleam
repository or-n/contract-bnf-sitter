import gleam/bool
import gleam/list
import gleam/result
import gleam/string

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
  use <- bool.guard(children |> list.is_empty, label |> quote)
  let children_str = children |> list.map(show_ast) |> string.join(", ")
  label <> "(" <> children_str <> ")"
}

pub fn eat_rules(i, grammar) {
  grammar
  |> list.fold_until(Error(i), fn(acc, rule) {
    case eat_rule(i, rule, grammar) {
      Ok(ok) -> ok |> Ok |> list.Stop
      _ -> acc |> list.Continue
    }
  })
}

pub fn eat_rule(i, rule, grammar) {
  let #(label, #(_variant, bnf)) = rule
  echo label
  use #(i, asts) <- result.try(drop_bnf(i, bnf, grammar))
  #(i, Node(label, asts)) |> Ok
}

pub fn drop_bnf(i, bnf, grammar) {
  case bnf {
    Id(label) -> {
      let bnfs = grammar |> list.key_filter(label)
      bnfs
      |> list.fold_until(Error(Nil), fn(acc, labeled_bnf) {
        let #(_variant, other_bnf) = labeled_bnf
        case drop_bnf(i, other_bnf, grammar) {
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
      use <- bool.guard(
        i |> string.starts_with(drop_i) |> bool.negate,
        Error(Nil),
      )
      let i = i |> string.drop_start(drop_i |> string.length)
      #(i, [Node(drop_i, [])]) |> Ok
    }
    End(drop_i) -> {
      echo drop_i
      use <- bool.guard(i != drop_i, Error(Nil))
      #("", []) |> Ok
    }
    Seq(bnfs) ->
      bnfs
      |> list.try_fold(#(i, []), fn(acc, other_bnf) {
        let #(i, labels) = acc
        use #(i, labels2) <- result.try(drop_bnf(i, other_bnf, grammar))
        #(i, labels |> list.append(labels2)) |> Ok
      })
    Opt(other_bnf) ->
      drop_bnf(i, other_bnf, grammar)
      |> result.unwrap(#(i, []))
      |> Ok
    Rep(other_bnf) ->
      case drop_bnf(i, other_bnf, grammar) {
        Ok(#(i, labels)) -> {
          use #(i, labels2) <- result.try(drop_bnf(i, bnf, grammar))
          #(i, labels |> list.append(labels2)) |> Ok
        }
        _ -> Ok(#(i, []))
      }
  }
}
