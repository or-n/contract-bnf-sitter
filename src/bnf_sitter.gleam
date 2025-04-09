import bnf
import gleam/bool
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import indent

pub fn drop(i) {
  i |> indent.Text |> list.wrap |> bnf.Drop
}

pub fn end(i) {
  i |> indent.Text |> list.wrap |> bnf.End
}

pub fn grammar() -> bnf.LBNF(List(indent.Token)) {
  [
    #("end", #("term", [bnf.Id("term"), "." |> end] |> bnf.Seq)),
    #("term", #("wrap", ["(" |> drop, bnf.Id("terms"), ")" |> drop] |> bnf.Seq)),
    #("terms", #(
      "term",
      [bnf.Id("term"), [" " |> drop, bnf.Id("terms")] |> bnf.Seq |> bnf.Opt]
        |> bnf.Seq,
    )),
    #("term", #(
      "pair",
      ["(" |> drop, bnf.Id("term"), ", " |> drop, bnf.Id("term"), ")" |> drop]
        |> bnf.Seq,
    )),
    #("term", #("i", bnf.Id("i"))),
    #("i", #("0", bnf.Id("0"))),
    #("i", #(
      "!0",
      ["-" |> drop |> bnf.Opt, bnf.Id("f"), bnf.Id("?0f") |> bnf.Rep]
        |> bnf.Seq,
    )),
    #("?0f", #("0", bnf.Id("0"))),
    #("?0f", #("!0", bnf.Id("f"))),
    #("f", #("f", "f" |> drop)),
    #("f", #("e", "e" |> drop)),
    #("f", #("d", "d" |> drop)),
    #("f", #("c", "c" |> drop)),
    #("f", #("b", "b" |> drop)),
    #("f", #("a", "a" |> drop)),
    #("f", #("<", bnf.Id("9"))),
    #("9", #("9", "9" |> drop)),
    #("9", #("8", "8" |> drop)),
    #("9", #("<", bnf.Id("7"))),
    #("7", #("7", "7" |> drop)),
    #("7", #("6", "6" |> drop)),
    #("7", #("5", "5" |> drop)),
    #("7", #("4", "4" |> drop)),
    #("7", #("3", "3" |> drop)),
    #("7", #("2", "2" |> drop)),
    #("7", #("<", bnf.Id("1"))),
    #("1", #("1", "1" |> drop)),
    #("0", #("0", "0" |> drop)),
  ]
}

fn drop_token(tokens, token) {
  use first <- result.try(tokens |> list.first)
  let rest = tokens |> list.drop(1)
  case token, first {
    indent.Text(drop_text), indent.Text(text) -> {
      use i <- result.try(text |> drop_string(drop_text))
      Ok(rest |> list.prepend(i |> indent.Text))
    }
    _, _ -> {
      use <- bool.guard(first != token, Error(Nil))
      Ok(rest)
    }
  }
}

fn drop_fn(i, to_drop) {
  to_drop |> list.try_fold(i, drop_token)
}

fn drop_string(i, to_drop) {
  let r = i |> string.starts_with(to_drop) |> bool.negate
  use <- bool.guard(r, Error(Nil))
  i |> string.drop_start(to_drop |> string.length) |> Ok
}

pub fn main() {
  // let ctx = bnf.Context(drop_fn: drop_string, to_string: fn(x) { x }, empty: "")
  let token_ctx =
    bnf.Context(drop_fn, to_string: indent.list_to_string, empty: [])
  let i =
    // "\t(0)\n\t\t."
    "(-2f01, (2137, 0))."
    // "(0 1 (2, 1))."
    // "(0)."
    |> indent.tokens
  let r = i |> bnf.eat_rules(grammar(), token_ctx)
  let r =
    r
    |> result.map(fn(pair) {
      let #(i, ast) = pair
      echo i
      bnf.show_ast(ast)
    })
  case r {
    Ok(text) -> text
    Error(tokens) -> {
      let str = tokens |> indent.list_to_string
      "error: " <> str
    }
  }
  |> io.println
}
