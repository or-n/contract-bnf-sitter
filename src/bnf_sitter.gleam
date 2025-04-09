import bnf
import gleam/io
import gleam/list
import gleam/result
import indent

pub fn drop(i) {
  i |> indent.Text |> list.wrap |> bnf.Drop
}

pub fn end(i) {
  i |> indent.Text |> list.wrap |> bnf.End
}

pub fn grammar() {
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

pub fn main() {
  // let ctx = bnf.Context(drop_fn: drop_string, to_string: fn(x) { x }, empty: "")
  let indent_ctx =
    bnf.Context(
      drop_fn: indent.drop_token_list,
      to_string: indent.list_to_string,
      empty: [],
    )
  let r =
    // "\t(0)\n\t\t."
    "(-2f01, (2137, 0))."
    // "(0 1 (2, 1))."
    // "(0)."
    |> indent.tokens
    |> bnf.eat_rules(grammar(), indent_ctx)
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
