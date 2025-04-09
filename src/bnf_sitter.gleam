import bnf.{Id}
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
    #("end", #("terms", [Id("term"), "." |> end] |> bnf.Seq)),
    #("term", #("wrap", ["(" |> drop, Id("terms"), ")" |> drop] |> bnf.Seq)),
    #("terms", #(
      "term",
      [Id("term"), [" " |> drop, Id("terms")] |> bnf.Seq |> bnf.Opt]
        |> bnf.Seq,
    )),
    #("term", #(
      "pair",
      ["(" |> drop, Id("term"), ", " |> drop, Id("term"), ")" |> drop]
        |> bnf.Seq,
    )),
    #("term", #("i", Id("i"))),
    #("i", #("0", Id("0"))),
    #("i", #(
      "!0",
      ["-" |> drop |> bnf.Opt, Id("f"), Id("?0f") |> bnf.Rep]
        |> bnf.Seq,
    )),
    #("?0f", #("0", Id("0"))),
    #("?0f", #("!0", Id("f"))),
    #("f", #("f", "f" |> drop)),
    #("f", #("e", "e" |> drop)),
    #("f", #("d", "d" |> drop)),
    #("f", #("c", "c" |> drop)),
    #("f", #("b", "b" |> drop)),
    #("f", #("a", "a" |> drop)),
    #("f", #("<", Id("9"))),
    #("9", #("9", "9" |> drop)),
    #("9", #("8", "8" |> drop)),
    #("9", #("<", Id("7"))),
    #("7", #("7", "7" |> drop)),
    #("7", #("6", "6" |> drop)),
    #("7", #("5", "5" |> drop)),
    #("7", #("4", "4" |> drop)),
    #("7", #("3", "3" |> drop)),
    #("7", #("2", "2" |> drop)),
    #("7", #("<", Id("1"))),
    #("1", #("1", "1" |> drop)),
    #("0", #("0", "0" |> drop)),
  ]
}

pub fn main() {
  // let ctx = bnf.Context(drop: indent.drop_string, to_string: fn(x) { x }, empty: "")
  let indent_ctx =
    bnf.Context(
      drop: indent.drop_token_list,
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
      let str = i |> indent.list_to_string |> bnf.quote
      let str = "i: " <> str
      str |> io.println
      ast |> bnf.show_ast
    })
  case r {
    Ok(text) -> text
    Error(i) -> {
      let str = i |> indent.list_to_string
      "error: " <> str
    }
  }
  |> io.println
}
