import bnf
import gleam/int
import gleam/io
import gleam/list
import gleam/result

// pub fn or(a, b) {
//   [a, b] |> bnf.Alt
// }

// pub fn drop_alt(xs) {
//   xs |> list.map(bnf.Drop) |> bnf.Alt
// }

// pub fn dec() {
//   list.range(1, 9) |> list.map(int.to_string) |> drop_alt
// }

// pub fn hex() {
//   ["a", "b", "c", "d", "e", "f"] |> drop_alt |> or(bnf.Id("dec"))
// }

// pub fn number() {
//   [
//     "-" |> bnf.Drop |> bnf.Opt,
//     bnf.Id("hex"),
//     bnf.Id("hex") |> or("0" |> bnf.Drop) |> bnf.Rep,
//   ]
//   |> bnf.Seq
//   |> or("0" |> bnf.Drop)
// }

// pub fn end() {
//   [bnf.Id("number"), "." |> bnf.End] |> bnf.Seq
// }

// pub fn grammar() {
//   [#("end", end()), #("number", number()), #("hex", hex()), #("dec", dec())]
// }

pub fn grammar() {
  [
    #("end", #("terms", [bnf.Id("term"), "." |> bnf.End] |> bnf.Seq)),
    #("term", #(
      "wrap",
      ["(" |> bnf.Drop, bnf.Id("terms"), ")" |> bnf.Drop] |> bnf.Seq,
    )),
    #("terms", #(
      "term",
      [bnf.Id("term"), [" " |> bnf.Drop, bnf.Id("terms")] |> bnf.Seq |> bnf.Opt]
        |> bnf.Seq,
    )),
    #("term", #(
      "pair",
      [
        "(" |> bnf.Drop,
        bnf.Id("term"),
        ", " |> bnf.Drop,
        bnf.Id("term"),
        ")" |> bnf.Drop,
      ]
        |> bnf.Seq,
    )),
    #("term", #("i", bnf.Id("i"))),
    #("i", #("0", bnf.Id("0"))),
    #("i", #(
      "!0",
      ["-" |> bnf.Drop |> bnf.Opt, bnf.Id("f"), bnf.Id("?0f") |> bnf.Rep]
        |> bnf.Seq,
    )),
    #("?0f", #("0", bnf.Id("0"))),
    #("?0f", #("!0", bnf.Id("f"))),
    #("f", #("f", "f" |> bnf.Drop)),
    #("f", #("e", "e" |> bnf.Drop)),
    #("f", #("d", "d" |> bnf.Drop)),
    #("f", #("c", "c" |> bnf.Drop)),
    #("f", #("b", "b" |> bnf.Drop)),
    #("f", #("a", "a" |> bnf.Drop)),
    #("f", #("<", bnf.Id("9"))),
    #("9", #("9", "9" |> bnf.Drop)),
    #("9", #("8", "8" |> bnf.Drop)),
    #("9", #("<", bnf.Id("7"))),
    #("7", #("7", "7" |> bnf.Drop)),
    #("7", #("6", "6" |> bnf.Drop)),
    #("7", #("5", "5" |> bnf.Drop)),
    #("7", #("4", "4" |> bnf.Drop)),
    #("7", #("3", "3" |> bnf.Drop)),
    #("7", #("2", "2" |> bnf.Drop)),
    #("7", #("<", bnf.Id("1"))),
    #("1", #("1", "1" |> bnf.Drop)),
    #("0", #("0", "0" |> bnf.Drop)),
  ]
}

pub fn main() {
  // "(-2f01, (2137, 0))."
  // "(0 1 (2, 1))."
  "(0)."
  |> bnf.eat_rules(grammar())
  |> result.map(fn(pair) {
    let #(i, ast) = pair
    echo i
    bnf.show_ast(ast)
  })
  |> result.unwrap("error")
  |> io.println
}
