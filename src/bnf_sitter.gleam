import bnf.{Id, Opt, Rep, Seq}
import gleam/io
import gleam/list
import gleam/result
import gleam/string
import indent
import util

pub fn drop(i) {
  i |> indent.Text |> list.wrap |> bnf.Drop
}

pub fn end(i) {
  i |> indent.Text |> list.wrap |> bnf.End
}

pub fn char_range(name, start, end) {
  list.range(start |> util.ord, end |> util.ord)
  |> list.try_map(string.utf_codepoint)
  |> result.unwrap([])
  |> list.map(fn(c) { c |> list.wrap |> string.from_utf_codepoints })
  |> list.map(fn(s) { #(name, #(s, drop(s))) })
}

pub fn graphemes(text) {
  let #(acc, _) =
    #([], text)
    |> util.iterate(fn(pair) {
      let #(acc, text) = pair
      case text |> string.pop_grapheme() {
        Ok(pair) -> {
          let #(first, rest) = pair
          #([first, ..acc], rest) |> Ok
        }
        _ -> pair |> Error
      }
    })
  acc |> list.reverse
}

pub fn chars(name, text) {
  text |> graphemes |> list.map(fn(s) { #(name, #(s, drop(s))) })
}

pub fn grammar() {
  [
    [
      #("end", #("term", [Id("t"), end(".")] |> Seq)),
      #("t", #("wrap", [drop("("), Id("terms"), drop(")")] |> Seq)),
      #("terms", #("term", [Id("t"), Id("app"), Id("app") |> Rep] |> Seq)),
      #("app", #("app", [drop(" "), Id("t")] |> Seq)),
      #("t", #(",", [drop("("), Id("t"), drop(", "), Id("t"), drop(")")] |> Seq)),
      #("t", #("i", Id("i"))),
      #("t", #("id", Id("id"))),
      #("id", #("alpha", [Id("alpha"), Id("?alphai") |> Rep] |> Seq)),
      #("?alphai", #("alpha", Id("alpha"))),
      #("?alphai", #("i", Id("i"))),
      #("i", #("!0", [drop("-") |> Opt, Id("u")] |> Seq)),
      #("i", #("0", Id("0"))),
      #("u", #("9", [Id("9"), Id("?09") |> Rep] |> Seq)),
      #("u", #("f", [drop("0x"), Id("f"), Id("?0f") |> Rep] |> Seq)),
      #("?0f", #("0", Id("0"))),
      #("?0f", #("!0", Id("f"))),
      #("?09", #("0", Id("0"))),
      #("?09", #("!0", Id("9"))),
      #("f", #("<", Id("9"))),
      #("9", #("<", Id("7"))),
      #("7", #("<", Id("1"))),
      #("1", #("1", drop("1"))),
      #("0", #("0", drop("0"))),
      #("alpha", #("_", drop("_"))),
      #("alpha", #("lower", Id("lower"))),
      #("alpha", #("upper", Id("upper"))),
    ],
    char_range("f", "a", "f"),
    char_range("9", "8", "9"),
    char_range("7", "2", "7"),
    char_range("lower", "a", "z"),
    char_range("upper", "A", "Z"),
    chars("lower", pl),
    chars("upper", pl |> string.uppercase),
  ]
  |> list.flatten
}

pub const pl = "ąćęłńóśźż"

pub const default_ctx = bnf.Context(
  drop: util.drop_string,
  to_string: util.id,
  empty: "",
)

pub const indent_ctx = bnf.Context(
  drop: indent.drop_token_list,
  to_string: indent.token_list_to_string,
  empty: [],
)

pub const examples = [
  #("a", "\t(0)\n\t\t."),
  #("b", "(-0x2f01, (2137, 0))."),
  #("c", "(0 1 (2, 1))."),
  #("d", "0."),
  #("e", "(_a21 0xa)."),
  #("pl", "Żółć"),
]

pub fn main() {
  let i =
    examples
    |> list.key_find("pl")
    |> result.unwrap("")
    |> indent.tokens
  let r =
    i
    |> bnf.eat_rules(grammar(), indent_ctx)
    |> result.map(fn(pair) {
      let #(i, ast) = pair
      let i_str = i |> indent.token_list_to_string |> util.quote
      io.println("i: " <> i_str)
      bnf.show_ast(ast)
    })
  case r {
    Ok(text) -> text
    _ -> i |> indent.token_list_to_string |> util.quote
  }
  |> io.println
}
