import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub fn id(x) {
  x
}

pub fn quote(text) {
  "\"" <> text <> "\""
}

pub fn drop_string(i, to_drop) {
  let starts_with = i |> string.starts_with(to_drop)
  use <- bool.guard(starts_with |> bool.negate, Error(Nil))
  i |> string.drop_start(to_drop |> string.length) |> Ok
}

pub fn iterate(x, f) {
  case f(x) {
    Ok(x) -> iterate(x, f)
    _ -> x
  }
}

pub fn ord(char_str) {
  char_str
  |> string.to_utf_codepoints
  |> list.first
  |> result.map(string.utf_codepoint_to_int)
  |> result.unwrap(0)
}

pub fn drop_last(xs) {
  let #(before, _) = xs |> list.split(xs |> list.length |> int.subtract(1))
  before
}
