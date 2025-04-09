import gleam/bool
import gleam/int
import gleam/list
import gleam/order
import gleam/result
import gleam/string

pub type Token {
  Indent
  Deindent
  Text(String)
}

pub fn to_string(token) {
  case token {
    Text(text) -> text
    Indent -> "<Indent>"
    Deindent -> "<Deindent>"
  }
}

pub fn list_to_string(tokens) {
  tokens |> list.map(to_string) |> string.concat
}

pub fn drop_string(i, to_drop) {
  let r = i |> string.starts_with(to_drop) |> bool.negate
  use <- bool.guard(r, Error(Nil))
  i |> string.drop_start(to_drop |> string.length) |> Ok
}

pub fn drop_token(tokens, token) {
  use first <- result.try(tokens |> list.first)
  let rest = tokens |> list.drop(1)
  case token, first {
    Text(drop_text), Text(text) -> {
      use i <- result.try(text |> drop_string(drop_text))
      Ok(rest |> list.prepend(i |> Text))
    }
    _, _ -> {
      use <- bool.guard(first != token, Error(Nil))
      Ok(rest)
    }
  }
}

pub fn drop_token_list(i, to_drop) {
  to_drop |> list.try_fold(i, drop_token)
}

pub fn iterate(x, f) {
  case f(x) {
    Ok(x) -> iterate(x, f)
    _ -> x
  }
}

pub fn drop_last(xs) {
  xs |> list.reverse |> list.drop(1) |> list.reverse
}

pub fn tokens(i) {
  let #(_, lines) =
    i
    |> string.split("\n")
    |> list.map(fn(line) {
      iterate(#(line, 0), fn(pair) {
        let #(i, tab_count) = pair
        case i {
          "\t" <> i -> Ok(#(i, tab_count + 1))
          _ -> Error(Nil)
        }
      })
    })
    |> list.append([#("", 0)])
    |> list.map_fold(0, fn(before, pair) {
      let #(line, after) = pair
      let d = after - before
      let tokens =
        case d |> int.compare(0) {
          order.Lt -> Deindent |> list.repeat(d |> int.absolute_value)
          order.Eq -> []
          order.Gt -> Indent |> list.repeat(d)
        }
        |> list.append([Text(line)])
      #(after, tokens)
    })
  lines |> list.flatten |> drop_last
}
