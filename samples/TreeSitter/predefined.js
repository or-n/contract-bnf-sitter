module . exports = grammar (
{
  name : "grammar", rules :
  {
    grammar : $ => choice ($ . integer, $ . double, $ . char, $ . string, $ . ident), integer : $ => seq ("Integer", /[0-9]+/), double : $ => seq ("Double", /[0-9]+\.[0-9]+(e-?[0-9]+)?/), char : $ => seq ("Char", /'([^'\\]|\\[tnrf])'/), string : $ => seq ("String", /"([^"\\]|\\[tnrf])*"/), ident : $ => seq ("Ident", /[a-zA-z][a-zA-z0-9_']*/),
  }
  ,
}
);