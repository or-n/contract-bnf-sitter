module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, top : $ => choice ($ . integer, $ . double, $ . char, $ . string, $ . identT), integer : $ => seq ("Integer", /[0-9]+/), double : $ => seq ("Double", /[0-9]+\.[0-9]+(e-?[0-9]+)?/), char : $ => seq ("Char", /'([^'\\]|\\['\\tnrf])'/), string : $ => seq ("String", /"([^"\\]|\\["\\tnrf])*"/), identT : $ => seq ("Ident", /[a-zA-z][a-zA-z0-9_']*/),
  }
  , inline : $ => [],
}
);