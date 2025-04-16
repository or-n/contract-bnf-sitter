module . exports = grammar (
{
  name : "grammar", rules :
  {
    grammar : $ => choice ($ . Integer, $ . Double, $ . Char, $ . String, $ . Ident), Integer : $ => seq ("Integer", /[0-9]+/), Double : $ => seq ("Double", /[0-9]+\.[0-9]+(e-?[0-9]+)?/), Char : $ => seq ("Char", /'([^'\\]|\\[tnrf])'/), String : $ => seq ("String", /"([^"\\]|\\[tnrf])*"/), Ident : $ => seq ("Ident", /[a-zA-z][a-zA-z0-9_']*/),
  }
  ,
}
);