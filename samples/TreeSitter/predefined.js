module . exports = grammar (
{
  name : "grammar", rules :
  {
    grammar : $ => choice (seq ("Integer", /[0-9]+/), seq ("Double", /[0-9]+\.[0-9]+(e-?[0-9]+)?/), seq ("Char", /'([^'\\]|\\[tnrf])'/), seq ("String", /"([^"\\]|\\[tnrf])*"/), seq ("Ident", /[a-zA-z][a-zA-z0-9_']*/))
  }
  ,
}
);