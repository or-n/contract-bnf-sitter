module . exports = grammar (
{
  name : "grammar", rules :
  {
    Grammar : $ => choice (seq ("Integer", /[0-9]+/), seq ("Double", /[0-9]+\.[0-9]+(e-?[0-9]+)?/), seq ("Char", "<Char>"), seq ("String", "<String>"), seq ("Ident", "<Ident>"))
  }
  ,
}
);