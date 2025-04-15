module . exports = grammar (
{
  name : "grammar", rules :
  {
    _grammar : $ => choice (seq ("Integer", $ . _integer), seq ("Double", $ . _double), seq ("Char", $ . _char), seq ("String", $ . _string), seq ("Ident", $ . _ident))
  }
  ,
}
);