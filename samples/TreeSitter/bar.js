module . exports = grammar (
{
  name : "grammar", rules :
  {
    _bar : $ => choice ($ . foo, $ . lol), foo : $ => seq ("foo", $ . _bar), lol : $ => "lol",
  }
  ,
}
);