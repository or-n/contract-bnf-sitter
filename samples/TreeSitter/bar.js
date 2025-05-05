module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, _bar : $ => choice ($ . foo, $ . lol), foo : $ => seq ("foo", $ . _bar), lol : $ => "lol",
  }
  , inline : $ => [],
}
);