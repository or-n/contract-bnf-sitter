module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => choice ($ . _a, $ . _b, $ . _c), _a : $ => $ . a, a : $ => "A", _b : $ => $ . b, b : $ => "B", _c : $ => $ . c, c : $ => "C",
  }
  , inline : $ => [],
}
);