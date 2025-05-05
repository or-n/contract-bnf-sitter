const c0 = "constant";
module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, top : $ => choice ($ . a, $ . b), a : $ => $ . _a, b : $ => $ . _b, _a : $ => $ . const, const : $ => c0, _b : $ => $ . const, const : $ => c0,
  }
  , inline : $ => [],
}
);