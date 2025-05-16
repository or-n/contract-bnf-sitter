module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, top : $ => $ . exp, exp : $ => $ . _exp, _exp : $ => choice ($ . shift, $ . _exp1), shift : $ => seq ($ . _exp, "+", $ . _exp1), _exp1 : $ => choice ($ . scale, $ . _exp2), scale : $ => seq ($ . _exp1, "*", $ . _exp2), _exp2 : $ => choice ($ . number, seq ("(", $ . _exp, ")")), number : $ => /[0-9]+/,
  }
  , inline : $ => [],
}
);