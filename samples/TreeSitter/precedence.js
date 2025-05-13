module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, top : $ => $ . exp, exp : $ => $ . _exp, _exp : $ => choice ($ . shift, $ . toExp), shift : $ => seq ($ . _exp, "+", $ . _exp1), toExp : $ => $ . _exp1, _exp1 : $ => choice ($ . scale, $ . toExp1), scale : $ => seq ($ . _exp1, "*", $ . _exp2), toExp1 : $ => $ . _exp2, _exp2 : $ => choice ($ . number, $ . toExp2), number : $ => /[0-9]+/, toExp2 : $ => seq ("(", $ . _exp, ")"),
  }
  , inline : $ => [],
}
);