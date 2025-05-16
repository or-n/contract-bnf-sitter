module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, _list_a : $ => seq ($ . _a, repeat (seq (",", $ . _a))), _list_b : $ => repeat1 (seq ($ . _b, ",")), top : $ => choice ($ . a, $ . b, $ . c), a : $ => seq ("A", optional ($ . _list_a)), b : $ => seq ("B", $ . _list_b), c : $ => seq ("C", $ . _list_c), _a : $ => $ . mkA, mkA : $ => /[a-zA-z][a-zA-z0-9_']*/, _b : $ => $ . mkB, mkB : $ => /[a-zA-z][a-zA-z0-9_']*/, _c : $ => $ . mkC, mkC : $ => /[a-zA-z][a-zA-z0-9_']*/, _list_c : $ => choice (seq ($ . _c, ";"), seq ($ . _c, ";", $ . _list_c)),
  }
  , inline : $ => [],
}
);