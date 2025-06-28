module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, _list_a : $ => seq ($ . _a, repeat (seq (",", $ . _a))), _list_b : $ => repeat1 (seq ($ . _b, ",")), _list_c : $ => choice (seq ($ . _c, ";", $ . _list_c), seq ($ . _c, ";")), _list_d : $ => seq ($ . _d, "*", $ . _list_d), _a : $ => $ . mkA, mkA : $ => /[a-zA-z][a-zA-z0-9_']*/, _b : $ => $ . mkB, mkB : $ => /[a-zA-z][a-zA-z0-9_']*/, _c : $ => $ . mkC, mkC : $ => /[a-zA-z][a-zA-z0-9_']*/, _d : $ => $ . mkD, mkD : $ => /[a-zA-z][a-zA-z0-9_']*/, top : $ => choice ($ . d, $ . c, $ . b, $ . a), d : $ => seq ("D", $ . _list_d), c : $ => seq ("C", $ . _list_c), b : $ => seq ("B", $ . _list_b), a : $ => seq ("A", optional ($ . _list_a)),
  }
  , inline : $ => [],
}
);