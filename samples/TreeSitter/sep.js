module . exports = grammar (
{
  name : "grammar", rules :
  {
    _list_a : $ => optional (seq ($ . _a, repeat (seq (",", $ . _a)))), _list_b : $ => repeat1 (seq ($ . _b, ",")), _top : $ => choice ($ . a, $ . b), a : $ => seq ("A", $ . _list_a), b : $ => seq ("B", $ . _list_b), _a : $ => $ . a, a : $ => /[a-zA-z][a-zA-z0-9_']*/, _b : $ => $ . b, b : $ => /[a-zA-z][a-zA-z0-9_']*/,
  }
  ,
}
);