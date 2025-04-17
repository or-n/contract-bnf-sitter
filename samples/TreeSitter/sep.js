module . exports = grammar (
{
  name : "grammar", rules :
  {
    _list_a : $ => optional (seq ($ . _a, repeat (seq (",", $ . _a)))), _grammar0 : $ => $ . grammar0, grammar0 : $ => $ . _list_a, _a : $ => $ . a, a : $ => /[a-zA-z][a-zA-z0-9_']*/,
  }
  ,
}
);