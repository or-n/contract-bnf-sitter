module . exports = grammar (
{
  name : "grammar", rules :
  {
    _grammar : $ => $ . _a,
    // grammar : $ => $ . _a,
    seq2 : $ => $ . _list_a,
    _a : $ => $ . a,
    a : $ => $ . _ident,
  }
  ,
}
);
