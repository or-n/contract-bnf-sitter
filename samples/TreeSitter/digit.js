module . exports = grammar (
{
  name : "grammar", rules :
  {
    Grammar : $ => $ . Digit, Digit : $ => choice ("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")
  }
  ,
}
);