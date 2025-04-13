const constant = "constant";
module . exports = grammar (
{
  name : "grammar", rules :
  {
    Grammar : $ => choice ($ . A, $ . B), A : $ => constant, B : $ => constant
  }
  ,
}
);