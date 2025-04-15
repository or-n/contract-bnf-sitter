module . exports = grammar (
{
  name : "grammar", rules :
  {
    Grammar : $ => seq ("Integer", /[0-9]+/)
  }
  ,
}
);