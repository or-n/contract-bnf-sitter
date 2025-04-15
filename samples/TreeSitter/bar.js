module . exports = grammar (
{
  name : "grammar", rules :
  {
    _bar : $ => choice (seq ("foo", $ . _bar), "lol")
  }
  ,
}
);