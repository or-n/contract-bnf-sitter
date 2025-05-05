module . exports = grammar (
{
  name : "grammar", rules :
  {
    source_file : $ => $ . top, top : $ => $ . mk, mk : $ => $ . _digit, _digit : $ => choice ($ . zero, $ . one, $ . two, $ . three, $ . four, $ . five, $ . six, $ . seven, $ . eight, $ . nine), zero : $ => "0", one : $ => "1", two : $ => "2", three : $ => "3", four : $ => "4", five : $ => "5", six : $ => "6", seven : $ => "7", eight : $ => "8", nine : $ => "9",
  }
  , inline : $ => [],
}
);