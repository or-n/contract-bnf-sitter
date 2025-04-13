module.exports = grammar({
  name: "quoted_string",

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.string
    ),
   
    string: $ => 
      seq(
        "\"",
        repeat(choice(
          new RustRegex("([^\"\\]|\\.)*"),
          seq("\\", $.escaped_char)
        )),
        "\""
      ),

    escaped_char: $ => choice("b", "f", "n", "r", "t", "v"),
  },
});
