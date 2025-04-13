module.exports = grammar({
  name: "quoted_string",

  rules: {
    source_file: $ => repeat($._statement),

    _statement: $ => choice(
      $.string
    ),

    /*
    string: $ => choice(
      seq('"', repeat(choice($.escaped_char, /[^"\\]/)), '"'),
      seq("'", repeat(choice($.escaped_char, /[^'\\]/)), "'")
    ),

    escaped_char: $ => seq('\\', /["'\\bfnrtv]/),
    */
    
    string: $ => choice(
      seq('"', repeat(choice($.escaped_char, new RustRegex("[\[\]]"))), '"'),
      seq("'", repeat(choice($.escaped_char, new RustRegex("[\[\]]"))), "'")
    ),

    escaped_char: $ => seq("\\", "\\"),
  },
});
