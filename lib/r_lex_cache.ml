include Lexer_cache.Make (struct
  type token = R_lexer.token
  type mode = R_lexer.mode

  let initial_mode = R_lexer.Normal
  let lex_line = R_lexer.lex_line
  let lex_as_default = R_lexer.lex_as_default
end)
