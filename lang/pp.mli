type token =
  | INT of (int64)
  | IDENT of (string)
  | TRUE
  | FALSE
  | LPAREN
  | RPAREN
  | AND
  | OR
  | NOT
  | EQ
  | NEQ
  | LT
  | GT
  | LTE
  | GTE
  | PLUS
  | MINUS
  | UNEGATE
  | TIMES
  | DIVIDE
  | MODULO
  | EOF
  | LET
  | IN
  | IF
  | THEN
  | ELSE

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Lexing.position * Lexing.position) Exprs.program
