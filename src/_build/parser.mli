type token =
  | NUM of (int64)
  | ID of (string)
  | DEF
  | ANDDEF
  | ADD1
  | SUB1
  | LPARENSPACE
  | LPARENNOSPACE
  | RPAREN
  | LBRACK
  | RBRACK
  | LET
  | IN
  | EQUAL
  | COMMA
  | PLUS
  | MINUS
  | TIMES
  | IF
  | COLON
  | ELSECOLON
  | EOF
  | PRINT
  | PRINTSTACK
  | TRUE
  | FALSE
  | ISBOOL
  | ISNUM
  | ISTUPLE
  | EQEQ
  | LESSSPACE
  | GREATER
  | LESSEQ
  | GREATEREQ
  | AND
  | OR
  | NOT
  | COLONEQ
  | SEMI
  | NIL
  | LAMBDA
  | BEGIN
  | END
  | SHADOW
  | REC
  | UNDERSCORE
  | THREAD
  | GET
  | START

val program :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> (Lexing.position * Lexing.position) Exprs.program
