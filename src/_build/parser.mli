
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | TYPE
  | TO
  | TIMES
  | THEN
  | STRINGCONST of (string)
  | SEMI
  | RPAREN
  | RBracket
  | RBRACE
  | PLUS
  | OR
  | OF
  | NOTEQ
  | NOT
  | NIL
  | MINUS
  | LT
  | LPAREN
  | LET
  | LE
  | LBracket
  | LBRACE
  | INTCONST of (int)
  | IN
  | IF
  | ID of (string)
  | GT
  | GE
  | FUNCTION
  | FOR
  | FLOATCONST of (float)
  | EQ
  | EOF
  | END
  | ELSE
  | DOT
  | DO
  | DIVIDE
  | COMMA
  | COLON
  | BREAK
  | BOOLCONST of (bool)
  | ASSIGNMENT
  | ARRAY
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast2.exp)
