{
  open Lexing
  open Parser
  let commentdepth = ref 0;;

  type t = 
  {startpos : Lexing.position;
   endpos : Lexing.position;}

  exception Lexer_err of string * t

  let raise_error loc msg =
    raise @@ Lexer_err (msg,loc)
}

let white = [' ' '\t']+
let newline = '\n' | '\r' | "\r\n"
let digit = ['0'-'9']
let tfloat = tdigit+'.'tdigit*
let tint = tdigit+
let tid = ['a'-'z' 'A'-'Z']+['_' 'a'-'z' 'A'-'Z' '0'-'9']*


rule token = parse
    white {token lexbuf}
  | "//" {single_comment lexbuf}
  | "/*" {incr commentdepth;multi_comment lexbuf}
  | '"' { string (Buffer.create 16) lexbuf }
  | eof { EOF }
  | newline { new_line lexbuf; token lexbuf}
  | "array" {ARRAY}
  | "break" {BREAK}
  | "do" {DO}
  | "else" {ELSE}
  | "end" {END}
  | "for" {FOR}
  | "function" {FUNCTION}
  | "if" {IF}
  | "in" {IN}
  | "let" {LET}
  | "nil" {NIL}
  | "of" {OF}
  | "then" {THEN}
  | "to" {TO}
  | "type" {TYPE}
  | "var" {VAR}
  | "while" {WHILE}
  | "," {COMMA}
  | ":" {COLON}
  | ";" {SEMI}
  | "." {DOT}
  | "(" {LPAREN}
  | ")" {RPAREN}
  | "{" {LBRACE} 
  | "}" {RBRACE} 
  | "[" {LBracket} 
  | "]" {RBracket}
  | "+" {PLUS}
  | "-" {MINUS}
  | "*" {TIMES}
  | "/" {DIVIDE}
  | "=" {EQ}
  | "<>" {NOTEQ}
  | ">" {GT}
  | ">=" {GE}
  | "<" {LT}
  | "<=" {LE}
  | "&" {AND}
  | "|" {OR}
  | "not" {NOT}
  | ":=" {ASSIGNMENT}
  | tint as lxm {INTCONST(int_of_string lxm)}
  | tfloat as lxm {FLOATCONST(float_of_string lxm)}
  | "true" {BOOLCONST(true)}
  | "false" {BOOLCONST(false)}
  | tid as lxm {ID(lxm)}
  | _  {raise_error "wrong token" {(lexeme_start_p lexbuf);(lexeme_end_p lexbuf)}}

and single_comment = parse
  newline {next_line lexbuf;token lexbuf}
  | _ {single_comment lexbuf}

and multi_comment = parse
  newline {next_line lexbuf;multi_comment lexbuf}
  | "*/" {decr commentdepth;
          if !commentdepth = 0 then token lexbuf else multi_comment lexbuf}
  | "/*" {incr commentdepth;multi_comment lexbuf}
  | eof {raise_error "can't end comment" {(lexeme_start_p lexbuf);(lexeme_end_p lexbuf)}}
  | _ {multi_comment lexbuf}
  
and string buf = parse
  | '"' {
      STRINGCONST (Buffer.contents buf)
    }
  | '\n' {
      new_line lexbuf;
      Buffer.add_char buf '\n';
      string buf lexbuf
    }
  | _ as c {
      Buffer.add_char buf c;
      string buf lexbuf
    }
  | eof {raise_error "can't end parse string" {(lexeme_start_p lexbuf);(lexeme_end_p lexbuf)}}