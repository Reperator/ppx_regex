{
open Prsr
}

let white = [' ' '\t' '\r' '\n']+

rule read =
  parse
  | white { read lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '*' { STAR }
  | '\'' (_ as c) '\'' { CHAR c }
  | '\"' (_+ as str) '\"' { STRING str }
  | eof { EOF }
