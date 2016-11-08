{
open Prsr

(* string buffer *)
let buf = Buffer.create 100
}

let white = [' ' '\t' '\r' '\n']+

rule read =
  parse
  | white { read lexbuf }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '*' { STAR }
  | '\'' "\\'" '\'' { CHAR '\'' }
  | '\'' (_ as c) '\'' { CHAR c }
  | '\"' { Buffer.clear buf; string lexbuf; STRING (Buffer.contents buf) }
  | eof { EOF }

(* custom entrypoint to allow string literals to contain "" characters *)
and string =
  parse
  | [^ '"']+ as str { Buffer.add_bytes buf str; string lexbuf }
  | '\\' '\"' { Buffer.add_char buf '"'; string lexbuf }
  | '"' { () }