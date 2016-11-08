%{
open Regex
%}

%token LPAREN RPAREN STAR EOF
%token <char> CHAR
%token <string> STRING
%start parse
%type <Regex.t> parse

%%

parse:
  | r = regex; EOF { r }
  | r = regex; p = parse { Concat (r, p) }
  ;

regex:
  | LPAREN; r = regex; RPAREN { r }
  | c = CHAR { Char c }
  | s = STRING { String s }
  | r = regex; STAR { Star r }
  ;
