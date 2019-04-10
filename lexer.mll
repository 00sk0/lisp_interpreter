{
  open Lexing
  open Parse
  exception SyntaxError of string
}

let number = '-'? ['0'-'9']+
let digit  = ['0'-'9']
let string = '\"' ([^ '\\' '\"'] | '\\'_ )* '\"'
let var = ['a'-'z' '_' '=' '+' '*' '/' '<' '>' '!' '?' '-']['a'-'z' '0'-'9' '_' '=' '+' '*' '/' '<' '>' '!' '?' '-']*
let white  = (' ' | '\n' | '\t')*

rule read = parse
| white       {read lexbuf}
| number as n {NUMBER (int_of_string n)}
| '('         {LEFT_PAREN}
| ')'         {RIGHT_PAREN}
| var as s    {VARIABLE s}
| string as s {STRING (String.sub s 1 (String.length s - 2))}
| _           {raise @@ SyntaxError ("unexpected: " ^ Lexing.lexeme lexbuf)}
| eof         {EOF}

