{
  open Parser
}
let space = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let id = letter (letter | digit | '_')*
rule read =
  parse
  | space { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "not" { NOT }
  | "+" { PLUS }
  | "*" { TIMES }
  | "=" { EQUALS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id { ID (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
