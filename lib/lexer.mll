{
  open Parser
}
let space = [' ' '\t' '\n' '\r']+
let digit = ['0'-'9']
let int = '-'? digit+
let upper = ['A'-'Z']
let lower = ['a'-'z']
let letter = ['a'-'z' 'A'-'Z']
let id_type = upper (letter | digit | '_' | '-' | '\'' )*
let id_value = lower (letter | digit | '_' | '-' | '\'' )*
rule read =
  parse
  | space { read lexbuf }
  | "true" { TRUE }
  | "false" { FALSE }
  | "!" { NOT }
  | "&&" { AND }
  | "||" { OR }
  | "-" { MINUS }
  | "+" { PLUS }
  | "*" { TIMES }
  | "=" { EQUALS }
  | "(" { LPAREN }
  | ")" { RPAREN }
  | "->" { RARROW }
  | "let" { LET }
  | "in" { IN }
  | "if" { IF }
  | "then" { THEN }
  | "else" { ELSE }
  | id_type { ID_TYPE (Lexing.lexeme lexbuf) }
  | id_value { ID_VALUE (Lexing.lexeme lexbuf) }
  | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | eof { EOF }
