open Small_functional_language
open Ast

let parse (s : string) : string expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.program Lexer.read lexbuf in
  ast

let () = print_endline (string_of_expr (parse "if 1 + 2 then abc else def"))
