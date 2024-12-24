open Small_functional_language

let test_eval =
  let lexbuf = Lexing.from_string "0 * 1 + 2 * 4" in
  let ast = Parser.program Lexer.read lexbuf in
  let result = Eval.eval ast in
  if result = Ast.Int 8 then
    print_endline "test_eval OK"
  else
    failwith ("failed - got " ^ (Ast.string_of_expr result))

let () = test_eval
