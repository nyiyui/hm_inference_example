open Small_functional_language

let test_eval src expected =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.read lexbuf in
  let result = Eval.eval ast in
  if result = expected then
    print_endline "test_eval OK"
  else
    failwith ("failed - got " ^ (Ast.string_of_expr result))

let () = test_eval "1" (Int 1)
let () = test_eval "0 * 1 + 2 * 4" (Int 8)
let () = test_eval "let x = 1 in x" (Int 1)
let () = test_eval "let x = 1 in x + 2" (Int 3)
let () = test_eval "let x = 1 in x + let y = 2 in y" (Int 3)
let () = test_eval "let x = 1 in x + let x = 2 in x" (Int 3)
let () = test_eval "let x = 1 in x + let y = 2 in (y + x)" (Int 4)
(* let x = 1 in x + let y = 2 in (y + x) *)
(* let x = 1 in x + 2 + x *)
(* 1 + 2 + 1 *)
(* 4 *)
