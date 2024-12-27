open Small_functional_language

let test_eval src expected =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.read lexbuf in
  let result = Eval.eval ast in
  if result = expected then
    print_endline "test_eval OK"
  else
    failwith ("failed - got " ^ (Ast.string_of_expr result))

let test_infer src expected =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.read lexbuf in
  let () = print_endline ("infer " ^ (Ast.string_of_expr ast)) in
  let result = Type_check.infer ast Type_check.Env.empty |> fst in
  let result = Type_check.lower result in
  if result = expected then
    print_endline ("test_infer OK " ^ (Type_check.string_of_typ result))
  else
    failwith ("failed - got " ^ (Type_check.string_of_typ result))

let test_unify (t1 : Type_check.typ) (t2 : Type_check.typ) (expected : Type_check.subst) =
  let result = Type_check.unify t1 t2 in
  if result = expected then
    print_endline ("test_unify OK " ^ (Type_check.string_of_typ t1) ^ " " ^ (Type_check.string_of_typ t2))
  else
    failwith ("failed - got " ^ (Type_check.string_of_typ t1) ^ " " ^ (Type_check.string_of_typ t2) ^ " ==> " ^ (Type_check.string_of_subst result))

let () = test_eval "1" (Int 1)
let () = test_eval "0 * 1 + 2 * 4" (Int 8)
let () = test_eval "let x = 1 in x" (Int 1)
let () = test_eval "let x = 1 in x + 2" (Int 3)
let () = test_eval "let x = 1 in x + (let y = 2 in y)" (Int 3)
let () = test_eval "let x = 1 in x + (let x = 2 in x)" (Int 3)
let () = test_eval "let x = 1 in x + (let y = 2 in y + x)" (Int 4)
(* let x = 1 in x + let y = 2 in (y + x) *)
(* let x = 1 in x + 2 + x *)
(* 1 + 2 + 1 *)
(* 4 *)

let () = test_eval "x -> x" (Closure ("x", Var "x"))
let () = test_eval "let first = x -> y -> x in first 1 2" (Int 1)
 let () = test_eval "let second = x -> y -> y in second 1 2" (Int 2)
let () = test_eval "(x -> x) 1" (Int 1)
let () = test_eval "(x -> x) (y -> y) 1" (Int 1)
let () = test_eval "(x -> x) (x -> x) 1" (Int 1)
let () = test_eval "let identity = x -> x in identity 1" (Int 1)
let () = test_eval "let identity = x -> x in let x = 2 in identity x" (Int 2)
let () = test_eval "let f = x -> y -> x + y in f 1 2" (Int 3)
let () = test_eval "let add1 = x -> (x + 1) in let compose-twice = (f -> x -> f (f x)) in compose-twice add1 1" (Int 3)

(*
let () = test_infer "1" Type_check.TInt
let () = test_infer "x -> x" (Type_check.TCon (Type_check.TClosure, [Type_check.TVar("$1"); Type_check.TVar("$1")]))
let () = test_infer "let id = x -> x in id" (Type_check.TCon (Type_check.TClosure, [Type_check.TVar("$1"); Type_check.TVar("$1")]))
*)

let () = test_unify Type_check.TInt Type_check.TInt []
let () = test_unify
  (Type_check.TCon (Type_check.TClosure, [Type_check.TVar "$1"; Type_check.TVar "$1"]))
  (Type_check.TCon (Type_check.TClosure, [Type_check.TVar "$2"; Type_check.TInt]))
  [("$1", Type_check.TInt)]

let () = test_infer "let id = x -> x in id 1" Type_check.TInt
