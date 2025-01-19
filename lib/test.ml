open Hm_inference_example
open Type_check

let test_eval src expected =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.read lexbuf in
  let result = Eval.eval ast in
  if result = expected then print_endline ("test_eval OK " ^ src)
  else failwith ("failed - got " ^ Ast.string_of_expr result)

let test_infer src expected =
  let lexbuf = Lexing.from_string src in
  let ast = Parser.program Lexer.read lexbuf in
  let result = infer ast Env.empty |> fst in
  let result = lower result in
  if result = expected then
    print_endline ("test_infer OK " ^ string_of_typ result)
  else failwith ("failed - got " ^ string_of_typ result)

let test_unify (t1 : typ) (t2 : typ) (expected : subst) =
  let result = unify t1 t2 in
  if result = expected then
    print_endline ("test_unify OK " ^ string_of_typ t1 ^ " " ^ string_of_typ t2)
  else
    failwith
      ("failed - got " ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ " ==> "
     ^ string_of_subst result)

let test_unify_property (t1 : typ) (t2 : typ) =
  let s = unify t1 t2 in
  let t1' = apply_typ s t1 in
  let t2' = apply_typ s t2 in
  if t1' = t2' then
    print_endline
      ("test_unify_property OK " ^ string_of_typ t1 ^ " " ^ string_of_typ t2)
  else
    failwith
      ("failed - got " ^ string_of_typ t1 ^ " " ^ string_of_typ t2 ^ " ==> "
     ^ string_of_subst s ^ " so forms " ^ string_of_typ t1' ^ " and "
     ^ string_of_typ t2')

let test_compose_property (t1 : typ) (s1 : subst) (s2 : subst) : unit =
  let s3 = compose s1 s2 in
  let t2 = apply_typ s2 (apply_typ s1 t1) in
  let t3 = apply_typ s3 t1 in
  if t2 = t3 then print_endline "test_compose_property OK"
  else
    failwith
      ("test_compose_property failed\n- t1 = " ^ string_of_typ t1 ^ " ; t2 = "
     ^ string_of_typ t2 ^ " ; t3 = " ^ string_of_typ t3 ^ "\n; s1 = "
     ^ string_of_subst s1 ^ " ; s2 = " ^ string_of_subst s2 ^ " ; s3 = "
     ^ string_of_subst s3)

(* test eval function *)
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

let () =
  test_eval
    "let add1 = x -> (x + 1) in let compose-twice = (f -> x -> f (f x)) in \
     compose-twice add1 1"
    (Int 3)

(* test compose function *)

let () =
  test_compose_property
    (TCon (TClosure, [ TVar "$1"; TVar "$2" ]))
    [ ("$2", TCon (TClosure, [ TVar "$3"; TInt ])) ]
    [ ("$1", TBool); ("$3", TInt) ]

(* test unify function (uses compose) *)

let () = test_unify TBool TBool []
let () = test_unify TInt TInt []

let () =
  test_unify_property
    (TCon (TClosure, [ TInt; TVar "$1" ]))
    (TCon (TClosure, [ TInt; TBool ]))

let () =
  test_unify_property
    (TCon (TClosure, [ TInt; TVar "$1" ]))
    (TCon (TClosure, [ TVar "$2"; TBool ]))

let () =
  test_unify_property
    (TCon (TClosure, [ TVar "$1"; TVar "$1" ]))
    (TCon (TClosure, [ TInt; TInt ]))

let () =
  test_unify_property
    (TCon (TClosure, [ TVar "$1"; TVar "$1" ]))
    (TCon (TClosure, [ TInt; TVar "$2" ]))

(* test infer function (uses unify) *)

let () = test_infer "let id = x -> x in id 1" TInt
let () = test_infer "x -> x" (TCon (TClosure, [ TVar "$1"; TVar "$1" ]))

let () =
  test_infer "let id = x -> x in id" (TCon (TClosure, [ TVar "$1"; TVar "$1" ]))

let () =
  test_infer "let id = x -> x in id id"
    (TCon (TClosure, [ TVar "$1"; TVar "$1" ]))

let () =
  test_infer "let id = x -> x in (id id) (id id)"
    (TCon (TClosure, [ TVar "$1"; TVar "$1" ]))

let () = test_infer "let f = x -> x = 1 in f" (TCon (TClosure, [ TInt; TBool ]))

let () =
  test_infer "let f = x -> x = true in f" (TCon (TClosure, [ TBool; TBool ]))

let () =
  test_infer "let id = x -> x in let f = x -> x = id in f"
    (TCon (TClosure, [ TCon (TClosure, [ TVar "$1"; TVar "$1" ]); TBool ]))

let () = test_infer "true" TBool
let () = test_infer "!true" TBool
let () = test_infer "1" TInt
let () = test_infer "let x = 1 in x" TInt
let () = test_infer "let x = 1 in -x" TInt

let () =
  test_infer "let negate = x -> -x in negate" (TCon (TClosure, [ TInt; TInt ]))

let () =
  test_infer "let id = x -> 1 * x in id" (TCon (TClosure, [ TInt; TInt ]))

let () =
  test_infer "let id = x -> 0 + x in id" (TCon (TClosure, [ TInt; TInt ]))

let () =
  test_infer "let flip = x -> !x in flip" (TCon (TClosure, [ TBool; TBool ]))

let () =
  test_infer "let id = x -> true && x in id" (TCon (TClosure, [ TBool; TBool ]))

let () =
  test_infer "let id = x -> false || x in id"
    (TCon (TClosure, [ TBool; TBool ]))

let () = test_infer "true && false" TBool
let () = test_infer "true || false" TBool
