open Ast

type inst = Compiler.inst
type program = Compiler.program

type 'i value = 'i Compiler.value
let string_of_value = Compiler.string_of_value

type 'i stack = 'i value list
type 'i local = 'i value list

let rec step (data, text : program) (s : 'i stack) (l : 'i local) : program * 'i stack * 'i local =
  let rec take_first n l = match n with
    | 0 -> ([], l)
    | n -> (match l with
      | x::xs ->
        let a, b = take_first (n - 1) xs in
        ([x] @ a, b)
      | [] -> failwith "not enough"
    )in
  let store_at l i v =
    let rec extend l len =
      if List.length l < len then extend (l @ [Compiler.Expr (If (Bool false, Bool false, Bool false))]) len else l in
    let l' = extend l (i + 1) in
    List.mapi (fun j x -> if j == i then v else x) l' in
  let binary_int p' s f l = match s with
    | (Compiler.Expr Int(x))::(Compiler.Expr Int(y))::rest ->
      let s' = [Compiler.Expr(Int(f x y))] @ rest in
      let () = print_endline (string_of_int x ^ " and " ^ string_of_int y ^ " --> " ^ string_of_int (f x y)) in
      (p', s', l)
    | _ -> failwith ("invalid stack " ^ (List.hd s |> string_of_value) ^ " and " ^ (List.tl s |> List.hd |> string_of_value)) in
  let p' = (data, List.tl text) in
  let current = List.hd text in
  match current with
  | LiteralInt v -> (p', [Compiler.Expr (Int v)] @ s, l)
  | LiteralBool v -> (p', [Compiler.Expr (Bool v)] @ s, l)
  | LocalLoad i ->
(*     let () = print_endline ("from " ^ string_of_int i ^ " loaded " ^ string_of_value (List.nth l i)) in *)
    (p', [List.nth l i] @ s, l)
  | LocalStore i -> (p', List.tl s, store_at l i (List.hd s))
  | ClosureLoad (i, n_captures) -> (match List.nth data i with
    | Closure (_, p2) ->
      let vars, s' = take_first n_captures s in
      (p', [Compiler.Closure (vars, p2)] @ s', l)
    | _ -> failwith "invalid data type"
  )
  | BinaryAdd -> binary_int p' s ( + ) l
  | BinaryMul -> binary_int p' s ( * ) l
  | BinaryApply -> (match s with
    | arg::(Compiler.Closure (vars, p2))::_ ->
      let fn_stack = arg::vars in
      let () = print_endline ("fn     is " ^ Compiler.string_of_program p2) in
      let () = print_endline ("fn_s   is " ^ (List.map string_of_value fn_stack |> String.concat " ")) in
      let () = print_endline ("arg    is " ^ string_of_value arg) in
      let s2, _ = run p2 [] fn_stack in
      let retval = List.hd s2 in
      let () = print_endline ("retval is " ^ string_of_value retval) in
      (p', [retval] @ s, l)
    | _ -> failwith ("invalid stack " ^ (List.hd s |> string_of_value) ^ " and " ^ (List.tl s |> List.hd |> string_of_value))
  )
  | _ -> failwith ("TODO step with " ^ Compiler.string_of_inst current)

and run (p : program) (s : 'i stack) (l : 'i local) : 'i stack * 'i local =
  let rec step' (p : program) (s : 'i stack) (l : 'i local) : program * 'i stack * 'i local =
    let (data', text'), s', l' = step p s l in
    if List.is_empty text' then ([], []), s', l' else step' (data', text') s' l' in
  let _, s', l' = step' p s l in
  s', l'
