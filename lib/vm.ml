open Ast

type inst = Compiler.inst
type program = Compiler.program

type 'i stack = 'i expr list
type 'i local = 'i expr list

let step (data, text : program) (s : 'i stack) (l : 'i local) : program * 'i stack * 'i local =
  let store_at l i v =
    let rec extend l len =
      if List.length l < len then extend (l @ [If (Bool false, Bool false, Bool false)]) len else l in
    let l' = extend l (i + 1) in
    List.mapi (fun j x -> if j == i then v else x) l' in
  let binary_int p' s f l = match s with
    | Int(x)::Int(y)::rest ->
      let s' = [Int(f x y)] @ rest in
      let () = print_endline (string_of_int x ^ " and " ^ string_of_int y ^ " --> " ^ string_of_int (f x y)) in
      (p', s', l)
    | _ -> failwith ("invalid stack " ^ (List.hd s |> string_of_expr) ^ " and " ^ (List.tl s |> List.hd |> string_of_expr)) in
  let p' = (data, List.tl text) in
  let current = List.hd text in
  match current with
  | LiteralInt v -> (p', [Int v] @ s, l)
  | LiteralBool v -> (p', [Bool v] @ s, l)
  | LocalLoad i ->
    let () = print_endline ("from " ^ string_of_int i ^ " loaded " ^ string_of_expr (List.nth l i)) in
    (p', [List.nth l i] @ s, l)
  | LocalStore i -> (p', List.tl s, store_at l i (List.hd s))
  | BinaryAdd -> binary_int p' s ( + ) l
  | BinaryMul -> binary_int p' s ( * ) l
  | _ -> failwith ("TODO step with " ^ Compiler.string_of_inst current)

let run (p : program) (s : 'i stack) (l : 'i local) : 'i stack * 'i local =
  let rec step' (p : program) (s : 'i stack) (l : 'i local) : program * 'i stack * 'i local =
    let (data', text'), s', l' = step p s l in
(*     let () = print_endline ("s' = " ^ (List.map string_of_expr s' |> String.concat " ")) in *)
    if List.is_empty text' then ([], []), s', l' else step' (data', text') s' l' in
  let _, s', l' = step' p s l in
  s', l'
