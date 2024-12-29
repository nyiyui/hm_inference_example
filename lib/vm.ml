open Ast

type inst = Compiler.inst
type program = Compiler.program

type 'i stack = 'i expr list

let step (data, text : program) (s : 'i stack) : program * 'i stack =
  let binary_int p' s f = match s with
    | Int(x)::Int(y)::rest ->
      let s' = [Int(f x y)] @ rest in
      let () = print_endline (string_of_int x ^ " and " ^ string_of_int y ^ " --> " ^ string_of_int (f x y)) in
      (p', s')
    | _ -> failwith "invalid stack" in
  let p' = (data, List.tl text) in
  let current = List.hd text in
  match current with
  | LiteralInt (v) -> (p', [Int v] @ s)
  | LiteralBool (v) -> (p', [Bool v] @ s)
  | BinaryAdd -> binary_int p' s ( + )
  | BinaryMul -> binary_int p' s ( * )
  | _ -> failwith ("TODO step with " ^ Compiler.string_of_inst current)

let run (p : program) (s : 'i stack) : 'i stack =
  let rec step' (p : program) (s : 'i stack) : program * 'i stack =
    let (data', text'), s' = step p s in
(*     let () = print_endline ("s' = " ^ (List.map string_of_expr s' |> String.concat " ")) in *)
    if List.is_empty text' then ([], []), s' else step' (data', text') s' in
  let _, s' = step' p s in
  s'
