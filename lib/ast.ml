type unaryOp =
  | Not
  | Neg

type binaryOp =
  | Add
  | Mul
  | And
  | Or

type 'i expr =
  | Var of 'i
  | Int of int
  | Bool of bool
  | OpUnary of unaryOp * 'i expr
  | OpBinary of binaryOp * 'i expr * 'i expr
  | Closure of 'i * 'i expr
  | Application of 'i expr * 'i expr (* actually a binary operation, but make it a separate constructor since it has special behaviour *)
  | If of 'i expr * 'i expr * 'i expr

let string_of_expr (e : string expr) : string =
  let rec string_of_expr' (e : string expr) : string =
    match e with
    | Var x -> x
    | Int n -> string_of_int n
    | Bool b -> string_of_bool b
    | OpUnary (op, e) -> (match op with
        | Not -> "not " ^ string_of_expr' e
        | Neg -> "-" ^ string_of_expr' e)
    | OpBinary (op, e1, e2) -> (match op with
        | Add -> string_of_expr' e1 ^ " + " ^ string_of_expr' e2
        | Mul -> string_of_expr' e1 ^ " * " ^ string_of_expr' e2
        | And -> string_of_expr' e1 ^ " && " ^ string_of_expr' e2
        | Or -> string_of_expr' e1 ^ " || " ^ string_of_expr' e2)
    | Closure (x, e) -> x ^ " -> " ^ string_of_expr' e
    | Application (e1, e2) -> "(" ^ string_of_expr' e1 ^ ") (" ^ string_of_expr' e2 ^ ")"
    | If (e1, e2, e3) -> "if " ^ string_of_expr' e1 ^ " then " ^ string_of_expr' e2 ^ " else " ^ string_of_expr' e3
  in
  string_of_expr' e
