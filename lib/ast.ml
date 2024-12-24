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
  | Let of 'i * 'i expr * 'i expr
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
    | Let (x, e1, e2) -> "let " ^ x ^ " = " ^ string_of_expr' e1 ^ " in " ^ string_of_expr' e2
    | If (e1, e2, e3) -> "if " ^ string_of_expr' e1 ^ " then " ^ string_of_expr' e2 ^ " else " ^ string_of_expr' e3
  in
  string_of_expr' e
