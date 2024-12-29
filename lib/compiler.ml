type datum = string

type data = datum list

type inst =
  | LiteralInt of int
  | LiteralBool of bool
  | LoadRelative of int
  | UnaryNot
  | UnaryNeg
  | BinaryEqual
  | BinaryAdd
  | BinaryMul
  | BinaryAnd
  | BinaryOr
  | BinaryApply
  | JumpOnFalse of int (* move program counter by the int (do not autoincrement, so JumpOnFalse 1 is a no-op) *)

type text = inst list

module Env = struct
  (** [t] is the type of an environment. *)
  type t = (string * int) list

  (** [empty] is the empty environment. *)
  let empty = []

  (** [lookup env x] returns the index bound to [x] in [env]. *)
  let lookup env x =
    try List.assoc x env
    with Not_found -> failwith ("unbound variable " ^ x)

  (** [extend env x i] returns [env] with a binding from [x] to [i]. *)
  let extend env x i =
    (x, i) :: env
end

type program = data * text

let inst_length ((_, is) : program) = List.length is

(** [merge p1 p2] returns a new [program] with [p1] running first, then [p2]. *)
let merge (data1, text1 : program) (data2, text2 : program) : program =
  (* TODO: implement renaming datum indices *)
  data1 @ data2, text1 @ text2

let inst_of_unary_op = function
  | Ast.Not -> UnaryNot
  | Ast.Neg -> UnaryNeg

let inst_of_binary_op = function
  | Ast.Equal -> BinaryEqual
  | Ast.Add -> BinaryAdd
  | Ast.Mul -> BinaryMul
  | Ast.And -> BinaryAnd
  | Ast.Or -> BinaryOr

let rec compile (env : Env.t) (stack_index : int) (e : string Ast.expr) : program = match e with
  | Var x ->
    let i = Env.lookup env x in
    [], [LoadRelative (stack_index-i)]
  | Int v -> [], [LiteralInt v]
  | Bool v -> [], [LiteralBool v]
  | OpUnary (op, e1) ->
    let p1 = compile env stack_index e1 in
    let p2 = [], [inst_of_unary_op op] in
    merge p1 p2
  | OpBinary (op, e1, e2) ->
    let p1 = compile env stack_index e1 in
    let p2 = compile env (stack_index + 1) e2 in
    let p3 = [], [inst_of_binary_op op] in
    merge (merge p1 p2) p3
  | Closure _ -> failwith "TODO"
  | Application (e1, e2) ->
    let p1 = compile env stack_index e1 in
    let p2 = compile env (stack_index + 1) e2 in
    let p3 = [], [BinaryApply] in
    merge (merge p1 p2) p3
  | Let (x, e1, e2) ->
    let p1 = compile env stack_index e1 in
    let env' = Env.extend env x stack_index in
    let p2 = compile env' (stack_index + 1) e2 in
    merge p1 p2
  | If (e1, e2, e3) ->
    let p1 = compile env stack_index e1 in
    let p2 = compile env (stack_index + 1) e2 in
    let p3 = compile env (stack_index + 1) e3 in
    let pJump = [], [JumpOnFalse (inst_length p2 + 1)] in
    merge (merge (merge p1 pJump) p2) p3

let string_of_inst = function
  | LiteralInt i -> string_of_int i
  | LiteralBool b -> string_of_bool b
  | LoadRelative i -> "load_relative " ^ string_of_int i
  | UnaryNot -> "not"
  | UnaryNeg -> "neg"
  | BinaryEqual -> "equal"
  | BinaryAdd -> "add"
  | BinaryMul -> "mul"
  | BinaryAnd -> "and"
  | BinaryOr -> "or"
  | BinaryApply -> "apply"
  | JumpOnFalse i -> "jump_on_false " ^ string_of_int i

let string_of_program ((ds, is) : program) : string =
  let string_of_datum d = d in
  let string_of_data ds = String.concat " " (List.map string_of_datum ds) in
  let string_of_text is = String.concat "\n" (List.map string_of_inst is) in
  "data:\n" ^ string_of_data ds ^ "\ntext:\n" ^ string_of_text is
