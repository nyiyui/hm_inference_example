type inst =
  | LiteralInt of int
  | LiteralBool of bool
  | LocalLoad of int
  | LocalStore of int
  | ClosureLoad of int * int (* [ClosureLoad i n] instantiates a new closure from data index [i] and with [n] captures from the stack *)
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

type 'i value =
  | Expr of 'i Ast.expr
  | Closure of ('i value list) * text
  (* [Closure (captures, t)] is a closure that runs [t]. [captures] msut be an empty list during compilation, before runtime. *)

type data = string value list

type program = data * text

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

  let string_of_env env = (List.map (fun (x, i) -> x ^ ":" ^ (string_of_int i)) env |> String.concat " ")
end

let inst_length ((_, is) : program) = List.length is

let offset_datum_indices (t : text) i =
  let offset i s = match s with
    | ClosureLoad (j, n) -> ClosureLoad (j + i, n)
    | _ -> s in
  if i = 0 then t else List.map (offset i) t

let offset_data_indices (d : data) i =
  let offset i s = match s with
    | Closure (captures, t) ->
      if captures = []
      then Closure (captures, offset_datum_indices t i)
      else failwith "captures must be empty list"
    | _ -> s in
  if i = 0 then d else List.map (offset i) d

(** [merge p1 p2] returns a new [program] with [p1] running first, then [p2]. *)
let merge (data1, text1 : program) (data2, text2 : program) : program =
  let l1 = List.length data1 in
  let text2' = offset_datum_indices text2 l1 in
  let data2' = offset_data_indices data2 l1 in
  data1 @ data2', text1 @ text2'

let inst_of_unary_op = function
  | Ast.Not -> UnaryNot
  | Ast.Neg -> UnaryNeg

let inst_of_binary_op = function
  | Ast.Equal -> BinaryEqual
  | Ast.Add -> BinaryAdd
  | Ast.Mul -> BinaryMul
  | Ast.And -> BinaryAnd
  | Ast.Or -> BinaryOr

let rec vars_in (e : string Ast.expr) : string list = match e with
  | Var x -> [x]
  | Int _ | Bool _ -> []
  | OpUnary (_, e1) -> vars_in e1
  | OpBinary (_, e1, e2) -> (vars_in e1) @ (vars_in e2)
  | Closure (x, e) -> List.filter (fun y -> not (y = x)) (vars_in e)
  | Application (e1, e2) -> (vars_in e1) @ (vars_in e2)
  | Let (x, e1, e2) -> (vars_in e1) @ (vars_in e2 |> (List.filter (fun y -> not (y = x))))
  | If (e1, e2, e3) -> (vars_in e1) @ (vars_in e2) @ (vars_in e3)

let rec compile (env : Env.t) (local_from : int) (e : string Ast.expr) : program = match e with
  | Var x ->
    let () = print_endline ("lookup " ^ x ^ " becomes " ^ string_of_int (Env.lookup env x)) in
    [], [LocalLoad (Env.lookup env x)]
  | Int v -> [], [LiteralInt v]
  | Bool v -> [], [LiteralBool v]
  | OpUnary (op, e1) ->
    let p1 = compile env local_from e1 in
    let p2 = [], [inst_of_unary_op op] in
    merge p1 p2
  | OpBinary (op, e1, e2) ->
    let p1 = compile env local_from e1 in
    let p2 = compile env local_from e2 in
    let p3 = [], [inst_of_binary_op op] in
    merge (merge p1 p2) p3
  | Closure (x, e) ->
    let captures = vars_in e |> List.filter (fun y -> not (y = x)) in
    let () = print_endline ("x = " ^ x) in
    let () = print_endline ("e = " ^ (Ast.string_of_expr e)) in
    let () = print_endline ("captures: " ^ String.concat " " captures) in
    let capture_text = List.map (fun y -> LocalLoad (Env.lookup env y)) captures in
    let add_indices = List.mapi (fun i y -> (y, i + 1)) in
    let env2 = List.fold_left (fun env (y, i) -> Env.extend env y i) (Env.extend [] x 0) (add_indices captures) in
    let () = print_endline ("env2 = " ^ (Env.string_of_env env2)) in
    (* indices are: 0 = argument, 1... = captures, then locals from e.g. let *)
    let (data2, text2) = compile env2 1 e in (* closure is given a new stack and local, local at index 0 is argument *)
    data2 @ [Closure ([], text2)], capture_text @ [ClosureLoad (List.length data2, List.length captures)]
  | Application (e1, e2) ->
    let p1 = compile env local_from e1 in
    let p2 = compile env local_from e2 in
    let p3 = [], [BinaryApply] in
    merge (merge p1 p2) p3
  | Let (x, e1, e2) ->
    let p1 = merge (compile env local_from e1) ([], [LocalStore local_from]) in
    let env' = Env.extend env x local_from in
    let p2 = compile env' (local_from + 1) e2 in
    merge p1 p2
  | If (e1, e2, e3) ->
    let p1 = compile env local_from e1 in
    let p2 = compile env local_from e2 in
    let p3 = compile env local_from e3 in
    let pJump = [], [JumpOnFalse (inst_length p2 + 1)] in
    merge (merge (merge p1 pJump) p2) p3

let string_of_inst = function
  | LiteralInt i -> string_of_int i
  | LiteralBool b -> string_of_bool b
  | LocalLoad i -> "local_load-" ^ string_of_int i
  | LocalStore i -> "local_store-" ^ string_of_int i
  | ClosureLoad (i, n) -> "closure_load-" ^ string_of_int i ^ "-" ^ string_of_int n
  | UnaryNot -> "not"
  | UnaryNeg -> "neg"
  | BinaryEqual -> "equal"
  | BinaryAdd -> "add"
  | BinaryMul -> "mul"
  | BinaryAnd -> "and"
  | BinaryOr -> "or"
  | BinaryApply -> "apply"
  | JumpOnFalse i -> "jump_on_false-" ^ string_of_int i

let rec string_of_program ((ds, is) : program) : string =
  let string_of_data ds = String.concat " " (List.map string_of_value ds) in
  "{{data: " ^ string_of_data ds ^ " }}*{{text: " ^ string_of_text is ^ " }}"
and string_of_text is = String.concat " " (List.map string_of_inst is)
and string_of_value = function
  | Expr e -> Ast.string_of_expr e
  | Closure (vars, c) -> "[" ^ (String.concat " " (List.map string_of_value vars)) ^ "]/{" ^ string_of_text c ^ "}"
