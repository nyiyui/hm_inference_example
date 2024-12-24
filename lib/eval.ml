open Ast

(** Whether the expr is a concrete value. *)
let is_value : 'a expr -> bool = function
  | Int _ | Bool _ -> true
  | _ -> false

  (** [subst e v x] is [e] with the value [v] substituted in for the name/variable [x] *)
let rec subst e v x = match e with
  | Int _ | Bool _ -> e
  | Var y -> if y = x then v else e
  | OpUnary (uop, e1) -> OpUnary (uop, subst e1 v x)
  | OpBinary (bop, e1, e2) -> OpBinary (bop, subst e1 v x, subst e2 v x)
  | If (y, e1, e2) -> If (subst y v x, subst e1 v x, subst e2 v x)
  | Let (y, e1, e2) ->
      let e1' = subst e1 v x in
      if y = x then Let (y, e1', e2) else Let (y, e1', subst e2 v x)

let rec step : 'a expr -> 'a expr = function
  | Int _ | Bool _ -> failwith "no step"
  | Var _ -> failwith "unbound variable"
  | OpUnary (uop, e1) when is_value e1 -> step_uop uop e1
  | OpUnary (uop, e1) -> OpUnary(uop, step e1)
  | OpBinary (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | OpBinary (bop, e1, e2) when is_value e1 -> OpBinary(bop, e1, step e2)
  | OpBinary (bop, e1, e2) -> OpBinary(bop, step e1, e2)
  | Let (x, e1, e2) when is_value e1 -> subst e2 e1 x
  | Let (x, e1, e2) -> Let (x, step e1, e2)
  | If (Bool true, e2, _) -> e2
  | If (Bool false, _, e3) -> e3
  | If (Int _, _, _) -> failwith "guard must be bool"
  | If (e1, e2, e3) -> If (step e1, e2, e3)

and step_uop uop e1 = match uop, e1 with
  | Not, Bool true -> Bool false
  | Not, Bool false -> Bool true
  | Neg, Int a -> Int (-a)
  | _ -> failwith "unary op with given operand not defined"

and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mul, Int a, Int b -> Int (a * b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _ -> failwith "binary op with given operands not defined"

(* step and friends above implement a single step, eval and friends below implement zero or multiple steps *)

let rec eval (e : 'a expr) : 'a expr = match e with
  | Int _ | Bool _ -> e
  | Var _ -> failwith "unbound variable"
  | OpUnary(uop, e1) -> eval_uop uop e1
  | OpBinary(bop, e1, e2) -> eval_bop bop e1 e2
  | Let(x, e1, e2) -> eval (subst e2 (eval e1) x)
  | If(e1, e2, e3) -> eval_if e1 e2 e3

and eval_uop uop e1 = match uop, eval e1 with
  | Not, Bool true -> Bool false
  | Not, Bool false -> Bool true
  | Neg, Int a -> Int (-a)
  | _ -> failwith "unary op with given operand not defined"

and eval_bop bop e1 e2 = match bop, eval e1, eval e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mul, Int a, Int b -> Int (a * b)
  | And, Bool a, Bool b -> Bool (a && b)
  | Or, Bool a, Bool b -> Bool (a || b)
  | _ -> failwith "binary op with given operands not defined"

and eval_if e1 e2 e3 = match eval e1 with
  | Bool true -> eval e2
  | Bool false -> eval e3
  | _ -> failwith "guard must be bool"
