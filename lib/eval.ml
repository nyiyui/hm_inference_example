open Ast

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
