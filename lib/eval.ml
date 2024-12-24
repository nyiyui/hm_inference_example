open Ast

(** [subst e v x] is [e] with the value [v] substituted in for the name/variable [x] *)
let rec subst e v x = match e with
  | Int _ | Bool _ -> e
  | Var y -> if y = x then v else e
  | OpUnary (uop, e1) -> OpUnary (uop, subst e1 v x)
  | OpBinary (bop, e1, e2) -> OpBinary (bop, subst e1 v x, subst e2 v x)
  | If (y, e1, e2) -> If (subst y v x, subst e1 v x, subst e2 v x)
  | Closure (y, e1) -> if y = x then e else Closure (y, subst e1 v x)
  | Application (e1, e2) -> Application (subst e1 v x, subst e2 v x)

let rec eval (e : 'a expr) : 'a expr = match e with
  | Int _ | Bool _ | Closure _ -> e
  | Var x -> failwith ("unbound variable " ^ x ^ " while evaluating " ^ string_of_expr e)
  | OpUnary (uop, e1) -> eval_uop uop e1
  | OpBinary (bop, e1, e2) -> eval_bop bop e1 e2
  | Application (e1, e2) -> (match eval e1 with
      | Closure (x, e) -> eval (subst e e2 x)
      | _ -> failwith "application of non-closure")
  | If (e1, e2, e3) -> eval_if e1 e2 e3

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
