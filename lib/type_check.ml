open Ast

(* use variable names: ct, ct1 *)
type tConType =
  | TClosure
  | TApplication
  (* TList etc... *)

(* use variable names: t, t1 *)
type typ =
  | TVar of string
  | TCon of tConType * typ list
  | TKind
  | TInt
  | TBool

(* use variable names: q, q1 *)
type typ_scheme = string list * typ

(* use variable names: s, s1 *)
type subst = (string * typ) list


let rec not_contains (a : string) (t : typ) : bool = match t with
  | TVar b when b = a -> false
  | TCon (_, ts) -> List.map (not_contains a) ts |> List.fold_left (&&) true
  | _ -> true

let remove_duplicates (type a) (l: a list) =
  let module S = Set.Make(struct type t = a let compare = compare end) in
  let rec remove acc seen_set = function
      | [] -> List.rev acc
      | a :: rest when S.mem a seen_set -> remove acc seen_set rest
      | a :: rest -> remove (a::acc) (S.add a seen_set) rest in
  remove [] S.empty l

let gensym : unit -> string =
  let c = ref 0 in
  fun _ -> incr c; Printf.sprintf "$%d" (!c)

let rec string_of_typ (t : typ) = match t with
  | TVar x -> x
  | TCon (TClosure, [t1; t2]) -> Printf.sprintf "(%s -> %s)" (string_of_typ t1) (string_of_typ t2)
  | TCon (TApplication, [t1; t2]) -> Printf.sprintf "(%s %s)" (string_of_typ t1) (string_of_typ t2)
  | TCon (TClosure, _) -> failwith "invalid closure type"
  | TCon (TApplication, _) -> failwith "invalid application type"
  | TKind -> "kind"
  | TInt -> "int"
  | TBool -> "bool"

let string_of_subst (s : subst) : string =
  let string_of_subst' (x, t) = x ^ " -> " ^ (string_of_typ t) in
  "{" ^ String.concat ", " (List.map string_of_subst' s) ^ "}"

(** [vars_in t] is the unique list of type variables in [t]. *)
let vars_in (t : typ) : string list =
  let rec vars_in' (acc : string list) (t : typ) : string list = match t with
    | TVar x -> x :: acc
    | TCon (_, ts) -> List.fold_left vars_in' acc ts
    | _ -> acc
  in
  let vars = vars_in' [] t in
  remove_duplicates vars

let rec apply_typ (s : subst) (t : typ) : typ = match t with
  | TVar x -> (try List.assoc x s with Not_found -> t)
  | TCon (c, ts) -> TCon (c, List.map (apply_typ s) ts)
  | _ -> t

(** [apply_typ (compose s1 s2) t] is equivalent to [apply_typ s2 (apply_typ s1 t)]. *)
let compose (s1 : subst) (s2 : subst) : subst =
  (* apply s1 to each type in s2, and then add in substitutions from s1 that s2 does not have *)
  let s1' = List.map (fun (x, t) -> (x, apply_typ s2 t)) s1 in
  let s2' = List.filter (fun (x, _) -> List.find_opt (fun (y, _) -> x = y) s1' = None) s2 in
  s1' @ s2'

let lower (t : typ) : typ =
  let vars = vars_in t in
  let c' = ref 0 in
  let gensym' = fun _ -> incr c'; Printf.sprintf "$%d" (!c') in
  let s = List.map (fun x -> (x, TVar (gensym' ()))) vars in
  apply_typ s t

let string_of_typ_scheme (ts : typ_scheme) =
  let (xs, t) = ts in
  let xs' = String.concat " " xs in
  Printf.sprintf "∀%s.%s" xs' (string_of_typ t)

module Env = struct
  (** [t] is the type of an environment. *)
  type t = (string * typ_scheme) list

  (** [empty] is the empty environment. *)
  let empty = []

  (** [lookup env x] returns the type scheme bound to [x] in [env]. *)
  let lookup env x =
    try List.assoc x env
    with Not_found -> failwith ("unbound variable " ^ x)

  (** [extend env x ty] returns [env] with a binding from [x] to [ty]. *)
  let extend env x ty =
    (x, ty) :: env
end

let apply_env (s : subst) (env : Env.t) : Env.t =
  List.map (fun (x, ts) -> (x, (fst ts, apply_typ s (snd ts)))) env

let instantiate (ts : typ_scheme) : typ =
  let s = List.map (fun x -> (x, TVar (gensym ()))) (fst ts) in
  apply_typ s (snd ts)

let generalize (env : Env.t) (t : typ) : typ_scheme =
  let vars_in_t = vars_in t in
  let vars_in_env = List.concat (List.map (fun (_, (xs, _)) -> xs) env) in
  let vars_not_in = List.filter (fun x -> not (List.mem x vars_in_env)) vars_in_t in
  (vars_not_in, t)

let rec unify (t1 : typ) (t2 : typ) : subst = match t1, t2 with
  | TKind, TKind -> []
  | TInt, TInt -> []
  | TBool, TBool -> []
  | TVar x, TVar y when x = y -> []
  | TVar x, t when not_contains x t -> [(x, t)]
  | t, TVar x when not_contains x t -> [(x, t)]
  | TCon (TClosure, [lhs1; rhs1]), TCon (TClosure, [lhs2; rhs2]) ->
      (* TODO: generalize for any [TCon] with same [tConType] and same number of children *)
      let s1 = unify lhs1 lhs2 in
      (* type variable substitution must happen simultaneously *)
      let s2 = unify (apply_typ s1 rhs1) (apply_typ s1 rhs2) in
      compose s1 s2
  | _ -> failwith "unification failed"

let rec infer (e : 'i expr) (env : Env.t) : typ * subst =
  let unary_expect s t = function
    | TVar x -> let s' = [(x, t)] in (t, compose s s')
    | t' when t' = t -> (t', s)
    | _ -> failwith ("must be " ^ string_of_typ t) in
  let binary_expect t e1 e2 env =
    let t1, s1 = infer e1 env in
    let t2, s2 = infer e2 env in
    let s3 = compose s1 s2 in
    match t1, t2 with
    | TVar x, TVar y when x = y -> let s' = [(x, t)] in (t, compose s3 s')
    | TVar x, TVar y -> let s' = [(x, t); (y, t)] in (t, compose s3 s')
    | TVar x, t2' when t2' = t -> let s' = [(x, t)] in (t, compose s3 s')
    | t1', TVar y when t1' = t -> let s' = [(y, t)] in (t, compose s3 s')
    | t1', t2' when t1' = t2' && t2' = t -> (t, s3)
    | _ -> failwith ("both sides must be " ^ string_of_typ t ^ " but lhs is " ^ string_of_typ t1 ^ " and rhs is " ^ string_of_typ t2)
    in
  match e with
  | Var x ->
      let t = Env.lookup env x in
      let res = instantiate t in
      (res, [])
  | Int _ -> (TInt, [])
  | Bool _ -> (TBool, [])
  | OpUnary (op, e) ->
      let t, s = infer e env in
      (match op with
      | Not -> unary_expect s TBool t
      | Neg -> unary_expect s TInt t)
  | OpBinary (op, e1, e2) ->
      (match op with
      | Add -> binary_expect TInt e1 e2 env
      | Mul -> binary_expect TInt e1 e2 env
      | And -> binary_expect TBool e1 e2 env
      | Or  -> binary_expect TBool e1 e2 env)
  | Closure (x, e) ->
      let a = TVar (gensym ()) in
      let env' = Env.extend env x ([], a) in
      let (t, s) = infer e env' in
      (TCon (TClosure, [a; t]), s)
  | Application (e1, e2) ->
      let a = TVar (gensym ()) in
      let (t1, s1) = infer e1 env in (* left hand side *)
      let env1 = apply_env s1 env in (* LHS side might have useful info (e.g. the func only takes in int, or `(plus1 x) (id x)' ) *)
      let (t2, s2) = infer e2 env1 in (* right hand side *)
      let t1' = apply_typ s2 t1 in (* similarly, RHS might have something useful about the LHS *)
      let t3 = TCon (TClosure, [t2; a]) in
      let s3 = unify t1' t3 in
      let s4 = compose s3 (compose s2 s1) in
      (apply_typ s4 a, s4)
  | Let (x, e1, e2) ->
      let (t1, s1) = infer e1 env in
      let ts = generalize env (apply_typ s1 t1) in
      let env' = Env.extend (apply_env s1 env) x ts in
      infer e2 env'
  | If _ -> failwith "TODO3"
