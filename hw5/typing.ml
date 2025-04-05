open Tml

exception TypeError

(***************************************************** 
 * replace unit by your own type for typing contexts *
 *****************************************************)
type context = var -> tp option

(*
 * For each function you introduce, 
 * write its type, specification, and invariant. 
 *)

(*
 * val createEmptyContext : unit -> context
 * Create empty context.
 *)
let createEmptyContext () = fun _ -> None

(* 
 * val extend : context -> Tml.var -> Tml.tp -> context 
 * Add a new type binding to original context.
 *)
let extend (cxt : context) (x : var) (tp : tp) : context = fun y -> if y = x then Some tp else cxt y

(* val typing : context -> Tml.exp -> Tml.tp *)
let rec typing (cxt : context) (e : exp) : tp = 
  match e with 
  | Var x ->
    (match cxt x with
    | Some tp -> tp
    | None -> raise TypeError) 
  | Lam (x, t, e') ->
    let new_cxt = extend cxt x t in
    let t_res = typing new_cxt e' in
    Fun (t, t_res)
  | App (e1, e2) ->
    (match typing cxt e1 with
    | Fun (t1, t2) ->
      let t_arg = typing cxt e2 in
      if t1 = t_arg then t2 else raise TypeError
    | _ -> raise TypeError)
  | Pair (e1, e2) ->
    let t1 = typing cxt e1 in
    let t2 = typing cxt e2 in
    Prod (t1, t2)
  | Fst e' ->
    (match typing cxt e' with
    | Prod (t1, _) -> t1
    | _ -> raise TypeError)
  | Snd e' ->
    (match typing cxt e' with
    | Prod (_, t2) -> t2
    | _ -> raise TypeError)
  | Eunit -> Unit
  | Inl (e', t) -> 
    let t_inl = typing cxt e' in
    Sum (t_inl, t)
  | Inr (e', t) -> 
    let t_inr = typing cxt e' in
    Sum (t, t_inr)
  | Case (e', x1, e1, x2, e2) ->
    (match typing cxt e' with
    | Sum (t1, t2) -> 
      let new_cxt_1 = extend cxt x1 t1 in
      let new_cxt_2 = extend cxt x2 t2 in
      let t_e1 = typing new_cxt_1 e1 in
      let t_e2 = typing new_cxt_2 e2 in
      if t_e1 = t_e2 then t_e1
      else raise TypeError
    | _ -> raise TypeError)
  | Fix (x, t, e') ->
    let new_cxt = extend cxt x t in
    let t' = typing new_cxt e' in
    if t = t' then t else raise TypeError
  | True | False -> Bool
  | Ifthenelse (e', e1, e2) ->
    (match typing cxt e' with
    | Bool -> 
      let t_e1 = typing cxt e1 in
      let t_e2 = typing cxt e2 in
      if t_e1 = t_e2 then t_e1
      else raise TypeError
    | _ -> raise TypeError)
  | Num n -> Int
  | Plus | Minus -> Fun (Prod(Int, Int), Int)
  | Eq -> Fun (Prod(Int, Int), Bool)


let typeOf e = typing (createEmptyContext ()) e 
let typeOpt e = try Some (typeOf e) 
                with TypeError -> None



