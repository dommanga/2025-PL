open Tml
exception NotImplemented 
exception Stuck
exception NotConvertible

type stoval = 
    Computed of value 
  | Delayed of exp * env

 and stack =
   Hole_SK
   | Frame_SK of stack * frame

 and state =
   Anal_ST of (stoval Heap.heap) * stack * exp * env
   | Return_ST of (stoval Heap.heap) * stack * value 

 (* Define your own datatypes *)
 and env = NOT_IMPLEMENT_ENV
 and value = NOT_IMPLEMENT_VALUE
 and frame = NOT_IMPLEMENT_FRAME
(* Define your own empty environment *)
let emptyEnv = NOT_IMPLEMENT_ENV

(* Implement the function value2exp : value -> Tml.exp
 * Warning : If you give wrong implementation of this function,
 *           you wiil receive no credit for the entire third part!  *)
let value2exp _ = raise NotImplemented

(* Problem 1. 
 * texp2exp : Tml.texp -> Tml.exp *)
type naming_cxt = (string * int) list

let texp2exp texp =
  let rec lookup cxt x =
    match cxt with
    | [] -> None
    | (y, i) :: rest -> if x = y then Some i else lookup rest x
  in

  let rec shift cxt = List.map (fun (x, i) -> (x, i + 1)) cxt in

  let rec texp2exp_aux (e : texp) (bound : naming_cxt) (free : naming_cxt ref) : exp =
    match e with
    | Tvar x ->
        (match lookup bound x with
          | Some i -> Ind i
          | None ->
            match lookup !free x with
            | Some i -> Ind (List.length bound + i)
            | None ->
              let idx = List.length !free in
              free := !free @ [(x, idx)];
              Ind (List.length bound + idx))
    | Tlam (x, _, e1) ->
        Lam (texp2exp_aux e1 ((x, 0) :: shift bound) free)
    | Tapp (e1, e2) ->
        App (texp2exp_aux e1 bound free, texp2exp_aux e2 bound free)
    | Tpair (e1, e2) ->
        Pair (texp2exp_aux e1 bound free, texp2exp_aux e2 bound free)
    | Tfst e1 -> Fst (texp2exp_aux e1 bound free)
    | Tsnd e1 -> Snd (texp2exp_aux e1 bound free)
    | Teunit -> Eunit
    | Tinl (e1, _) -> Inl (texp2exp_aux e1 bound free)
    | Tinr (e1, _) -> Inr (texp2exp_aux e1 bound free)
    | Tcase (e0, x1, e1, x2, e2) ->
        let b1 = (x1, 0) :: shift bound in
        let b2 = (x2, 0) :: shift bound in
        Case (
          texp2exp_aux e0 bound free,
          Lam (texp2exp_aux e1 b1 free),
          Lam (texp2exp_aux e2 b2 free)
        )
    | Tfix (x, _, e1) ->
        Fix (texp2exp_aux e1 ((x, 0) :: shift bound) free)
    | Ttrue -> True
    | Tfalse -> False
    | Tifthenelse (e0, e1, e2) ->
        Ifthenelse (
          texp2exp_aux e0 bound free,
          texp2exp_aux e1 bound free,
          texp2exp_aux e2 bound free
        )
    | Tnum n -> Num n
    | Tplus -> Plus
    | Tminus -> Minus
    | Teq -> Eq
  in
  texp2exp_aux texp [] (ref [])
  

(* Problem 2. 
 * step1 : Tml.exp -> Tml.exp *)   
let rec step1 _ = raise Stuck

(* Problem 3. 
 * step2 : state -> state *)
let step2 _ = raise Stuck
                    
(* exp2string : Tml.exp -> string *)
let rec exp2string exp = 
  match exp with 
    Ind x -> string_of_int x
  | Lam e -> "(lam. " ^ (exp2string e) ^ ")"
  | App (e1, e2) -> "(" ^ (exp2string e1) ^ " " ^ (exp2string e2) ^ ")"
  | Pair (e1, e2) -> "(" ^ (exp2string e1) ^ "," ^ (exp2string e2) ^ ")"
  | Fst e -> "(fst " ^ (exp2string e) ^ ")"
  | Snd e -> "(snd " ^ (exp2string e) ^ ")"
  | Eunit -> "()"
  | Inl e -> "(inl " ^ (exp2string e) ^ ")"
  | Inr e -> "(inr " ^ (exp2string e) ^ ")"
  | Case (e, e1, e2) -> "(case " ^ (exp2string e) ^" of " ^ (exp2string e1) ^
                          " | " ^ (exp2string e2) ^ ")"
  | Fix e -> "(fix. "  ^ (exp2string e) ^ ")"
  | Ifthenelse (e, e1, e2) -> 
     "(if " ^ (exp2string e) ^ " then " ^ (exp2string e1) ^ 
       " else " ^ (exp2string e2) ^ ")"
  | True -> "true"  | False -> "false"
  | Num n -> "<" ^ (string_of_int n) ^ ">"
  | Plus -> "+"  | Minus -> "-" | Eq -> "="

(* state2string : state -> string 
 * you may modify this function for debugging your code *)
let state2string st = match st with
    Anal_ST(_,_,exp,_) -> "Analysis : ???"
  | Return_ST(_,_,_) -> "Return : ??? "

(* ------------------------------------------------------------- *)     
let stepOpt1 e = try Some (step1 e) with Stuck -> None
let stepOpt2 st = try Some (step2 st) with Stuck -> None

let rec multiStep1 e = try multiStep1 (step1 e) with Stuck -> e
let rec multiStep2 st = try multiStep2 (step2 st) with Stuck -> st

let stepStream1 e =
  let rec steps e = 
    match (stepOpt1 e) with
      None -> Stream.from (fun _ -> None)
    | Some e' -> Stream.icons e' (steps e')
  in 
  Stream.icons e (steps e)

let stepStream2 st =
  let rec steps st = 
    match (stepOpt2 st) with
      None -> Stream.from (fun _ -> None)
    | Some st' -> Stream.icons st' (steps st')
  in 
  Stream.icons st (steps st)
