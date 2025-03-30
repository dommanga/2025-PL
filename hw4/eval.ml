(*
 * Call-by-value reduction   
 *)
 open Uml
 exception NotImplemented 
 exception Stuck
 
 let freshVarCounter = ref 0
                           
 (*   getFreshVariable : string -> string 
  *   use this function if you need to generate a fresh variable from s. 
  *)
 let getFreshVariable s = 
   let _ = freshVarCounter := !freshVarCounter + 1
   in
   s ^ "__" ^ (string_of_int (!freshVarCounter))
 
 module StringSet = Set.Make(String)
 
 let rec freeVars e = 
   match e with
   | Var x -> StringSet.singleton x
   | Lam (x, body) -> StringSet.diff (freeVars body) (StringSet.singleton x)
   | App (e1, e2) -> StringSet.union (freeVars e1) 
   (freeVars e2)
 
 let rec subst e' x e = 
   match e with
   | Var y -> if x = y then e' else Var y
   | App (e1, e2) -> App (subst e' x e1, subst e' x e2)
   | Lam (y, body) -> 
     if x = y then Lam (y, body)
     else if not (StringSet.mem y (freeVars e')) then Lam (y, subst e' x body)
     else 
       let z = getFreshVariable y in
       let body' = subst (Var z) y body in
       Lam (z, subst e' x body')
 (*
  * implement a single step with reduction using the call-by-value strategy.
  *)
 let rec stepv e = 
   match e with
   | Var x -> raise Stuck
   | Lam (x, body) -> raise Stuck
   | App (e1, e2) -> 
     try
       let e1' = stepv e1 in 
       App (e1', e2)
     with Stuck -> 
       match e1 with
       | Lam (x, body) -> 
         try
           let e2' = stepv e2 in
           App (e1, e2')
         with Stuck ->
           match e2 with
           | Lam _ | Var _ -> subst e2 x body
           | App _ -> raise Stuck
       | _ -> raise Stuck

 let stepOpt stepf e = try Some (stepf e) with Stuck -> None
 
 let rec multiStep stepf e = try multiStep stepf (stepf e) with Stuck -> e
 
 let stepStream stepf e =
   let rec steps e = 
     match (stepOpt stepf e) with 
       None -> Stream.from (fun _ -> None)
     | Some e' -> Stream.icons e' (steps e')
   in 
   Stream.icons e (steps e)
 
 