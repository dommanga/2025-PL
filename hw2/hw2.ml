exception NotImplemented
	    
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree
						      
(** Recursive functions **)

let rec lrevrev l = 
  match l with
  | [] -> []
  | h :: t ->
    let rec lrev lst = 
      match lst with
      | [] -> []
      | h :: t -> lrev t @ [h]
    in
    lrevrev t @ [lrev h]

let rec lfoldl f e l = 
  match l with
  | [] -> e
  | h :: t -> lfoldl f (f (h, e)) t				
			 
(** Tail recursive functions  **)

let fact n = 
  let rec fact_aux n acc = 
    if n = 0 then acc
    else fact_aux (n-1) (acc * n)
  in fact_aux n 1

let fib n = 
  let rec fib_aux n (acc1, acc2) = 
    if n = 0 || n = 1 then acc1 + acc2
    else fib_aux (n-1) (acc1 + acc2, acc1)
  in fib_aux n (1, 0)

let alterSum l = 
  match l with
  | [] -> 0
  | [a] -> a
  | _ ->
    let rec alterSum_aux l acc sub = 
      match l with
      | [] -> acc
      | h :: t -> 
        if sub then alterSum_aux t (acc - h) (not sub)
        else alterSum_aux t (acc + h) (not sub)
    in alterSum_aux l 0 false


let ltabulate n f = 
  let rec ltabulate_aux n acc = 
    if n < 0 then acc
    else ltabulate_aux (n-1) (f n :: acc)
  in ltabulate_aux (n-1) []

let lfilter p l = 
  let rec lfilter_aux p l acc = 
    match l with
    | [] -> acc
    | h :: t -> 
      if p h = true then lfilter_aux p t (acc @ [h])
      else lfilter_aux p t acc
  in lfilter_aux p l []

let rec union s k = 
  match s with
  | [] -> k
  | h :: t -> 
    let rec mem_check a l = 
      match l with
      | [] -> false
      | h :: t -> 
        if a = h then true
        else mem_check a t
    in
    if mem_check h k then union t k
    else union t (h :: k)
      

let inorder t = 
  let rec inorder' (t': 'a tree) (post: 'a list) : 'a list = 
    match t' with
    | Leaf (a) -> a :: post
    | Node (l, n, r) -> inorder' l (n :: (inorder' r post))
  in
  inorder' t []
	   
let postorder t = 
  let rec postorder' t' post = 
    match t' with
    | Leaf (a) -> a :: post
    | Node (l, n, r) -> postorder' l (postorder' r (n :: post))
  in
  postorder' t []

let preorder t = 
  let rec preorder' t' post = 
    match t' with
    | Leaf (a) -> a :: post
    | Node (l, n, r) -> n :: preorder' l (preorder' r post)
  in
  preorder' t []

(** Sorting in the ascending order **)

let rec quicksort l = 
  match l with
  | [] -> []
  | h :: t ->
    let l_a = lfilter (fun x -> x <= h) t in
    let l_b = lfilter (fun x -> x > h) t in
    quicksort l_a @ [h] @ quicksort l_b

let rec mergesort l = 
  let rec split l = 
    match l with
    | [] -> ([], [])
    | [x] -> ([x], [])
    | h :: m :: t -> 
      let a, b = split t in
      (h :: a, m :: b)
  in
  let rec merge l m = 
    match (l, m) with
    | ([], m) -> m
    | (l, []) -> l
    | (h1::t1, h2::t2) -> 
      if h1 < h2 then h1 :: (merge t1 m)
      else h2 :: (merge l t2)
  in
  match l with
  | [] -> []
  | [a] -> [a]
  | _ ->
    let l_a, l_b = split l in
    merge (mergesort l_a) (mergesort l_b)
    
			
(** Structures **)

module type HEAP = 
  sig
    exception InvalidLocation
    type loc
    type 'a heap
    val empty : unit -> 'a heap
    val allocate : 'a heap -> 'a -> 'a heap * loc
    val dereference : 'a heap -> loc -> 'a 
    val update : 'a heap -> loc -> 'a -> 'a heap
  end
    
module type DICT =
  sig
    type key
    type 'a dict
    val empty : unit -> 'a dict
    val lookup : 'a dict -> key -> 'a option
    val delete : 'a dict -> key -> 'a dict
    val insert : 'a dict -> key * 'a -> 'a dict 
  end

module Heap : HEAP =
  struct
    exception InvalidLocation 
		
    type loc = int
    type 'a heap = (loc * 'a) list

    let empty () = []
    let allocate h v = 
      match h with
      | [] -> ([(0, v)], 0)
      | _ -> 
        let max_loc = List.fold_left (fun acc (loc, _) -> max acc loc) (-1) h in
        let new_loc = max_loc + 1 in
        ((new_loc, v) :: h, new_loc)
    let dereference h l =
      try
        snd (List.find (fun (loc, _) -> loc = l) h)
      with
        Not_found -> raise InvalidLocation
    let update h l v = 
      if List.exists (fun (loc, _) -> loc = l) h then
        List.map (fun (loc, val_) -> 
          if loc = l then (loc, v) 
          else (loc, val_)) h
      else raise InvalidLocation
  end
    
module DictList : DICT with type key = string =
  struct
    type key = string
    type 'a dict = (key * 'a) list
			      
    let empty () = []
    let lookup d k = 
      try
        Some (snd (List.find (fun (key, _) -> key = k) d))
      with
        Not_found -> None
    let delete d k =
      if List.exists (fun (key, _) -> key = k) d then 
        List.filter (fun (key, _) -> key <> k) d
      else d
    let insert d (k, v) = 
      if List.exists (fun (key, _) -> key = k) d then
        List.map (fun (key, item) -> if key = k then (key, v) else (key, item)) d
      else (k, v) :: d
  end
    
module DictFun : DICT with type key = string =
  struct
    type key = string
    type 'a dict = key -> 'a option
			     
    let empty () = fun _ -> None
    let lookup d k = d k
    let delete d k = 
      fun key -> 
        if key = k then None 
        else d key
    let insert d (k, v) = 
      fun key ->
        if key = k then Some v
        else d key
  end
