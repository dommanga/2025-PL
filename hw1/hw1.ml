exception Not_implemented

type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree

let rec sum n = 
  if n = 1 then 1
  else sum(n - 1) + n
let rec power x n = 
  if n = 0 then 1
  else power x (n - 1) * x
let rec gcd m n = 
  if m = 0 then n
  else if n = 0 then m
  else if m > n then gcd (m mod n) n
  else gcd (n mod m) m
let rec combi n k = 
  if k = 0 || k = n then 1
  else combi (n - 1) (k - 1) + combi (n - 1) k

let rec sum_tree t =
  match t with
  | Leaf (a) -> a
  | Node (l, n, r) -> sum_tree l  + sum_tree r + n
let rec depth t = 
  match t with
  | Leaf (_) -> 0
  | Node (l, _, r) -> 
    let d_left = depth l in
    let d_right = depth r in
    if d_left > d_right then d_left + 1
    else d_right + 1
let rec bin_search t x = 
  match t with
  | Leaf (a) -> x = a
  | Node (l, n, r) -> 
    if x = n then true
    else if x > n then bin_search r x
    else bin_search l x
let rec postorder t = 
  match t with
  | Leaf (a) -> [a]
  | Node (l, n, r) -> postorder l @ postorder r @ [n]

let rec max l = 
  match l with
  | [] -> 0
  | h :: t -> 
    let m = max t in
    if h > m then h
    else m
let rec list_add l m = 
  match (l, m) with
  | ([], m) -> m
  | (l, []) -> l
  | (h1::t1, h2::t2) -> [h1 + h2] @ list_add t1 t2
let rec insert m l = 
  match l with
  | [] -> [m]
  | h :: t ->
    if m > h then h :: insert m t
    else m :: h :: t
let rec insort l = 
  match l with
  | [] -> []
  | h :: t -> insert h (insort t)

let rec compose f g = fun x -> g(f(x))
let rec curry f = fun a -> fun b -> f (a, b)
let rec uncurry f = fun (a, b) -> f a b
let rec multifun f n = 
  if n = 1 then f
  else fun x -> f(multifun f (n - 1) x)

let rec ltake l n =
  match l with
  | [] -> []
  | h :: t -> if n > 1 then h :: ltake t (n - 1) else [h]
let rec lall f l = 
  match l with
  | [] -> true
  | h :: t -> f h && lall f t
let rec lmap f l = 
  match l with
  | [] -> []
  | h :: t -> f h :: lmap f t
let rec lrev l = 
  match l with
  | [] -> []
  | h :: t -> lrev t @ [h]
let rec lflat l = 
  match l with
  | [] -> []
  | h :: t -> h @ lflat t
let rec lzip l m = 
  match (l, m) with
  | ([], m) -> []
  | (l, []) -> []
  | (h1::t1, h2::t2) -> (h1, h2) :: lzip t1 t2
let rec split l = 
  match l with
  | [] -> ([], [])
  | [x] -> ([x], [])
  | h :: t -> raise Not_implemented
let rec cartprod _ _ = raise Not_implemented
let rec powerset _ = raise Not_implemented
