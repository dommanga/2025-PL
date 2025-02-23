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

let rec sum_tree _ = raise Not_implemented
let rec depth _ = raise Not_implemented
let rec bin_search _ _ = raise Not_implemented
let rec postorder _ = raise Not_implemented

let rec max _ = raise Not_implemented
let rec list_add _ _ = raise Not_implemented
let rec insert _ _ = raise Not_implemented
let rec insort _ = raise Not_implemented

let rec compose _ _ = raise Not_implemented
let rec curry _ _ _ = raise Not_implemented
let rec uncurry _ _ = raise Not_implemented
let rec multifun _ _ = raise Not_implemented

let rec ltake _ _ = raise Not_implemented
let rec lall _ _ = raise Not_implemented
let rec lmap _ _ = raise Not_implemented
let rec lrev _ = raise Not_implemented
let rec lflat _ = raise Not_implemented
let rec lzip _ _ = raise Not_implemented
let rec split _ = raise Not_implemented
let rec cartprod _ _ = raise Not_implemented
let rec powerset _ = raise Not_implemented
