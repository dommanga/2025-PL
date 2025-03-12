open Common

exception NotImplemented

exception IllegalFormat

module Integer : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 0
  let one = 1

  let (++) x y = x + y
  let ( ** ) x y = x * y
  let (==) x y = x = y 
end

(* Problem 1-1 *)
(* Scalars *)

module Boolean : SCALAR with type t = bool 
=
struct
  type t = bool

  exception ScalarIllegal

  let zero = false
  let one = true

  let (++) x y = x || y
  let ( ** ) x y = x && y
  let (==) x y = x == y
end

(* Problem 1-2 *)
(* Vectors *)

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t
=
struct
  type elem = Scal.t
  type t = elem list

  exception VectorIllegal

  let create l = 
    match l with
    | [] -> raise VectorIllegal
    | _ -> l
  let to_list v = v
  let dim v = List.length v
  let nth v n = 
    if n < 0 || n >= dim v then
      raise VectorIllegal
    else List.nth v n
  let (++) x y = 
    if dim x != dim y then
      raise VectorIllegal
    else
      List.map2 Scal.(++) x y
  let (==) x y = 
    if dim x != dim y then
      raise VectorIllegal
    else
      List.for_all2 Scal.(==) x y
  let innerp x y = 
    if dim x != dim y then
      raise VectorIllegal
    else
      List.fold_left Scal.(++) Scal.zero (List.map2 Scal.( ** ) x y)
end

(* Problem 1-3 *)
(* Matrices *)

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t
=
struct
  module V = VectorFn(Scal)

  type elem = Scal.t
  type t = V.t list

  exception MatrixIllegal

  let create ll = 
    let dim = List.length ll in
    if dim == 0 || List.exists (fun x -> dim != List.length x) ll then raise MatrixIllegal
    else
     List.map (V.create) ll
  let identity d = 
    if d <= 0 then raise MatrixIllegal
    else
      let make_row i = 
        let elements = List.init d (fun x -> if x != i then Scal.zero else Scal.one) in V.create elements
      in
      List.init d make_row
  let dim m = List.length m
  let transpose m = 
    if m = [] then raise MatrixIllegal
    else
      let col_cnt = V.dim (List.hd m) in
      let get_col j = 
        V.create (List.map (fun v -> V.nth v j) m) in
      List.init col_cnt get_col
  let to_list m = List.map V.to_list m
  let get m r c = 
    if r >= dim m || r < 0 || c >= dim m || c < 0 then raise MatrixIllegal
    else V.nth (List.nth m r) c
  let (++) m n =
    if dim m != dim n then raise MatrixIllegal
    else List.map2 V.(++) m n
  let ( ** ) m n = 
    if dim m != dim n then raise MatrixIllegal
    else 
      let n_t = transpose n in
      let row_cnt = dim m in
      let get_row i = 
        V.create (List.map (fun n_col -> V.innerp (List.nth m i) n_col) n_t) in
      List.init row_cnt get_row
  let (==) m n = 
    if dim m != dim n then raise MatrixIllegal
    else 
      List.for_all (fun x -> x) (List.map2 V.(==) m n)
end

(* Problem 2-1 *)
(* Closure *)

module ClosureFn (Mat : MATRIX) :
sig
  val closure : Mat.t -> Mat.t
end
=
struct
  let closure m = 
    let dim_m = Mat.dim m in
    let identity = Mat.identity dim_m in
    let rec closure_aux cur = 
      let ind_closure = Mat.(++) identity (Mat.( ** ) cur m) 
    in
      if Mat.(==) cur ind_closure then cur
      else closure_aux ind_closure
    in
    closure_aux identity
end

(* Problem 2-2 *)
(* Applications to Graph Problems *)

module BoolMat = MatrixFn (Boolean)
module BoolMatClosure = ClosureFn (BoolMat)

let reach _ = raise NotImplemented

let al = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  false; false];
   [false; true;  true;  false; true;  false];
   [false; true;  false; true;  true;  true];
   [false; false; true;  true;  true;  false];
   [false; false; false; true;  false; true]]

let solution_al' = 
  [[true;  false; false; false; false; false];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true];
   [false; true;  true;  true;  true;  true]]

module Distance : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)

  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let distance _ = raise NotImplemented

let dl =
  [[  0;  -1;  -1;  -1;  -1;  -1 ];
   [ -1; 0  ; 35 ; 200; -1 ; -1  ];
   [ -1; 50 ; 0  ; -1 ; 150; -1  ];
   [ -1; 75;  -1 ; 0  ; 100; 25  ];
   [ -1; -1 ; 50 ; 65 ; 0  ; -1  ];
   [ -1; -1 ; -1 ; -1 ; -1 ; 0   ]]

let solution_dl' =
  [[0;  -1;  -1;  -1;  -1;  -1  ];
   [-1; 0;   35;  200; 185; 225 ];
   [-1; 50;  0;   215; 150; 240 ];
   [-1; 75;  110; 0;   100; 25  ];
   [-1; 100; 50;  65;  0;   90  ];
   [-1; -1;  -1;  -1;  -1;  0   ]]

module Weight : SCALAR with type t = int
=
struct
  type t = int

  exception ScalarIllegal

  let zero = 999999              (* Dummy value : Rewrite it! *)
  let one = 999999               (* Dummy value : Rewrite it! *)
 
  let (++) _ _ = raise NotImplemented
  let ( ** ) _ _ = raise NotImplemented
  let (==) _ _ = raise NotImplemented
end

(* .. Write some code here .. *)

let weight _ = raise NotImplemented

let ml =
  [[-1; 0  ; 0  ; 0  ; 0  ; 0   ];
   [0 ; -1 ; 10 ; 100; 0  ; 0   ];
   [0 ; 50 ; -1 ; 0  ; 150; 0   ];
   [0 ; 75 ; 0  ; -1 ; 125; 40 ];
   [0 ; 0  ; 25 ; -1 ; -1 ; 0   ];
   [0 ; 0  ; 0  ; 0  ; 0  ; -1  ]]

let solution_ml' =
  [[-1; 0;  0;   0;   0;   0  ];
   [0;  -1; 25;  100; 100; 40 ];
   [0;  75; -1;  150; 150; 40 ];
   [0;  75; 25;  -1;  125; 40 ];
   [0;  75; 25;  -1;  -1;  40 ];
   [0;  0;  0;   0;   0;   -1 ]]

let _ =
  try 
  if reach al = solution_al' && distance dl = solution_dl' && weight ml = solution_ml' then
    print_endline "\nYour program seems fine (but no guarantee)!"
  else
    print_endline "\nYour program might have bugs!"
  with _ -> print_endline "\nYour program is not complete yet!" 

