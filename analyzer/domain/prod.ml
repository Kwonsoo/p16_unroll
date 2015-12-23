(** Lattice constructor: cartesian product.  *)

open Lat
open Vocab

module ProdSet (A:SET) (B:SET) =
struct
  type t = A.t * B.t

  let name : string = "(" ^ A.name ^ "*" ^ B.name ^ ")"

  let to_string : t -> string = fun (a,b) ->
    "(" ^ A.to_string a ^ "," ^ B.to_string b ^ ")"
(*  let to_json : t -> Yojson.Safe.json = fun x ->
    Yojson.Safe.(
      `List (
        [ A.to_json (fst x); B.to_json (snd x);
        ]
      )
    )
*)    
end


module Prod (A:LAT) (B:LAT) =
struct
  include ProdSet (A) (B)

  let table = Hashtbl.create 251
  let hashcons x =
    try Hashtbl.find table x with
      Not_found -> Hashtbl.add table x x; x

  let le : t -> t -> bool = fun (xa, xb) (ya, yb) -> A.le xa ya && B.le xb yb
  let eq : t -> t -> bool = fun (xa, xb) (ya, yb) -> A.eq xa ya && B.eq xb yb

  let bot : t = hashcons (A.bot, B.bot)

  let join : t -> t -> t = fun x y ->
    if le x y then y else
    if le y x then x else
      hashcons (A.join (fst x) (fst y), B.join (snd x) (snd y))
  let meet : t -> t -> t = fun x y ->
    if le x y then x else
    if le y x then y else
      hashcons (A.meet (fst x) (fst y), B.meet (snd x) (snd y))

  let widen : t -> t -> t = fun x y ->
    if x = y then x else
      hashcons (A.widen (fst x) (fst y), B.widen (snd x) (snd y))
  let narrow : t -> t -> t = fun x y ->
    if x = y then x else
      hashcons (A.narrow (fst x) (fst y), B.narrow (snd x) (snd y))
end

(** {6 Cartesian product for multiple lattices}  *)
module ProdSet3 (A:SET) (B:SET) (C:SET) =
struct
  module E2 = ProdSet (A) (B)
  include ProdSet (E2) (C)
end

module ProdSet4 (A:SET) (B:SET) (C:SET) (D:SET) =
struct
  module E2 = ProdSet (A) (B)
  module E3 = ProdSet (E2) (C)
  include ProdSet (E3) (D)
end

module ProdSet5 (A:SET) (B:SET) (C:SET) (D:SET) (E:SET) =
struct
  module E2 = ProdSet (A) (B)
  module E3 = ProdSet (E2) (C)
  module E4 = ProdSet (E3) (D)
  include ProdSet (E4) (E)
end


module Prod3 (A:LAT) (B:LAT) (C:LAT) =
struct
   type t = A.t * B.t * C.t
  let name : string = "("^A.name^"*"^B.name^"*"^C.name^")"

  let le (a1,b1,c1) (a2,b2,c2) = 
    (A.le a1 a2) && (B.le b1 b2) && (C.le c1 c2)
  let eq (a1,b1,c1) (a2,b2,c2) = 
    (A.eq a1 a2) && (B.eq b1 b2) && (C.eq c1 c2)
  let join (a1,b1,c1) (a2,b2,c2) = 
    (A.join a1 a2, B.join b1 b2, C.join c1 c2)
  let meet (a1,b1,c1) (a2,b2,c2) = 
    (A.meet a1 a2, B.meet b1 b2, C.meet c1 c2)
  let widen (a1,b1,c1) (a2,b2,c2) = 
    (A.widen a1 a2, B.widen b1 b2, C.widen c1 c2)
  let narrow (a1,b1,c1) (a2,b2,c2) = 
    (A.narrow a1 a2, B.narrow b1 b2, C.narrow c1 c2)

  let bot = (A.bot, B.bot, C.bot)

  let fst (a,_,_) = a
  let snd (_,b,_) = b
  let trd (_,_,c) = c

  let to_string x = 
    (A.to_string (fst x))^", "^(B.to_string (snd x))^", "^(C.to_string (trd x))
(*  let to_json : t -> Yojson.Safe.json = fun x ->
    Yojson.Safe.(
      `List (
        [ A.to_json (fst x); B.to_json (snd x);
          C.to_json (trd x); 
        ]
      )
    )
*) 
end

module Prod4 (A:LAT) (B:LAT) (C:LAT) (D:LAT) =
struct
  type t = A.t * B.t * C.t * D.t
  let name : string = "("^A.name^"*"^B.name^"*"^C.name^"*"^D.name^")"

  let le (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.le a1 a2) && (B.le b1 b2) && (C.le c1 c2) && (D.le d1 d2)
  let eq (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.eq a1 a2) && (B.eq b1 b2) && (C.eq c1 c2) && (D.eq d1 d2)
  let join (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.join a1 a2, B.join b1 b2, C.join c1 c2, D.join d1 d2)
  let meet (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.meet a1 a2, B.meet b1 b2, C.meet c1 c2, D.meet d1 d2)
  let widen (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.widen a1 a2, B.widen b1 b2, C.widen c1 c2, D.widen d1 d2)
  let narrow (a1,b1,c1,d1) (a2,b2,c2,d2) = 
    (A.narrow a1 a2, B.narrow b1 b2, C.narrow c1 c2, D.narrow d1 d2)

  let bot = (A.bot, B.bot, C.bot, D.bot)

  let fst (a,_,_,_) = a
  let snd (_,b,_,_) = b
  let trd (_,_,c,_) = c
  let frth (_,_,_,d) = d

  let to_string x = 
    "("^(A.to_string (fst x))^", "^(B.to_string (snd x))^", "^(C.to_string (trd x))
    ^", "^(D.to_string (frth x))^")"
(*  let to_json : t -> Yojson.Safe.json = fun x ->
    Yojson.Safe.(
      `List (
        [ A.to_json (fst x); B.to_json (snd x);
          C.to_json (trd x); D.to_json (frth x);
        ]
      )
    )
*)
end

module Prod5 (A:LAT) (B:LAT) (C:LAT) (D:LAT) (E:LAT) =
struct
  type t = A.t * B.t * C.t * D.t * E.t
  let name : string = "("^A.name^"*"^B.name^"*"^C.name^"*"^D.name^"*"^E.name^")"

  let le (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.le a1 a2) && (B.le b1 b2) && (C.le c1 c2) && (D.le d1 d2) && (E.le e1 e2)
  let eq (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.eq a1 a2) && (B.eq b1 b2) && (C.eq c1 c2) && (D.eq d1 d2) && (E.eq e1 e2)
  let join (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.join a1 a2, B.join b1 b2, C.join c1 c2, D.join d1 d2, E.join e1 e2)
  let meet (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.meet a1 a2, B.meet b1 b2, C.meet c1 c2, D.meet d1 d2, E.meet e1 e2)
  let widen (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.widen a1 a2, B.widen b1 b2, C.widen c1 c2, D.widen d1 d2, E.widen e1 e2)
  let narrow (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = 
    (A.narrow a1 a2, B.narrow b1 b2, C.narrow c1 c2, D.narrow d1 d2, E.narrow e1 e2)

  let bot = (A.bot, B.bot, C.bot, D.bot, E.bot)

  let fst (a,_,_,_,_) = a
  let snd (_,b,_,_,_) = b
  let trd (_,_,c,_,_) = c
  let frth (_,_,_,d,_) = d
  let fifth (_,_,_,_,e) = e
  let to_string : t -> string 
  = fun x ->
    "("^(A.to_string (fst x))^", "^(B.to_string (snd x))^", "^(C.to_string (trd x))
    ^", "^(D.to_string (frth x))^", "^(E.to_string (fifth x))^")"
end
