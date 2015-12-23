(** Lattice constructor: sum.  *)

open Lat

module SumSet (A:SET) (B:SET) =
struct
  type t = Inl of A.t | Inr of B.t

  let name : string = "(" ^ A.name ^ "+" ^ B.name ^ ")"

  let to_string : t -> string = function
    | Inl a -> A.to_string a
    | Inr a -> B.to_string a
(*  let to_json : t -> Yojson.Safe.json = fun sumset ->
    match sumset with
    | Inl a -> A.to_json a
    | Inr a -> B.to_json a
*)    
end

module Sum (A:LAT) (B:LAT) =
struct
  type t = Top | Inl of A.t | Inr of B.t | Bot

  let name : string = "(" ^ A.name ^ "+" ^ B.name ^ ")"

  let to_string : t -> string = function
    | Top -> "top"
    | Inl a -> A.to_string a
    | Inr a -> B.to_string a
    | Bot -> "bot"

(*  let to_json : t -> Yojson.Safe.json = fun sum ->
    Yojson.Safe.(
      match sum with
      | Top -> `String "top"
      | Inl a -> A.to_json a
      | Inr a -> B.to_json a
      | Bot -> `String "bot"
    )
*)
  let le : t -> t -> bool = fun x y ->
    match x,y with
    | _,Top -> true
    | Top,_ -> false
    | Bot,_ -> true
    | _,Bot -> false
    | Inl a,Inl b -> A.le a b
    | Inr a,Inr b -> B.le a b
    | _,_ -> false

  let eq : t -> t -> bool = fun x y ->
    match x,y with
    | Top,Top -> true
    | Bot,Bot -> true
    | Inl a,Inl b -> A.eq a b
    | Inr a,Inr b -> B.eq a b
    | _,_ -> false

  let top : t = Top
  let bot : t = Bot

  let join : t -> t -> t = fun x y ->
    if x = y then x else
      match x,y with
      | Top,_
      | _,Top -> Top
      | Bot,a
      | a,Bot -> a
      | Inl a,Inl b -> Inl (A.join a b)
      | Inr a,Inr b -> Inr (B.join a b)
      | Inl _,Inr _
      | Inr _,Inl _ -> Top

  let meet : t -> t -> t = fun x y ->
    if x = y then x else
      match x,y with
      | Top,a
      | a,Top -> a
      | Bot,_
      | _,Bot -> Bot
      | Inl a,Inl b -> Inl (A.meet a b)
      | Inr a,Inr b -> Inr (B.meet a b)
      | Inl _,Inr _
      | Inr _,Inl _ -> Bot

  let widen : t -> t -> t = fun x y ->
    if x = y then x else
      match x,y with
      | _,Top -> Top
      | Top,_ -> Top
      | Bot,a -> a
      | a,Bot -> a
      | Inl a,Inl b -> Inl (A.widen a b)
      | Inr a,Inr b -> Inr (B.widen a b)
      | _,_ -> Top

  let narrow : t -> t -> t = fun x y ->
    if x = y then x else
      match x,y with
      | Top,a -> a
      | _,Bot -> Bot
      | Inl a,Inl b -> Inl (A.narrow a b)
      | Inr a,Inr b -> Inr (B.narrow a b)
      | _,_ -> invalid_arg "sum.ml: narrow"

end

(** {6 Sum for multiple lattices}  *)
module SumSet3 (A:SET) (B:SET) (C:SET) =
struct
  module E2 = SumSet (A) (B)
  include SumSet (E2) (C)
end

module SumSet4 (A:SET) (B:SET) (C:SET) (D:SET) =
struct
  module E2 = SumSet (A) (B)
  module E3 = SumSet (E2) (C)
  include SumSet (E3) (D)
end

module SumSet5 (A:SET) (B:SET) (C:SET) (D:SET) (E:SET) =
struct
  module E2 = SumSet (A) (B)
  module E3 = SumSet (E2) (C)
  module E4 = SumSet (E3) (D)
  include SumSet (E4) (E)
end

module Sum2 (A:LAT) (B:LAT) =
struct
  include Sum (A) (B)
end

module Sum3 (A:LAT) (B:LAT) (C:LAT) =
struct
  module S2 = Sum (A) (B)
  include Sum (S2) (C)
end

module Sum4 (A:LAT) (B:LAT) (C:LAT) (D:LAT) =
struct
  module S2 = Sum (A) (B)
  module S3 = Sum (S2) (C)
  include Sum (S3) (D)
end

module Sum5 (A:LAT) (B:LAT) (C:LAT) (D:LAT) (E:LAT) =
struct
  module S2 = Sum (A) (B)
  module S3 = Sum (S2) (C)
  module S4 = Sum (S3) (D)
  include Sum (S4) (E)
end
