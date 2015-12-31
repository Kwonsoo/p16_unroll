(** Lattice constructor: powerset.  *)

open Lat
open Vocab

module Pow (A:SET) =
struct
  type t = A.t BatSet.t
  
  exception Error
  
  let name : string = "(pow " ^ A.name ^ ")"


  let to_string : t -> string = fun x ->
    if BatSet.is_empty x then "bot" else
      string_of_set A.to_string x


  let table = Hashtbl.create 251
  let hashcons x =
    try Hashtbl.find table x with
      Not_found -> Hashtbl.add table x x; x

  let le : t -> t -> bool = BatSet.subset

  let eq : t -> t -> bool = BatSet.equal

  let bot : t = BatSet.empty


  let join : t -> t -> t = fun x y ->
    if le x y then y else
    if le y x then x else
      hashcons (BatSet.union x y)

  let meet : t -> t -> t = fun x y ->
    if le x y then x else
    if le y x then y else
      hashcons (BatSet.intersect x y)


  (* Since module A is finite,  widening is defined as union which is
     sufficient to guarantee analysis termination.  *)
  let widen : t -> t -> t = fun x y ->
    if x = y then x else
      hashcons (BatSet.union x y)

  let narrow : t -> t -> t = fun x y ->
    if x = y then x else
      hashcons (BatSet.intersect x y)


  let filter : (A.t -> bool) -> t -> t = fun f s ->
    hashcons (BatSet.filter f s)

  let fold : (A.t -> 'a -> 'a) -> t -> 'a -> 'a = BatSet.fold

  let map = BatSet.map

  let iter : (A.t -> unit) -> t -> unit = BatSet.iter

  let singleton : A.t -> t = fun e ->
    hashcons (BatSet.singleton e)

  let subset : A.t BatSet.t -> t -> bool = BatSet.subset

  let cardinal : t -> int = BatSet.cardinal

  let mem : A.t -> t -> bool = BatSet.mem

  let add e s = hashcons (BatSet.add e s)

  let choose = BatSet.choose

  let remove = BatSet.remove

  let is_empty = BatSet.is_empty

  let for_all = BatSet.for_all
  let exists = BatSet.exists
(*  let to_json : t -> Yojson.Safe.json = fun pow ->
    Yojson.Safe.(
      `List (
        fold
        (fun a json ->
          (A.to_json a)::json
        ) pow []
      )
    )
*)    
end
