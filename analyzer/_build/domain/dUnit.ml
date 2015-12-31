(** Key constructor: unit. *)

open Lat
open Vocab

module DUnit =
struct
  type t = unit

  let name : string = "unit"

  let to_string : t -> string = fun x -> "()"
  let to_json x = 
    Yojson.Safe.(`String "()")
end
