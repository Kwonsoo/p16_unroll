(** Key constructor: list. *)

open Lat
open Vocab

module DList (A:SET) =
struct
  type t = A.t list

  let name : string = "(" ^ A.name ^ " list)"

  let to_string : t -> string = fun x -> string_of_list A.to_string x
(*  let to_json : t -> Yojson.Safe.json = fun lst ->
    Yojson.Safe.(
      `List (
        List.fold_right
        (fun a json ->
          (A.to_json a)::json
        ) lst []
      )
    )*)
end
