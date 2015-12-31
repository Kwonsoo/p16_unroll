(** Concrete key: string.  *)

type t = string

let name : string = "string"
let to_string : t -> string = fun x -> x
let to_json x =
  Yojson.Safe.(`String x)
