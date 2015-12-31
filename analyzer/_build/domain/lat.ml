(** Lattice signature.  *)

module type SET =
sig
  type t

  val name : string
  val to_string : t -> string
(*  val to_json : t -> Yojson.Safe.json*)
end

module type LAT =
sig
  include SET

  val le : t -> t -> bool
  val eq : t -> t -> bool

  val bot : t

  val join : t -> t -> t
  val meet : t -> t -> t

  val widen : t -> t -> t
  val narrow : t -> t -> t
end
