(** Abstract semantics: main analysis. *)

open Cil
open Vocab
open AbsDom
open ItvDom
open DMap

module type AbsSem = 
sig
  module Dom : DMAP
(*  val zoo_print : bool -> string -> Cil.exp list -> Dom.t -> Cil.location -> unit
  val zoo_dump : bool -> Dom.t -> Cil.location -> unit
*)  val run : ?mode:AbsDom.update_mode -> ?locset: Dom.A.t BatSet.t -> ?premem:Mem.t ->
      ?silent:bool -> Node.t -> Dom.t * Global.t -> Dom.t * Global.t
end
