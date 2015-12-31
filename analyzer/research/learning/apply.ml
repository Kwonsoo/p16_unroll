type dir = string

module Apply : sig
	(* Produce single-query programs from the given new program. *)
	val copy_pgms : dir -> dir -> unit
end = struct

end
