open Types

module Predictor : sig

	(* Produce single-query programs from the given new program. *)
	val copy_pgms : dir -> dir -> unit
	(**)
	val build_candidates : dir -> (fvector * locset) BatSet.t
	(* Select and return the locset's that are highly likely to increase precision. *)
	val select : dir -> (fvector * locset) BatSet.t -> locset

end = struct

	let copy_pgms = fun newprog sqdir -> ()	(* TODO *)

	let build_candidates = fun sqdir -> BatSet.empty	(* TODO *)

	let select = fun classifier_path candidates -> ">> Location sets are selected."; BatSet.empty	(* TODO *)

end
