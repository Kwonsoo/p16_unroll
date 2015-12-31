type locset = Loc.t BatSet.t

module Predictor : sig
	(* Produce single-query programs from the given new program. *)
	val copy_pgms : dir -> dir -> unit
	(**)
	val build_candidates : dir -> (fvector * locset) BatSet.t
	(* Selectively apply precision to some variables. *)
	val apply : dir -> (fvector * locset) BatSet.t -> unit

end = struct
	
	let copy_pgms = fun newprog sqdir -> ()	(* TODO *)

	let build_candidates = fun sqdir -> BatSet.empty	(* TODO *)

	let apply = fun classifier_path candidates -> ()	(* TODO *)

end
