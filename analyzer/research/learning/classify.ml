module Classifier : sig
	
	(* Learn a classifer based on the given feature vector and the training data D. *)
	val learn : Flang.t list -> (bool list * bool) BatSet.t -> unit
	
	(* Classify the given new fvectorized query. *)
	val classify : bool list -> bool

end = struct

	let learn = fun fvector tdata_set -> ()	(* TODO *)
		
	let classify = fun newprog -> true	(* TODO *)

end
