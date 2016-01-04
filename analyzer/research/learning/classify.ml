module Classifier : sig
	
	(* Learn a classifer based on the training data set D from T2. *)
	val learn : Types.tdata BatSet.t -> unit
	
	(* Classify the given new fvectorized query. *)
	val classify : Types.fvector -> bool

end = struct

	let learn = fun tdata_set -> prerr_endline ">> The classifier is learned."; ()	(* TODO *)
		
	let classify = fun newprog -> true	(* TODO *)

end
