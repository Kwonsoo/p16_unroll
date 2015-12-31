type dir = string

type fvector = bool list
type tdata = (fvector * bool)
	
let fvectorize : Global.t -> fvector
=fun global -> [true] (* TODO *)

let build_t_data : Global.t -> tdata
=fun global -> ([true], true)	(* TODO *)

	
module Trainer : sig

	(* Produce single-query programs from the given T2 directory. *)
	val copy_pgms : dir -> dir -> unit
	(* Build all training data from the single-query programs. *)
	val build_training_dataset : dir -> tdata BatSet.t

end = struct 

	let copy_pgms = fun t2dir sqdir -> ()	(* TODO *)

	let build_training_dataset = fun sqdir -> BatSet.empty	(* TODO *)

end
