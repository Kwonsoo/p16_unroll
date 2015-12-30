open Classify
	
let fvectorize : Global.t -> bool list
=fun global -> [true] (* TODO *)

let build_t_data : Global.t -> (bool list * bool)
=fun global -> ([true], true)	(* TODO *)
