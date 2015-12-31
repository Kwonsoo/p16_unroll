type t = int list (* placeholder *)
type dir = string

let gen : Global.t -> t BatSet.t
=fun global -> BatSet.empty (* TODO *)

let pred : Global.t -> t -> bool
=fun global feature -> true (* TODO *)


module FGenerator : sig

	(* Generate the feature set from the raw features. *)
	val gen_features : dir -> t BatSet.t

end = struct

	let gen_Features = fun reduced_dir -> BatSet.empty

end
