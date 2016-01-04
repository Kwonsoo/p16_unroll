open Flang
open Extractor
open Types

let gen : Global.t -> Flang.t BatSet.t
= fun global ->
	let file = global.file in
	let set_list =
		Cil.foldGlobals file (fun accum global ->
			match global with
			| Cil.GFun (fd, _) ->
					(Extractor.get_featSet fd)::accum
			| _ -> accum
		) []
	in union_over_set_list set_list

let pred : Global.t -> Flang.t -> bool
=fun global feature -> true (* TODO *)

module FGenerator : sig
	type dir = string

	(* Generate the feature set from the raw features. *)
	val gen_features : dir -> Flang.t BatSet.t

end = struct
	type dir = string

	let gen_features = fun reduced_dir -> prerr_endline ">> Features have been generated."; BatSet.empty	(* TODO *)

end
