open Flang
open Extractor
open Types
open Training

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
= fun global feature ->
	let sliced_fd = Slicer.find_observe_fundec global.file in
	let pgm_in_fl = Extractor.get_featSet sliced_fd in
	BatSet.exists (fun p ->
		Match.match_fl p feature
	) pgm_in_fl

module FGenerator : sig
	type dir = string

	(* Generate the feature set from the raw features. *)
	val gen_features : dir -> Flang.t BatSet.t

end = struct
	type dir = string

	let gen_from_one_source = fun file ->
		BatSet.empty
		
	let gen_features = fun reduced_dir ->
		(*각 reduced code를 읽어와서 extract 해서 나온 flang set들을 모두 union 하면 된다.*)	
		let files = Sys.readdir reduced_dir in
		let files = Array.to_list files in
		let features = List.fold_left (fun accum elem -> 
				let full_file_path = "../reduced" ^ elem in
				let a_feature_set = gen_from_one_source full_file_path in
				BatSet.union accum a_feature_set
				) BatSet.empty files in
		features

end
