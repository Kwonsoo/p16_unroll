open Flang
open Extractor

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
