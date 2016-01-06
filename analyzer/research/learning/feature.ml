open Flang
open Extractor
open Training
open Cil

(*=========================================================================== 
 NOTE:
 gen (i.e., extract) 과 pred (i.e., match) 의 인터페이스 명세를 한글로 적어놓았습니다.
 두 함수를 구현할 때는 여기에 맞추고,
 이거 자체를 바꾸는게 낫다면 같이 얘기한 다음에 수정합시다.
 ===========================================================================*)

(* extract TODO
		- 인자로 받는 프로그램은 single-query prgram 이어야 한다.
		- 만들어진 feature set의 각 원소들은 쿼리까지의 서로 다른 path 여아 한다.*)

let gen : Global.t -> Flang.t BatSet.t
= fun global ->
	let file = global.file in
	let observe_fd = Slicer.find_observe_fundec file in
	Extractor.get_featSet observe_fd 

(*
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
*)

let extract_locl_vars : Cil.fundec -> string BatSet.t
= fun fd -> 
	let vinfos = fd.slocals in
	let vnames = List.map (fun vinfo -> vinfo.vname) vinfos in
	BatSet.of_list vnames

(* May be used in futere 
(* Extract literally raw paths from the given single-query program. *)
let extract_raw_paths : Global.t -> (string * Cil.stmt list BatSet.t)
=fun global ->
	let file = global.file in
	let observe_fd = Slicer.find_observe_fundec file in
	let funname = observe_fd.svar.vname in
	let rawTbl = Extractor.build_varTbl observe_fd in
	let raw_paths = Hashtbl.fold (fun key value accum -> 
			BatSet.add value accum) rawTbl BatSet.empty in
	(funname, raw_paths)

let get_vinfos_set : Cil.stmt -> Cil.varinfo BatSet.t
= fun s ->
	match s.skind with
	| Instr il ->
		List.fold_left (fun acc instr ->
			match instr with
			| Set (lv, e, _) -> 
				let lv_info_list = Ptranal.resolve_lval lv in
				let e_info_list = Ptranal.resolve_exp e in
				let lv_set = BatSet.of_list lv_info_list in
				let e_set = BatSet.of_list e_info_list in
				let vset = BatSet.union acc lv_set in
				BatSet.union vset e_set
			| _ -> acc) BatSet.empty il
	| If (cond, _, _, _) -> 
		let e_info_list = Ptranal.resolve_exp cond in
		BatSet.of_list e_info_list
	| _ -> BatSet.empty

let extract_vnames_from_path : Cil.stmt list -> string BatSet.t
= fun stmts ->
	let vinfo_set_list = List.map get_vinfos_set stmts in
	let vinfo_set = List.fold_left (fun acc vset ->
		BatSet.union acc vset) BatSet.empty vinfo_set_list
	in BatSet.map (fun vinfo -> vinfo.vname) vinfo_set
*)

	
(* match TODO
		- 인자로 받는 프로그램은 extract된 program, 즉, feature path들의 set이어야 한다.
		- 인자로 받는 feature는 feature path 하나를 의미한다.
		- 리턴값은 feature path가 extracted program에 있으면 true, 없으면 false 이다. *)
let pred : Flang.t BatSet.t -> Flang.t -> bool
=fun prog f ->
	BatSet.exists (fun p ->
			Match.match_fl p f
		) prog


(*
let pred : Global.t -> Flang.t -> bool
= fun global feature ->
	let sliced_fd = Slicer.find_observe_fundec global.file in
	let pgm_in_fl = Extractor.get_featSet sliced_fd in
	BatSet.exists (fun p ->
		Match.match_fl p feature
	) pgm_in_fl
*)
