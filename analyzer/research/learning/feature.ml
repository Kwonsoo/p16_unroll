open Extractor
open Training
open Cil
open Types
open Visitors
open Report

(*EXTRACT for T1.
	Get global that is already unrolled and return a set of unique paths to airac_observe in flang.*)
let gen_t1 : Global.t -> Flang.t BatSet.t
= fun global ->
	let intercfg = global.icfg in
	(*intracfg that has airac_observe*)
	let intracfg_observe = Slicer.find_observe_intracfg intercfg in
	let paths_in_intracfg = Extractor.get_paths intracfg_observe in
	(*paths containing and ending with airac_observe*)
	let paths_in_intracfg = paths_in_intracfg in
	(*translate to flang*)
	let paths_flang = BatSet.map (fun path ->
			Flang.trans_graph path
		) paths_in_intracfg in
	paths_flang

(*The given intracfg paths should be unique paths to the given query.*)
let gen_t2 : query -> IntraCfg.t BatSet.t -> Flang.t BatSet.t
=fun q pathset ->
	(*
	let paths_dependency = BatSet.map (fun path ->
			IntraCfg.dependency path
		) pathset in
	*)
	let paths_flang = BatSet.map (fun path ->
			Flang.trans_graph path
		) pathset in
	paths_flang

(*EXTRACT for T2.
	Get global that is already unrolled and return a set of unique paths to query in flang.*)
(*
let gen_t2 : Global.t -> query -> Flang.t BatSet.t
= fun global q ->
	let pid_query = fst q.node in
	(*intracfg that has the query*)
	let intracfg_query = BatMap.find pid_query global.icfg.cfgs in
	(*intracfg with the successors of the query removed*)
	let intracfg_query = Training.slice_query_path q intracfg_query in
	(*unique paths in intracfg*)
	let paths_to_query_intracfg = Extractor.get_paths intracfg_query in
	(*dependency*)
	let paths_dependency = BatSet.map (fun path ->
			IntraCfg.dependency path
		)  paths_to_query_intracfg in
	(*translate to flang*)
	let paths_flang = BatSet.map (fun path ->
			Flang.trans_graph path
		) paths_dependency in
	paths_flang
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

(* Build a feature-boolean-vector from the given extracted program and given feature vector. *)
let fbvectorize : Flang.t BatSet.t -> Flang.t list -> fbvector
=fun fpaths features -> 
	List.fold_right (fun f accum ->
			(pred fpaths f) :: accum
		) features []


(* May be used in futere *)
(* Extract literally raw paths from the given single-query program. *)
(*
let extract_raw_paths : Global.t -> (string * Cil.stmt list BatSet.t)
=fun global ->
	let file = global.file in
	let observe_fd = Slicer.find_observe_fundec file in
	let funname = observe_fd.svar.vname in
	let rawTbl = Extractor.build_varTbl observe_fd in
	let raw_paths = Hashtbl.fold (fun key value accum -> 
			BatSet.add value accum) rawTbl BatSet.empty in
	(funname, raw_paths)
*)
