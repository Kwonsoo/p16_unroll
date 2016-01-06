open Flang
open Extractor
open Training
open Cil
open Types

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
