open Flang
open Extractor
open Training

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

(* Extract literally raw paths from the given single-query program. *)
let extract_raw_paths : Global.t -> Cil.stmt list BatSet.t
=fun global ->
	let file = global.file in
	let observe_fd = Slicer.find_observe_fundec file in
	let rawTbl = Extractor.build_varTbl observe_fd in
	Hashtbl.fold (fun key value accum -> 
		BatSet.add value accum) rawTbl BatSet.empty
	
	

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
