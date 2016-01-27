open Types
open Report
type pid = InterCfg.pid
open InterCfg
open IntraCfg

module Slicer =
struct
	open Cil

	let get_fundecs : Cil.file -> Cil.fundec list
	= fun file ->
		List.fold_left (fun acc glob ->
			match glob with
			| Cil.GFun (fd, _) -> fd::acc
			| _ -> acc
		) [] file.globals

	let is_var : Cil.lval -> bool
	= fun (lhost, _) ->
		match lhost with
		| Cil.Var vinfo -> true
		| _ -> false
	
	let is_observe_call : Cil.instr -> bool
	= fun instr ->
		match instr with
		| Cil.Call (ret, func, args, _) ->
				(match func with
				| Cil.Lval lv when is_var lv ->
						let (Cil.Var vinfo, _) = lv in 						
						vinfo.vname = "airac_observe"
				| _ -> prerr_endline "nonono"; false)
		| _ -> false

	let has_observer : Cil.fundec -> bool
	= fun fd ->
		let stmts = fd.sallstmts in
		List.exists (fun stmt ->
			match stmt.skind with
			| Cil.Instr il ->
					List.exists is_observe_call il
			| _ -> false
		) stmts
	(*		
	let find_observe_fundec : Cil.file -> Cil.fundec
	= fun file ->
		let fundecs = get_fundecs file in
		List.find has_observer fundecs
	*)

	(* Return a intracfg that has airac_observe from the given intercfg. *)
	let find_observe_intracfg : InterCfg.t -> IntraCfg.t
	=fun inter ->
		let cfgs = inter.cfgs in
		let cfgs' = BatMap.filter (fun pid intracfg -> 
				prerr_string ("PID: " ^ pid ^ " -> ");
				let fd = intracfg.fd in
				let has = has_observer fd in
				let has_print = if has then "Yeah" else "no" in
				prerr_endline has_print;
				has
			) cfgs in
		prerr_int (BatMap.cardinal cfgs');
		if BatMap.cardinal cfgs' = 1
		then (
				let (pid, intra) = BatMap.choose cfgs' in
				intra
		)
		else raise (Failure "Slicer.find_observe_intracfg: airac_observe should be one and only one.")
		
end

let filter_normal_queries : query list -> query list
= fun qs ->
	let queries = List.filter (fun q -> 
		let pid = Report.get_pid q in
		pid <> "_G_" ) qs in
	List.filter (fun q -> q.status <> BotAlarm) queries 
	
let cluster_queries_with_pid : query list -> (pid, query list) BatMap.t
= fun queries ->
	List.fold_left (fun map q ->
		let pid = Report.get_pid q in
		match BatMap.mem pid map with
		| true ->
			let found = BatMap.find pid map in
			let qs = q::found in
			BatMap.add pid qs map
		| false ->
			BatMap.add pid [q] map) BatMap.empty queries

let is_query_node : IntraCfg.t -> query -> IntraCfg.node -> bool
= fun cfg q node ->
	let nid_string = string_of_int (Unroller.nidof_q q) in
	let cmd = IntraCfg.find_cmd node cfg in
	match cmd with
	| IntraCfg.Cmd.Ccall (None, Cil.Lval (Cil.Var vinfo, Cil.NoOffset), exps, _) when vinfo.vname = "airac_nid" ->
		let _ = assert (List.length exps = 1) in
		let call_arg = List.hd exps in
		let call_nid = Unroller.nid_from_arg_exp call_arg in
		call_nid = nid_string
	| _ -> false

let has_query_node : query -> IntraCfg.t -> bool
= fun query cfg ->
	let nodes = IntraCfg.nodesof cfg in
	List.exists (is_query_node cfg query) nodes

let slice_query_path : query -> IntraCfg.t -> IntraCfg.t
= fun q cfg ->
	let nodes = IntraCfg.nodesof cfg in
	let q_node = List.find (is_query_node cfg q) nodes in
	let q_next = List.hd (IntraCfg.succ q_node cfg) in
	let cfg_deleted = IntraCfg.remove_node q_next cfg in
	let unreaches = IntraCfg.unreachable_node cfg_deleted in
	BatSet.fold (fun node acc ->
		IntraCfg.remove_node node acc) unreaches cfg_deleted

let get_query_depend_paths : IntraCfg.t BatSet.t -> query -> IntraCfg.t BatSet.t
= fun cfgs query ->
	let paths_have_query = BatSet.filter (has_query_node query) cfgs in
	BatSet.map (slice_query_path query) paths_have_query

(* Use this function to get paths of a query *)
let get_query_to_paths_map : InterCfg.t -> query list -> (query, IntraCfg.t BatSet.t) BatMap.t
= fun icfg queries ->
	let pid2qs_map = cluster_queries_with_pid queries in
	let paths2qs_map = BatMap.foldi (fun pid qs acc ->
		let paths = InterCfg.cfgof icfg pid |> Extractor.get_paths in
		BatMap.add paths qs acc) pid2qs_map BatMap.empty in
	let query_to_paths_map = BatMap.foldi (fun paths qs acc ->
		let map_from_each_paths = List.fold_left (fun acc q ->
			let query_dep_paths = get_query_depend_paths paths q in	
			BatMap.add q query_dep_paths acc) BatMap.empty qs in
		BatMap.union map_from_each_paths acc) paths2qs_map BatMap.empty in
	query_to_paths_map	


