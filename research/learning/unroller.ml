open Cil
open Visitors
open Report
open IntraCfg

module Path = Graph.Path.Check (G)
type scc = node list
type loop_nodes = node * node * node

let get_all_doms : Path.path_checker -> G.t -> node -> node BatSet.t
= fun pc tree head ->
	G.fold_vertex (fun node sdoms ->
		if Path.check_path pc head node
		then BatSet.add node sdoms
		else sdoms) tree BatSet.empty
		
let find_loop_head : IntraCfg.t -> scc -> node
= fun g scc ->
	let head = List.find (fun node ->
		let preds = pred node g in
		List.exists (fun pred -> (not (List.mem pred scc))) preds
		) scc in
	head

let find_loop_break : IntraCfg.t -> node -> node -> node
= fun g tail head ->
	let dom_tree = get_dom_tree g in
	let pc = Path.create dom_tree in
	
	let rec proceed_succs node =
		let succs = succ node g in
		match List.length succs with
		| 1 -> proceed_succs (List.hd succs)
		| 2 ->
			List.find (fun succ ->
				let doms = get_all_doms pc dom_tree succ in
				not (BatSet.mem tail doms)) succs
		| _ -> raise (Failure "Recon.find_loop_break: fatal")
	in proceed_succs head

let get_loop_nodes : IntraCfg.t -> scc -> loop_nodes
= fun cfg scc ->
	let loop_head = find_loop_head cfg scc in
	prerr_endline "---------";
	prerr_int (Node.getid loop_head);
	prerr_endline "\n--------";
	let tail = try
		List.find (fun node -> List.mem node scc) (pred loop_head cfg)
		with _ -> raise (Failure "Recon.get_loop_nodes: fatal") in
	let loop_break = find_loop_break cfg tail loop_head in
	(tail, loop_head, loop_break)

(*-----------------------------*)
let connect_two_nodes : IntraCfg.t -> Node.t -> Cmd.t -> Node.t -> Cmd.t -> IntraCfg.t
=fun cfg n1 cmd1 n2 cmd2 ->
	let cfg' = add_node_with_cmd n1 cmd1 cfg in
	let cfg' = add_node_with_cmd n2 cmd2 cfg' in
	let cfg' = add_edge n1 n2 cfg' in
	cfg'

let connect_original_and_copied_scc : IntraCfg.t -> (Node.t, Node.t) BatMap.t -> Node.t -> Node.t -> Node.t -> IntraCfg.t
=fun cfg sccorig_sccnew_map loophead looptail loopbreak ->
	let cfg' = remove_edge looptail loophead cfg in
	let cfg' = remove_edge (BatMap.find looptail sccorig_sccnew_map) (BatMap.find loophead sccorig_sccnew_map) cfg' in
	let cfg' = add_edge looptail (BatMap.find loophead sccorig_sccnew_map) cfg' in
	add_edge (BatMap.find looptail sccorig_sccnew_map) (List.nth (succ loopbreak cfg) 0) cfg'

let copy_scc : IntraCfg.t -> IntraCfg.t -> scc -> (Node.t, Node.t) BatMap.t -> IntraCfg.t
=fun cfg_orig cfg_updating scc sccorig_sccnew_map ->
	List.fold_right (fun n acc ->
			let n_new = BatMap.find n sccorig_sccnew_map in
			let n_new_cmd = find_cmd n cfg_orig in
			let succs = succ n cfg_orig in
			List.fold_right (fun s acc' ->
					try connect_two_nodes acc' (n_new) (n_new_cmd) (BatMap.find s sccorig_sccnew_map) (find_cmd s cfg_orig) with _ -> acc'
				) succs acc
		) scc cfg_updating

let copy_lbreak_and_connect2 : IntraCfg.t -> IntraCfg.t -> (Node.t, Node.t) BatMap.t -> Node.t -> Node.t -> IntraCfg.t
=fun cfg_orig cfg_updating sccorig_sccnew_map loophead loopbreak ->
	let loopbreak_new = Node.make () in
	let cfg' = add_node_with_cmd (loopbreak_new) (find_cmd loopbreak cfg_orig) cfg_updating in
	let cfg' = add_edge (BatMap.find loophead sccorig_sccnew_map) (loopbreak_new) cfg' in
	let cfg' = add_edge (loopbreak_new) (List.nth (succ loopbreak cfg') 0) cfg' in
	cfg'

let copy_loop : IntraCfg.t -> Node.t -> Node.t -> Node.t -> scc -> IntraCfg.t
=fun cfg looptail loophead loopbreak scc ->
	let sccorig_sccnew_map = List.fold_right (fun n acc ->
			let new_node = Node.make () in
			BatMap.add n new_node acc
		) scc BatMap.empty in
	let cfg_scc_copied = copy_scc cfg cfg scc sccorig_sccnew_map in
	let cfg_scc_connected = connect_original_and_copied_scc cfg_scc_copied sccorig_sccnew_map loophead looptail loopbreak in
	let cfg_done = copy_lbreak_and_connect2 cfg cfg_scc_connected sccorig_sccnew_map loophead loopbreak in
	cfg_done
	
(*
let reconstruct : IntraCfg.t -> loop_nodes -> scc -> IntraCfg.t
= fun cfg (tail, head, break) scc ->
	remove_edge tail head cfg
	|> add_edge tail break
*)

let reconstruct : IntraCfg.t -> loop_nodes -> scc -> IntraCfg.t
=fun cfg (tail, head, break) scc ->
	copy_loop cfg tail head break scc

let rec unroll_cfg : IntraCfg.t -> scc list -> IntraCfg.t 
=fun cfg sccs ->
	if cfg.fd.svar.vname = "_G_" then cfg
	else (
			(*
			let sccs = List.filter (fun scc -> (List.length scc) > 1) sccs in
			if List.length sccs = 0 then (prerr_endline "tete"; cfg)*)
			if not (List.exists (fun scc -> List.length scc > 1) sccs) then (cfg)
			else (
					prerr_int (List.length sccs);prerr_endline "";
					let one_scc_longer_than1 = List.find (fun scc -> List.length scc > 1) sccs in	(*one SCC means one loop.*)
					let loop_nodes = get_loop_nodes cfg one_scc_longer_than1 in
					let cfg' = reconstruct cfg loop_nodes one_scc_longer_than1 in	(*unroll*)
					let cfg' = compute_scc cfg' |> compute_dom in			(*recompute SCC and Dom*)
					let sccs' = cfg'.scc_list in
					unroll_cfg cfg' sccs'
			)
	)

let get_unrolled_icfg : Global.t -> InterCfg.t
= fun global ->
	let cfgs = global.icfg.cfgs in
	let _ = print_endline ("\nProgram	: " ^ global.file.fileName) in
	let _ = print_endline (">> Unrolling begins ...") in
	let _ = print_endline ("#_pids	: " ^ (string_of_int (BatMap.cardinal cfgs))) in
	let idx = ref 0 in
	let unrolled_cfgs = BatMap.map (fun cfg -> 
		let _ = idx := !idx + 1 in
		let status = "(" ^ (string_of_int !idx) ^ " of " ^ (string_of_int (BatMap.cardinal cfgs)) ^ ")" in
		let _ = print_endline ((get_pid cfg) ^ " ... " ^ status) in
		unroll_cfg cfg (cfg.scc_list)) cfgs in
	let _ = print_endline (">> Unrolling is done") in 
	{global.icfg with cfgs = unrolled_cfgs}


(*================================================================================*)

(* Use this function only to extract the nid from airac_nid *)
let nid_from_arg_exp : Cil.exp -> string
= fun e ->
	match e with
	| Cil.Lval lv ->
		(match lv with
		| (lh, _) -> 
			(match lh with
			| Cil.Var v -> v.vname
			| _ -> assert false)
		| _ -> assert false)
	| _ -> assert false

let nidof_q : Report.query -> int
= fun q -> 
	let intra_node = InterCfg.Node.get_cfgnode q.node in
	IntraCfg.Node.getid intra_node

let query2nid query =
	let nid = nidof_q query in
	let vinfo = Cil.makeGlobalVar (string_of_int nid) (TVoid []) in
	[Cil.Lval (Cil.Var vinfo, NoOffset)]

let gen_airac_nid query =
	let loc = query.loc in
	let var = Cil.makeGlobalVar "airac_nid" (TFun (TVoid [], Some [], true, [])) in
	let airac_nid = Cil.Lval (Cil.Var var, Cil.NoOffset) in
	let nid_arg = query2nid query in
	Call (None, airac_nid, nid_arg, loc)

let is_break_stmt : Cil.stmt -> bool
= fun s ->
	let labels = s.labels in
	List.exists (fun label -> 
		match label with
		| Label (name, _, flag) when flag = false -> 
			BatString.exists name "while_break"
		| _ -> false) labels

let unroll_body : Cil.fundec -> Cil.block -> int -> Cil.block
= fun fd body k ->
	match k with
	| 0 -> body
	| _ -> body

class unrollingVisitor (fd, k) = object(self)
	inherit nopCilVisitor
	method vstmt (s : stmt) =
		match s.skind with
		| Goto (target, _) when not (is_break_stmt (!target)) ->
			let skind = Instr [] in
			ChangeTo {labels = []; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds;}
		| Loop (body, _, _, _) ->
			let unrolled = unroll_body fd body k in
			let skind = Block unrolled in
			let body = {labels = []; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds;} in
			ChangeDoChildrenPost (body, (fun s -> s))
		| _ -> DoChildren
end

class insertNidVisitor (query) = object(self)
	inherit nopCilVisitor

	val alarm_exp = query.exp

	method vinst (i : instr) =
		match i with
		| Set (lhs, rhs, loc) when (found (rhs, loc, alarm_exp) || found (Lval lhs, loc, alarm_exp))
			-> ChangeTo [Set (lhs, rhs, loc ); gen_airac_nid query]
		| Call (ret, fexp, args, loc) when (found (fexp, loc, alarm_exp) || found_s (args, loc, alarm_exp))
			-> ChangeTo [Call (ret, fexp, args, loc); gen_airac_nid query]
		| _ -> DoChildren

	method vstmt (s : stmt) = 
    	match s.skind with 
    	| If (e, b1, b2, loc) when found (e, loc, alarm_exp) ->
        	let nid_stmt = {labels=[]; skind = Instr [gen_airac_nid query]; sid=0; succs=[]; preds=[]} in
        	let skind = Block { battrs = []; bstmts = [s;nid_stmt] } in
        	ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    	| Return (Some exp, loc) when found (exp, loc, alarm_exp) ->
        	let nid_stmt = {labels=[]; skind = Instr [gen_airac_nid query]; sid=0; succs=[]; preds=[]} in
        	let skind = Block { battrs = []; bstmts = [s;nid_stmt] } in
        	ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    	| Switch (exp, _,_,loc) when found (exp, loc, alarm_exp) ->
        	let nid_stmt = {labels=[]; skind = Instr [gen_airac_nid query]; sid=0; succs=[]; preds=[]} in
        	let skind = Block { battrs = []; bstmts = [s;nid_stmt] } in
        	ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    	| _ -> DoChildren 
end
