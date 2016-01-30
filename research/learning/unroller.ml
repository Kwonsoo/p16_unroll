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
	let tail = try
		List.find (fun node -> List.mem node scc) (pred loop_head cfg)
		with _ -> raise (Failure "Recon.get_loop_nodes: fatal") in
	let loop_break = find_loop_break cfg tail loop_head in
	(tail, loop_head, loop_break)

(*-----------------------------*)
let connect_two_nodes : IntraCfg.t -> Node.t -> Cmd.t -> Node.t -> Cmd.t -> IntraCfg.t
=fun cfg n1 cmd1 n2 cmd2 -?
	let cfg' = add_node_with_cmd n1 cmd1 cfg in
	let cfg' = add_node_with_cmd n2 cmd2 cfg' in
	let cfg' = add_edge n1 n2 cfg' in
	cfg'

let rec copy_scc : IntraCfg.t -> Node.t -> IntraCfg.t
=fun cfg curnode loophead ->
	let succs = succ curnode cfg in
	if List.mem loophead succs then connect_two_nodes cfg (curnode) (find_cmd curnode cfg) (loophead) (find_cmd loophead cfg)
	else (
			let cfg' = List.fold_right (fun suc acc -> 
					let cfg'' = connect_two_nodes acc () () () () in
					copy_scc cfg'' suc loophead
				) succs cfg in
			cfg'
	)

(*Copy loop-head and the next node of loop-head in SCC and connect them together.*)
let copy_loophead : IntraCfg.t -> Node.t -> IntraCfg.t
=fun cfg loophead scc node_next_to_lhead ->
	connect_two_nodes cfg (Node.make ()) (find_cmd loophead cfg) (Node.make ()) (find_cmd node_next_to_lhead cfg) in

(**)
let copy_loop : IntraCfg.t -> Node.t -> Node.t -> Node.t -> scc -> IntraCfg.t
=fun cfg looptail loophead looppbreak scc ->
	if List.length (succ loophead cfg) < 2 then raise (Failure "copy_loop: loophead succs less than 2")
	else (
			let node_next_to_lhead_in_scc = List.find (fun s -> List.mem s scc) (scc loophead cfg) in
			let cfg_lh_copied = copy_loophead cfg loophead node_next_to_lhead_in_scc in
			let cfg_scc_copied = copy_scc cfg_lh_copied node_next_to_lhead_in_scc loophead in

	)
			
			


(*
let copy_loop : IntraCfg.t -> Node.t -> Node.t -> Node.t -> IntraCfg.t
=fun cfg looptail loophead loopbreak ->
	if List.length (succ loophead cfg) < 2 then raise (Failure "copy_loop: loop-head successors less than 2")
	else (

			let loopin =
					if Node.equal (List.nth (succ loophead cfg) 0) loopbreak
					then List.nth (succ loophead cfg) 1 
					else List.nth (succ loophead cfg) 0 in
			let loop_initial = IntraCfg.empty (Cil.emptyFunction "dummy") in	(*empty*)
			let loop_initial =  in	(*loop-head*)
			
			let loophead_new = Node.make () in
			let loopbreak_new = Node.make () in
			let loopin_new = Node.make () in
			let looptail_new = Node.make () in
			
			let cfg_new = connect_two_nodes cfg cfg loophead_new (find_cmd loophead cfg) loopbreak_new (find_cmd loopbreak cfg) in	(*loophead -> loopbreak*)
			let cfg_new = connect_two_nodes cfg cfg_new loophead_new (find_cmd loophead cfg) loopin_new (find_cmd loopin cfg) in	(*loophead -> loopin*)
			let cfg_new = connect_two_nodes cfg cfg_new looptail_new (find_cmd loophead cfg) loophead_new (find_cmd loophead cfg) in	(*backedge_from -> loophead*)
			let cfg_new = connect_cycle cfg cfg_new loophead_new looptail in	(*connect cycle backards from backedge_from to loophead*)
			(*Connect separate two loops.*)
			let cfg_new = connect_two_loops cfg cfg_new looptail loophead loopbreak looptail_new loophead_new loopbreak_new in
			cfg_new
	)
*)

let reconstruct : IntraCfg.t -> loop_nodes -> scc -> IntraCfg.t
= fun cfg (tail, head, break) scc ->
	(*
	remove_edge tail head cfg
	|> add_edge tail break
	*)

let rec unroll_cfg : IntraCfg.t -> IntraCfg.t
= fun cfg ->
	let sccs = cfg.scc_list in
	if (List.exists (fun scc -> List.length scc > 1) sccs)
	then
		let cfg = compute_scc cfg |> compute_dom in
		let sccs = cfg.scc_list in
		let updated = List.fold_left (fun cfg scc ->
			if List.length scc < 2 then cfg 
			else
				let loop_nodes = get_loop_nodes cfg scc in
				reconstruct cfg loop_nodes scc) cfg sccs
		in unroll_cfg updated
	else
		cfg

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
		unroll_cfg cfg) cfgs in
	let _ = print_endline (">> Unrolling is done") in 
	{global.icfg with cfgs = unrolled_cfgs}


(*-----------------------------------------------------------------------------------------------*)

(*
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

(*
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

*)
