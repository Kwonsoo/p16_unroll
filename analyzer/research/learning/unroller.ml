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
	
let reconstruct_cfg : IntraCfg.t -> loop_nodes -> IntraCfg.t
= fun cfg (tail, head, break) ->
	remove_edge tail head cfg
	|> add_edge tail break

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
				reconstruct_cfg cfg loop_nodes) cfg sccs
		in unroll_cfg updated
	else
		cfg
		
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

(* TO DO *)
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
