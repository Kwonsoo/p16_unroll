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
		(*let _ = print_endline ((get_pid cfg) ^ "unrolling_end") in*)
		cfg
		
