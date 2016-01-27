(**************************************************************
 * Generate dependency graph by reaching definition analysis.	*
 * domain: node																								*
 * our definition of Def: typical def + assume								*
 **************************************************************)

open IntraCfg

module SS = Set.Make(String)

type defsinfo = (string, int BatSet.t) BatMap.t

(*Check if the node has def.*)
let has_def : IntraCfg.t -> IntraCfg.Node.t -> bool
=fun cfg node ->
	let cmd = find_cmd node cfg in
	match cmd with
	| Cset (_, _, _)
	| Cexternal (_, _)
	| Calloc (_, _, _, _)
	| Csalloc (_, _, _)
	| Cfalloc (_, _, _)
	| Cassume (_, _) -> true
	| Ccall (lval_opt, _, _, _) ->
			(match lval_opt with
			 | Some lval -> true
			 | None -> false)
	| _ -> false

(*Get def variables from exp.*)
let rec get_vars_exp : Cil.exp -> SS.t -> SS.t
=fun exp acc ->
	match exp with
	| Lval l -> get_vars_lval l acc
	| UnOp (_, e, _) -> get_vars_exp e acc
	| BinOp (_, e1, e2, _) -> SS.union (get_vars_exp e1 acc) (get_vars_exp e2 acc)
	| CastE (_, e) -> get_vars_exp e acc
	| AddrOf l -> get_vars_lval l acc
	| StartOf l -> get_vars_lval l acc
	| _ -> acc

(*Get def variables from lval.*)
and get_vars_lval : Cil.lval -> SS.t -> SS.t
=fun lval acc -> 
	let (lhost, _) = lval in
	match lhost with
	| Var varinfo -> SS.add varinfo.vname acc
	| Mem exp -> get_vars_exp exp acc

(*Get DEF variables from the given node.*)
let get_defvars : IntraCfg.t -> IntraCfg.Node.t -> SS.t
=fun cfg node ->
	let cmd = find_cmd node cfg in
	match cmd with
	| Cset (lval, _, _)
	| Cexternal (lval, _)
	| Calloc (lval, _, _, _)
	| Csalloc (lval, _, _)
	| Cfalloc (lval, _, _) -> get_vars_lval lval SS.empty
	| Cassume (e, _) -> get_vars_exp e SS.empty
	| Ccall (lval_opt, _, _, _) ->
			(match lval_opt with
			 | Some lval -> get_vars_lval lval SS.empty
			 | None -> SS.empty)
	| _ -> SS.empty

(*Get USE variables from the given node.*)
let get_usevars : IntraCfg.t -> IntraCfg.Node.t -> SS.t
=fun cfg node ->
	let cmd = find_cmd node cfg in
	match cmd with
	| Cset (_, e, _)
	| Cassume (e, _) -> get_vars_exp e SS.empty
	| Calloc (_, alloc, _, _) -> 
			(match alloc with
			 | Array e -> get_vars_exp e SS.empty)
	| Ccall (_, _, e_list, _) ->
			List.fold_right (fun e acc ->
					SS.union (get_vars_exp e SS.empty) acc
				) e_list SS.empty
	| Creturn (e_opt, _) ->
			(match e_opt with
			 | Some e -> get_vars_exp e SS.empty
			 | None -> SS.empty)
	| _ -> SS.empty
	
(*Construct a var-to-defnode_id map from the given cfg.*)
let cal_defsinfo : IntraCfg.t -> defsinfo
=fun cfg ->
	fold_vertex (fun node v2nids_map ->
		let defvars = get_defvars cfg node in
		let updated = SS.fold (fun var map ->
			let bound = try BatMap.find var map with Not_found -> BatSet.empty in
			let added = BatSet.add (Node.getid node) bound in
			BatMap.add var added map) defvars v2nids_map in
		updated) cfg BatMap.empty

(*gen-nid-set of the given node*)
let gen : IntraCfg.t -> IntraCfg.Node.t -> int BatSet.t
=fun cfg node -> 
	if has_def cfg node
	then BatSet.singleton (Node.getid node)
	else BatSet.empty

(*kill-nid-set of the given node*)
let kill : IntraCfg.t -> IntraCfg.Node.t -> defsinfo -> int BatSet.t
=fun cfg node defsinfo ->
	if has_def cfg node then (
			let defvars = get_defvars cfg node in
			let defnids = SS.fold (fun dv acc ->
					BatSet.union (BatMap.find dv defsinfo) acc
				) defvars BatSet.empty in
			BatSet.diff defnids (BatSet.singleton (Node.getid node))
	)
	else BatSet.empty

(*in-nid-set to the given node*)
let rec inn : IntraCfg.t -> IntraCfg.Node.t -> defsinfo -> int BatSet.t
=fun cfg node defsinfo ->
	let preds = pred node cfg in
	let preds = BatSet.of_list preds in
	BatSet.fold (fun pred acc ->
			BatSet.union (out cfg pred defsinfo) acc
		) preds BatSet.empty

(*out-nid-set from the given node*)
and out : IntraCfg.t -> IntraCfg.Node.t -> defsinfo -> int BatSet.t
=fun cfg node defsinfo ->
	let gens = gen cfg node in
	let inns = inn cfg node defsinfo in
	let kills = kill cfg node defsinfo in
	BatSet.union gens (BatSet.diff inns kills)

(*Initialize In & Out to empty set.*)
let init_inn_out : IntraCfg.t -> (int, (int BatSet.t * int BatSet.t)) BatMap.t
=fun cfg ->
	let nodes = nodesof cfg in
	let initial_io = List.fold_right (fun n acc ->
			BatMap.add (Node.getid n) (BatSet.empty, BatSet.empty) acc
		) nodes BatMap.empty in
	initial_io

(*Calculate In & Out by iterating all nodes.*)
let cal_inn_out_one_iteration : IntraCfg.t -> defsinfo -> Node.t list -> (int, (int BatSet.t * int BatSet.t)) BatMap.t -> (int, (int BatSet.t * int BatSet.t)) BatMap.t
=fun cfg defsinfo nodes prev_io_map ->
	List.fold_right (fun n acc ->
			let preds = pred n cfg in
			let ins = List.fold_right (fun pre acc' ->
					let pred_outs_from_previous_iter = snd (BatMap.find (Node.getid pre) prev_io_map) in
					BatSet.union pred_outs_from_previous_iter acc'
				) preds BatSet.empty in
			let gens = gen cfg n in
			let kills = kill cfg n defsinfo in
			let outs = BatSet.union gens (BatSet.diff ins kills) in
			BatMap.add (Node.getid n) (ins, outs) acc
		) nodes prev_io_map

(*Check if we have reached fixpoint of In * Out.*)
let check_io_map_fixpoint : (int, (int BatSet.t * int BatSet.t)) BatMap.t -> (int, (int BatSet.t * int BatSet.t)) BatMap.t -> bool
=fun prev current ->
	BatMap.for_all (fun prev_key prev_bind ->
		try 
			let (curr_fst, curr_snd) = BatMap.find prev_key current in
			let (prev_fst, prev_snd) = prev_bind in
			(BatSet.equal curr_fst prev_fst) && (BatSet.equal curr_snd prev_snd)
		with Not_found -> false) prev

(*Find fixpoint of In & Out.*)
let rec io_map_fixpoint : IntraCfg.t -> defsinfo -> Node.t list -> (int, (int BatSet.t * int BatSet.t)) BatMap.t -> (int, (int BatSet.t * int BatSet.t)) BatMap.t
=fun cfg defsinfo nodes prev_io_map ->
	let new_io_map = cal_inn_out_one_iteration cfg defsinfo nodes prev_io_map in
	if check_io_map_fixpoint prev_io_map new_io_map
	then new_io_map
	else io_map_fixpoint cfg defsinfo nodes new_io_map

(*Calculate In & Out of all nodes in the given cfg.*)
let cal_inn_out : IntraCfg.t -> defsinfo -> (int, (int BatSet.t * int BatSet.t)) BatMap.t
=fun cfg defsinfo ->
	let initial_io_map = init_inn_out cfg in
	let nodes = nodesof cfg in
	let final_io_map = io_map_fixpoint cfg defsinfo nodes initial_io_map in
	final_io_map

(**************************
 * Draw dependency graph.	*
 **************************)

(*Actually connect the two nodes.*)
let connect : IntraCfg.t -> IntraCfg.t -> IntraCfg.Node.t -> IntraCfg.Node.t -> IntraCfg.t
=fun cfg_orig cfg_new n1 n2 ->
	let cfg' = add_node_with_cmd n1 (find_cmd n1 cfg_orig) cfg_new in
	let cfg' = add_node_with_cmd n2 (find_cmd n2 cfg_orig) cfg' in
	let cfg' = add_edge n1 n2 cfg' in
	cfg'

(*Connect all the defs node to the given node, according to its USE variables.*)
let du_connect : IntraCfg.t -> IntraCfg.t -> IntraCfg.Node.t -> int BatSet.t -> IntraCfg.t
=fun cfg_orig cfg_new node reaching_defs ->
	let use_vars = get_usevars cfg_orig node in
	let cfg_after_all_usevars = SS.fold (fun usevar acc ->
			let cfg_after_usevar = BatSet.fold (fun r acc' ->
					let defvars_r = get_defvars cfg_orig (Node r) in
					if SS.mem usevar defvars_r then connect cfg_orig acc' (Node r) node
					else acc'
				) reaching_defs acc in
			cfg_after_usevar
		) use_vars cfg_new in
	cfg_after_all_usevars

(*Connect, for all nodes, from defs to node.*)
let du_connect_all : IntraCfg.t -> (int, int BatSet.t) BatMap.t -> IntraCfg.t
=fun cfg_orig n2reach_map ->
	(*let initial = IntraCfg.empty (Cil.emptyFunction "dummy") in*)
	let initial = IntraCfg.empty (cfg_orig.fd) in
	let initial = add_node Node.ENTRY initial in
	let succ_of_entry = List.nth (succ Node.ENTRY cfg_orig) 0 in
	let initial = add_node_with_cmd succ_of_entry (find_cmd succ_of_entry cfg_orig) initial in
	let initial = add_edge Node.ENTRY succ_of_entry initial in
	(*For each node, connect from defnodes to it.*)
	let nodes = nodesof cfg_orig in
	let dug = List.fold_right (fun n acc ->
			du_connect cfg_orig acc n (BatMap.find (Node.getid n) n2reach_map)
		) nodes initial in
	dug

(*Check if the node has any predecessor.*)
let has_pred : IntraCfg.t -> IntraCfg.Node.t -> bool
=fun cfg node ->
	if List.length (pred node cfg) = 0 then false else true

(*Connect from the ENTRY node to unconnected paths.*)
let from_entry : IntraCfg.t -> IntraCfg.t
=fun cfg ->
	let nodes = nodesof cfg in
	let cfg_entry_connected = List.fold_right (fun n acc ->
			if not (has_pred acc n) then (
					if not (is_entry n) then connect acc acc Node.ENTRY n
					else acc
			)
			else acc
		) nodes cfg in
	cfg_entry_connected

let connect_exit : IntraCfg.t -> IntraCfg.t
=fun cfg ->
	fold_vertex (fun node g ->
		if (List.length (succ node g) = 0)
		then add_edge node Node.EXIT g 
		else g
	) cfg cfg

(*Use this function to get the dependency graph*)
let get_dep_graph : IntraCfg.t -> IntraCfg.t
=fun cfg ->
	if cfg.fd.svar.vname = "_G_" then cfg	(*no dug for _G_*)
	else (
			let defsinfo = cal_defsinfo cfg in
			let final_io_map = cal_inn_out cfg defsinfo in
			let rd_map = BatMap.map (fun io -> fst io) final_io_map in
			du_connect_all cfg rd_map 
			|> from_entry
			|> connect_exit
	)

let get_dep_icfg : InterCfg.t -> InterCfg.t
= fun icfg ->
	prerr_endline ">> DUG start...";
	let dep_cfgs = BatMap.map (fun cfg -> get_dep_graph cfg) icfg.cfgs in
	prerr_endline ">> DUG done";
	{icfg with cfgs = dep_cfgs}

(****************
 * test					*
 ****************)
let test_inn_out : (int, (int BatSet.t * int BatSet.t)) BatMap.t -> unit
=fun final_io_map ->
	BatMap.iter (fun nid (inset, outset) -> 
			prerr_int nid; prerr_endline "";
			prerr_string "IN: ";
			BatSet.iter (fun id -> prerr_int id; prerr_string " ") inset;
			prerr_string "\nOUT: ";
			BatSet.iter (fun id -> prerr_int id; prerr_string " ") outset;
			prerr_endline "\n-----------";
		) final_io_map;
