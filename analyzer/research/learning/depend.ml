(**************************************************************
 * Generate dependency graph by reaching definition analysis.	*
 * domain: node																								*
 * Our definition of Def: typical def + assume								*
 **************************************************************)

open IntraCfg

module SS = Set.Make(String)

type defsinfo = (string, int BatSet.t) BatMap.t

(*Check if the node has def.*)
let has_def : IntraCfg.t -> IntraCfg.Node.t -> bool
=fun cfg node ->
	let cmd = find_cmd node cfg in
	match cmd with
	| Cset (lval, _, _)
	| Cexternal (lval, _)
	| Calloc (lval, _, _, _)
	| Csalloc (lval, _, _)
	| Cfalloc (lval, _, _) -> true
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

(*Get DEF variables from the given node.
	NOTE: Our DEF includes assume.*)
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

(**************************
 * Draw dependency graph	*
 **************************)

(*Actually connect the two nodes.*)
let connect : IntraCfg.t -> IntraCfg.Node.t -> IntraCfg.Node.t -> IntraCfg.t
=fun cfg n1 n2 ->
	let cfg' = add_node n1 cfg in
	let cfg' = add_node n2 cfg' in
	let cfg' = add_edge n1 n2 cfg' in
	cfg'

(*Connect all the defs node to the given node, according to its USE variables.*)
let du_connect : IntraCfg.t -> IntraCfg.t -> IntraCfg.Node.t -> int BatSet.t -> IntraCfg.t
=fun cfg_orig cfg_new node reaching_defs ->
	let use_vars = get_usevars cfg_orig node in
	let cfg_after_all_usevars = SS.fold (fun usevar acc ->
			let cfg_after_usevar = BatSet.fold (fun r acc' ->
					let defvars_r = get_defvars cfg_orig (Node r) in
					if SS.mem usevar defvars_r then connect acc' node (Node r)
					else acc'
				) reaching_defs acc in
			cfg_after_usevar
		) use_vars cfg_new in
	cfg_after_all_usevars

(*Check if the node has any predecessor.*)
let has_pred : IntraCfg.t -> IntraCfg.Node.t -> bool
=fun cfg node ->
	if List.length (pred node cfg) = 0 then false else true

(*Connect from the ENTRY node to unconnected paths.*)
let from_entry : IntraCfg.t -> IntraCfg.t
=fun cfg ->
	let nodes = nodesof cfg in
	let cfg_entry_connected = List.fold_right (fun n acc ->
			if not has_pred acc n then (
					if not is_entry n then connect acc Node.ENTRY n
					else acc
			)
			else acc
		) nodes cfg in
	cfg_entry_connected
