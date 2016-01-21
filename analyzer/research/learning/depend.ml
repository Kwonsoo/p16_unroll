(*Generate dependency graph by reaching definition analysis.
	domain: node*)

open IntraCfg

module SS = Set.Make(String)

type defsinfo = (string, int BatSet.t) BatMap.t

(*TODO*)
(*Construct a var-to-defnode_id map from the given cfg.*)
let cal_defsinfo : IntraCfg.t -> defsinfo
=fun cfg -> BatMap.empty

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
let rec get_defvars_exp : Cil.exp -> SS.t -> SS.t
=fun exp acc ->
	match exp with
	| Lval l -> get_defvars_lval l acc
	| UnOp (_, e, _) -> get_defvars_exp e acc
	| BinOp (_, e1, e2, _) -> SS.union (get_defvars_exp e1 acc) (get_defvars_exp e2 acc)
	| CastE (_, e) -> get_defvars_exp e acc
	| AddrOf l -> get_defvars_lval l acc
	| StartOf l -> get_defvars_lval l acc
	| _ -> acc

(*Get def variables from lval.*)
and get_defvars_lval : Cil.lval -> SS.t -> SS.t
=fun lval acc -> 
	let (lhost, _) = lval in
	match lhost with
	| Var varinfo -> SS.add varinfo.vname acc
	| Mem exp -> get_defvars_exp exp acc

(*Get def variables from the given node.*)
let get_defvars : IntraCfg.t -> IntraCfg.Node.t -> SS.t
=fun cfg node ->
	let cmd = find_cmd node cfg in
	match cmd with
	| Cset (lval, _, _)
	| Cexternal (lval, _)
	| Calloc (lval, _, _, _)
	| Csalloc (lval, _, _)
	| Cfalloc (lval, _, _) -> get_defvars_lval lval SS.empty
	| Ccall (lval_opt, _, _, _) ->
			(match lval_opt with
			 | Some lval -> get_defvars_lval lval SS.empty
			 | None -> SS.empty)
	| _ -> SS.empty

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

(**)
