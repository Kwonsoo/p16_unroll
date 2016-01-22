open IntraCfg
open Report

type branch_map = (node, (node * node)) BatMap.t

let card_succs : IntraCfg.t -> node -> int
= fun g node -> List.length (succ node g)

let remove_unreaches : IntraCfg.t -> IntraCfg.t
= fun g ->
	let unreaches = IntraCfg.unreachable_node g in
	BatSet.fold (fun node acc ->
		remove_node node acc) unreaches g 

let get_branch_map : IntraCfg.t -> branch_map
= fun g ->
	fold_vertex (fun node acc -> 
		if card_succs g node = 2
		then
			let left = List.nth (succ node g) 0 in
			let right = List.nth (succ node g) 1 in 
			BatMap.add node (left, right) acc
		else 
			acc) g BatMap.empty

let rec extract_paths : IntraCfg.t -> branch_map -> IntraCfg.t BatSet.t
= fun g branch_map -> 
	match BatMap.cardinal branch_map with
	| 0 -> 
		let clean_g = remove_unreaches g in
		BatSet.singleton clean_g
	| _ ->
		let (branch, (left, right)), map_rest = BatMap.pop branch_map in
		let g_left = remove_edge branch left g in
		let g_right = remove_edge branch right g in
		let left_side = extract_paths g_left map_rest in
		let right_side = extract_paths g_right map_rest in
		BatSet.union left_side right_side

let get_paths : IntraCfg.t -> IntraCfg.t BatSet.t
= fun g ->
	let branches = get_branch_map g in
	extract_paths g branches

