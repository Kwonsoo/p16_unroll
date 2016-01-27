open IntraCfg
open Report

module Path = Graph.Path.Check (G)
type branch_map = (node, (node * node)) BatMap.t

let cfg_reachables_only : IntraCfg.t -> Path.path_checker -> IntraCfg.t
= fun g pc ->
	fold_vertex (fun node g ->
		if (Path.check_path pc Node.ENTRY node)
		then g
		else remove_node node g) g g

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

let rec extract_paths : IntraCfg.t -> node -> IntraCfg.t BatSet.t
= fun g node -> 
	let succs = succ node g in
	match List.length succs with
	| 0 ->
		let _ = assert (node = Node.EXIT) in
		BatSet.singleton g
	| 1 ->
		extract_paths g (List.hd succs)
	| _ ->
		List.fold_left (fun acc succ ->
			let g_cut_all = List.fold_left (fun g succ ->
				remove_edge node succ g) g succs in
			let g_add = add_edge node succ g_cut_all in
			let paths = extract_paths g_add succ in
			BatSet.union paths acc) BatSet.empty succs

let get_paths : IntraCfg.t -> IntraCfg.t BatSet.t
= fun g ->
	let branches = get_branch_map g in
	let _ = print_endline ((IntraCfg.get_pid g) ^ (string_of_int (BatMap.cardinal branches))) in
	extract_paths g Node.ENTRY

