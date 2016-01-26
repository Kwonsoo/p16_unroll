open IntraCfg
open Cil

(********************************************************************************************
 * Unroll loops - dominator/back-edge/natural-loop analysis																												*
 * domain: Node																																							*
 * reference 1: https://www.cs.princeton.edu/courses/archive/spring03/cs320/notes/loops.pdf	*
 * reference 2: http://www.cs.cornell.edu/courses/cs412/2008sp/lectures/lec29.pdf						*
 ********************************************************************************************)

(**********************
 * dominator analysis	*
 **********************)
type dom = (Node.t, Node.t BatSet.t) BatMap.t

let dom_init : IntraCfg.t -> dom
=fun cfg ->
	let initial_dom = BatMap.add Node.ENTRY (BatSet.singleton Node.ENTRY) BatMap.empty in
	let nodes = nodesof cfg in
	let nodes = BatSet.of_list nodes in
	let initial_dom = BatSet.fold (fun n acc ->
			if not (Node.equal n Node.ENTRY)
			then BatMap.add n nodes acc
			else acc
		) nodes initial_dom in
	initial_dom

let check_dom_fixpoint : dom -> dom -> bool
=fun d1 d2 ->
	BatMap.for_all (fun n doms ->
			try BatSet.equal doms (BatMap.find n d2) with Not_found -> false
		) d1

let rec find : IntraCfg.t -> dom -> dom -> dom
=fun cfg d1 d2 ->
	if check_dom_fixpoint d1 d2
	then d2
	else (
			let nodes = nodesof cfg in
			let dom_new = List.fold_right (fun n acc ->
					let preds = pred n cfg in
					let preds_doms_intersection = List.fold_right (fun p acc' ->
							let pred_doms = try BatMap.find p d2 with Not_found -> BatSet.empty in
							BatSet.intersect pred_doms acc'
						) preds BatSet.empty in
					let new_doms_of_node = BatSet.union (BatSet.singleton n) preds_doms_intersection in
					BatMap.add n new_doms_of_node acc
				) nodes d2 in
			find cfg d2 dom_new
	)

let find_dominator : IntraCfg.t -> dom
=fun cfg -> 
	let initial = dom_init cfg in
	let final = find cfg BatMap.empty initial in
	final

(********************************
 * immediate dominator analysis	*
 ********************************)
type idom = (Node.t, Node.t) BatMap.t

(*
let find_immediate_dom : IntraCfg.t -> idom
=fun cfg -> BatMap.empty
*)

(**********************
 * back-edge analysis	*
 **********************)

let back_edges : IntraCfg.t -> dom -> (Node.t * Node.t) BatSet.t
=fun cfg dominfo ->
	let nodes = nodesof cfg in
	let bedges = List.fold_right (fun n acc ->
			let successors = succ n cfg in
			let bedges_after_node = List.fold_right (fun suc acc' ->
					let doms_of_n = BatMap.find n dominfo in
					if BatSet.mem suc doms_of_n
					then BatSet.add (n, suc) acc'
					else acc'
				) successors acc in
			bedges_after_node
		) nodes BatSet.empty in
	bedges

let rec get_all_nodes_cycle : IntraCfg.t -> (Node.t * Node.t) -> Node.t -> Node.t BatSet.t -> Node.t BatSet.t
=fun cfg back_edge curnode nodes_visited ->
	let preds = pred curnode cfg in
	let visited_updated = List.fold_right (fun p acc ->
			let visited_after_p = BatSet.add p acc in
			get_all_nodes_cycle cfg back_edge p visited_after_p
		) preds nodes_visited in
	visited_updated

let get_loopout_node : IntraCfg.t -> (Node.t * Node.t) -> Node.t
=fun cfg back_edge ->
	let nodes_in_cycle = BatSet.add (fst back_edge) BatSet.empty in	(*backedge_from*)
	let nodes_in_cycle = BatSet.add (snd back_edge) nodes_in_cycle in	(*loophead*)
	let nodes_in_cycle = get_all_nodes_cycle cfg back_edge (fst back_edge) nodes_in_cycle in
	let loophead = snd back_edge in
	prerr_endline "-----";
	prerr_int (List.length (succ loophead cfg));
	prerr_endline "\n-----";
	if List.length (succ loophead cfg) <> 2 then raise (Failure "get_loopout_node: succ size of loophead is not 2.")
	else (
			if BatSet.mem (List.nth (succ loophead cfg) 0) nodes_in_cycle
			then List.nth (succ loophead cfg) 1
			else List.nth (succ loophead cfg) 0
	)

(****************************************
 * natural loop construction.						*
 * A back-edge defines a natural loop.	*
 ****************************************)

(*Connect two nodes, given the node/cmd information.*)
let connect_two_nodes : IntraCfg.t -> IntraCfg.t -> IntraCfg.Node.t -> IntraCfg.Cmd.t -> IntraCfg.Node.t -> IntraCfg.Cmd.t -> IntraCfg.t
=fun cfg cfg_new n1 cmd1 n2 cmd2 ->
	let cfg' = add_node_with_cmd n1 cmd1 cfg_new in
	let cfg' = add_node_with_cmd n2 cmd2 cfg' in
	let cfg' = add_edge n1 n2 cfg' in
	cfg'

(*Draw a new loop part, separately.*)
let rec connect_cycle : IntraCfg.t -> IntraCfg.t -> Node.t -> Node.t -> IntraCfg.t
=fun cfg cfg_new loophead curnode ->
	if Node.equal curnode loophead then cfg_new
	else (
			let curnode_new = Node.make () in
			let curnode_new_cmd = find_cmd curnode cfg in
			let preds = pred curnode cfg in
			let cfg' = List.fold_right (fun p acc ->
					let p_new = Node.make () in
					let p_new_cmd = find_cmd p cfg in
					let cfg_pre2cur_connected = connect_two_nodes cfg acc p_new p_new_cmd curnode_new curnode_new_cmd in
					connect_cycle cfg cfg_pre2cur_connected loophead p
				) preds cfg_new in
			cfg'
	)

(*Conncet two loop parts, after drawing separate new copied loop.*)
let connect_two_loops : IntraCfg.t -> IntraCfg.t -> Node.t -> Node.t -> Node.t -> Node.t -> Node.t -> Node.t -> IntraCfg.t
=fun cfg cfg_new orig_backedge_from orig_loophead orig_loopout new_backedge_from new_loophead new_loopout ->
	let cfg' = add_edge (orig_backedge_from) (new_loophead) cfg_new in
	let cfg' = add_edge (new_backedge_from) (List.nth (succ orig_loopout cfg) 0) cfg' in
	cfg'

(*Make a new cfg with the new copied loop added and connected.*)
let copy_loop : IntraCfg.t -> (Node.t * Node.t) -> Node.t -> IntraCfg.t
=fun cfg back_edge loopout ->
	let loophead = snd back_edge in
	if List.length (succ loophead cfg) < 2 then raise (Failure "copy_loop: loop-head successors less than 2")
	else (
			let loopin =
					if Node.equal (List.nth (succ loophead cfg) 0) loopout 
					then List.nth (succ loophead cfg) 1 
					else List.nth (succ loophead cfg) 0 in
			let loophead_new = Node.make () in
			let loopout_new = Node.make () in
			let loopin_new = Node.make () in
			let new_backedge_from = Node.make () in
			let cfg_new = connect_two_nodes cfg cfg loophead_new (find_cmd loophead cfg) loopout_new (find_cmd loopout cfg) in	(*loophead -> loopout*)
			let cfg_new = connect_two_nodes cfg cfg_new loophead_new (find_cmd loophead cfg) loopin_new (find_cmd loopin cfg) in	(*loophead -> loopin*)
			let cfg_new = connect_two_nodes cfg cfg_new new_backedge_from (find_cmd (snd back_edge) cfg) loophead_new (find_cmd loophead cfg) in	(*backedge_from -> loophead*)
			let cfg_new = connect_cycle cfg cfg_new loophead_new (fst back_edge) in	(*connect cycle backards from backedge_from to loophead*)
			(*Connect separate two loops.*)
			let cfg_new = connect_two_loops cfg cfg_new (fst back_edge) (snd back_edge) loopout new_backedge_from loophead_new loopout_new in
			cfg_new
	)

let unroll : IntraCfg.t -> IntraCfg.t
=fun cfg ->
	let doms = find_dominator cfg in
	prerr_endline "dominators:";
	BatMap.iter (fun n domset ->
			prerr_int (Node.getid n); prerr_endline "";
			prerr_string "doms: ";
			BatSet.iter (fun dom ->
					prerr_int (Node.getid dom);
			prerr_endline "";
				) domset;
		) doms;
	let bedges = back_edges cfg doms in
	let bedges = BatSet.elements bedges in
	(*
	prerr_endline "back-edges:";
	List.iter (fun bedge ->
			prerr_int (Node.getid (fst bedge)); prerr_string " -> "; prerr_int (Node.getid (snd bedge)); prerr_endline ""
		) bedges;
	*)
	(*one back-edge: one natural loop*)
	let b_edge = List.nth bedges 0 in
	let loopout = get_loopout_node cfg b_edge in
	let unrolled_cfg = copy_loop cfg b_edge loopout in
	unrolled_cfg
