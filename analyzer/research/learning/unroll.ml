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
	let initial_dom = BatMap.add Node.ENTRY Node.ENTRY BatMap.empty in
	let nodes = nodesof cfg in
	let nodes = BatSet.of_list nodes in
	let initial_dom = List.fold_right (fun n acc ->
			if not Node.equal n Node.ENTRY
			then BatMap.add n nodes acc
			else acc
		) nodes initial_dom in
	initial_dom

(*TODO*)
let check_dom_fixpoint : dom -> dom -> bool
=fun d1 d2 -> true	(*place_holder*)

let rec find : IntraCfg.t -> dom -> dom -> dom
=fun cfg d1 d2 ->
	if check_dom_fixpoint d1 d2
	then d2
	else (
			let nodes = nodesof cfg in
			let dom_new = List.fold_right (fun n acc ->
					let preds = pred n cfg in
					let preds_doms_intersection = List.fold_right (fun p acc ->
							let pred_doms = try BatMap.find p d2 with Not_found -> BatSet.empty in
							BatSet.intersect pred_doms acc
						) preds BatSet.empty in
					let new_doms_of_node = BatSet.union n preds_doms_intersection in
					BatMap.add n new_doms_of_node d2
				) nodes in
			find cfg d2 dom_new
	)

let find_dominator : IntraCfg.t -> dom
=fun cfg -> 
	let initial = dom_init cfg in
	let final = find cfg BatMap.empty initial in
	final

(**********************
 * back-edge analysis	*
 **********************)

let back_edges : IntraCfg.t -> dom -> (Node.t * Node.t) Batet.t
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

(*TODO*)
let loopout_node : IntraCfg.t -> (Node.t * Node.t) -> Node.t
=fun cfg back_edge -> Node.ENTRY	(*place-holder*)
	

(****************************************
 * natural loop construction.						*
 * A back-edge defines a natural loop.	*
 ****************************************)

type loop = {
	cfg : IntraCfg.t;
	bedge : (Node.t * Node.t)
	loopout : Node.t
}

let rec copy : IntraCfg.t -> (Node.t * Node.t) -> Node.t -> Node.t -> loop
=fun cfg back_edge loopout loopin  ->
	

let connect_two_nodes : IntraCfg.t -> IntraCfg.t -> IntraCfg.Node.t -> IntraCfg.Cmd.t -> IntraCfg.Node.t -> IntraCfg.Cmd.t -> IntraCfg.t
=fun cfg cfg_new n1 cmd1 n2 cmd2 ->
	let cfg' = add_node_with_cmd n1 cmd1 cfg_new in
	let cfg' = add_node_with_cmd n2 cmd2 cfg' in
	let cfg' = add_edge n1 n2 cfg' in
	cfg'

let copy_loop : IntraCfg.t -> (Node.t * Node.t) -> Node.t -> loop
=fun cfg back_edge loopout ->
	let loophead = snd back_edge in
	if List.length (succ loophead cfg) < 2 then raise (Failure "copy_loop: loop-head successors less than 2");
	else (
			let loopin =
					if Node.equal (List.nth (succ loophead cfg) 0) loopout 
					then List.nth (succ loophead cfg) 1 
					else List.nth (succ loophead cfg) 0 in
			let loop_initial = IntraCfg.empty (Cil.emptyFunction "dummy") in			(*empty*)
			let loop_initial =  in	(*loop-head*)

	)
	

(*TODO*)
let connect_two_loops : IntraCfg.t -> (Node.t * Node.t) -> Node.t -> loop -> IntraCfg.t
=fun cfg back_edge loopout loop -> IntraCfg.empty







