open Cil

let card_succs : Cil.stmt -> int
= fun s -> List.length s.succs

let card_preds : Cil.stmt -> int 
= fun s -> List.length s.preds

let is_branch : Cil.stmt -> bool
= fun s -> if (card_succs s) > 1 then true else false

let get_the_succ : Cil.stmt -> Cil.stmt
= fun s -> List.hd s.succs

let get_succs : Cil.stmt -> Cil.stmt list
= fun s -> s.succs

(* Use this only to find a loop stmt. *)
let get_the_pred : Cil.stmt -> Cil.stmt
= fun this -> List.find (fun stmt ->
	this.sid > stmt.sid) this.preds
	
let get_loop_break : Cil.stmt -> Cil.stmt
= fun s ->
	let loop_next = get_the_succ s in
	let loop_stmt = get_the_pred loop_next in
	match loop_stmt.skind with
	| Cil.Loop (_, _, _, break) -> BatOption.get break
	| _ -> raise (Failure "get_loop_break: Not a loop stmt ")

let is_backward : Cil.stmt * Cil.stmt -> bool
= fun (this, succ) -> this.sid > succ.sid

let get_begin_stmt : Cil.fundec -> Cil.stmt
= fun fd ->
	let allStmts = fd.sallstmts in
	List.hd allStmts

let process_one_stmt : Cil.stmt -> Flang.t -> Flang.t
= fun stmt accum ->
	let trans = Flang.trans_stmt stmt in
	Flang.append_fl accum trans

let build_featTbl : Cil.fundec -> (int, Flang.t) Hashtbl.t
= fun fd ->
	let idx = ref 0 in
	let featTbl = Hashtbl.create 251 in
	let begin_stmt = get_begin_stmt fd in
			
	(let rec add_paths : Cil.stmt -> Flang.t -> unit
	= fun stmt accum ->
		let appended = process_one_stmt stmt accum in
		match (card_succs stmt) with
		| 0 -> (Hashtbl.add featTbl !idx appended); idx := !idx + 1
		| _ -> 
			let succs = get_succs stmt in
			List.iter (fun succ ->
				if is_backward (stmt, succ)
				then
					let break = get_loop_break stmt in
					add_paths break appended
				else
					add_paths succ appended
			) succs
		in add_paths begin_stmt []); featTbl

let build_varTbl : Cil.fundec -> (int, stmt list) Hashtbl.t
= fun fd ->
	let idx = ref 0 in
	let rawTbl = Hashtbl.create 251 in
	let begin_stmt = get_begin_stmt fd in

	(let rec add_paths : Cil.stmt -> stmt list -> unit
	= fun stmt accum ->
		let appended = accum @ [stmt] in
		match (card_succs stmt) with
		| 0 -> (Hashtbl.add rawTbl !idx appended); idx := !idx + 1
		| _ ->
			let succs = get_succs stmt in
			List.iter (fun succ ->
				if is_backward (stmt, succ)
				then
					let break = get_loop_break stmt in
					add_paths break appended
				else
					add_paths succ appended
			) succs
		in add_paths begin_stmt []); rawTbl
		

let delete_skip : Flang.t -> Flang.t
= fun f ->
	List.filter (fun s ->
		match s with
		| Flang.Skip -> false
		| _ -> true
	) f

let tbl_to_set : (int, Flang.t) Hashtbl.t -> Flang.t BatSet.t
= fun tbl ->
	let fold_f key value accum =
		BatSet.add (delete_skip value) accum
	in Hashtbl.fold fold_f tbl BatSet.empty

let get_featSet : Cil.fundec -> Flang.t BatSet.t
= fun fd ->
	let featTbl = build_featTbl fd in
	tbl_to_set featTbl

let unroll_loop : Cil.stmt list -> int -> Cil.stmt list
= fun orgs factor -> orgs (* TO DO *)



