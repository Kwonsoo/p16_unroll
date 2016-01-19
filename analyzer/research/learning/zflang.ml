open Cil
open IntraCfg

type t = cmd list

and cmd =
	| Skip
	| Assign of lv * exp
	| Alloc of lv * exp
	| Cond of exp

and exp =
	| Nothing
	| Top
	| Const
	| Uexp of exp
	| Bexp of exp * exp
	| Lval of lv
	| Addr of lv

and lv =
	| Id
	| Deref of lv
	| Array

let rec trans_exp : Cil.exp -> exp
= fun exp ->
	match exp with
	| Cil.Const _ -> Const
	| Cil.Lval lv -> Lval Id
	| Cil.UnOp (_, exp, _) -> Uexp (trans_exp exp)
	| Cil.BinOp (_, e1, e2, _) ->
		let e1, e2 = trans_exp e1, trans_exp e2 in
		Bexp (e1, e2)
	| Cil.AddrOf lv -> Lval (trans_lv lv)
	| Cil.StartOf lv -> Lval (trans_lv lv)
	| _ -> Nothing (* TO DO *)

and trans_lv : Cil.lval -> lv
= fun (lh, off) ->
	match lh with
	| Cil.Var v ->
		(match off with
		| Cil.NoOffset -> Id
		| Cil.Field _ -> Id
		| Cil.Index _ -> Array)
	| Cil.Mem e ->
		(match e with
		| Cil.Lval lv -> Deref Id
		| _ -> Id)
	| _ -> Id

and trans_alloc : IntraCfg.Cmd.alloc -> exp
= fun alloc ->
	match alloc with
	| IntraCfg.Cmd.Array e -> trans_exp e

let rec trans_cmd : IntraCfg.Cmd.t -> cmd
= fun cmd ->
	match cmd with
	| Cskip -> Skip
	| Cset (lv, e, _) ->
		let flv = trans_lv lv in
		let fexp = trans_exp e in
		Assign (flv, fexp)
	| Cexternal (lv, _) ->
		let flv = trans_lv lv in
		Assign (flv, Top)
	| Calloc (lv, alloc, _, _) ->
		let flv = trans_lv lv in
		let size = trans_alloc alloc in
		Alloc (flv, size)
	| Cassume (exp, _) -> 
		let fexp = trans_exp exp in
		Cond fexp
	| _ -> Skip

let trans_graph : IntraCfg.t -> t
= fun g ->
	let entry = fold_vertex (fun node acc ->
		match List.length (pred node g) with
		| 0 -> node
		| _ -> acc) g Node.ENTRY in
	
	let rec process_one_node node acc =
		let cmd = find_cmd node g in
		let translated = trans_cmd cmd in
		let succs = (succ node g) in
		match List.length succs with
		| 0 -> acc
		| 1 -> process_one_node (List.hd succs) (acc @ [translated])
		| _ -> raise (Failure "process_one_node") in
	let transed = process_one_node entry [] in
	let skip_removed = List.filter (fun cmd ->
		not (cmd = Skip)) transed in
	skip_removed

let rec exp_to_str : exp -> string
= fun e ->
	match e with
	| Nothing -> "Nothing "
	| Const -> "Const "
	| Uexp e -> "Uexp " ^ (exp_to_str e)
	| Bexp (e1, e2) -> "Bexp "^ (exp_to_str e1) ^ (exp_to_str e2)
	| Lval lv -> "Lval " ^ (lv_to_str lv)
	| Addr lv -> "Addr " ^ (lv_to_str lv)

and lv_to_str : lv -> string
= fun lv ->
	match lv with
	| Id -> "Id "
	| Deref lv -> "Deref "
	| Array -> "Array "

let cmd_to_str : cmd -> string
= fun c ->
	match c with
	| Skip -> "Skip \n"
	| Assign (lv, e) ->
		let lv = lv_to_str lv in
		let e = exp_to_str e in
		"Assign " ^ lv ^ e ^ "\n"
	| Alloc (lv, e) ->
		let lv = lv_to_str lv in
		let e = exp_to_str e in
		"Alooc " ^ lv ^ e ^ "\n"
	| Cond e -> "Cond " ^ (exp_to_str e) ^ "\n"

let print_flang : t -> unit
= fun t ->
	let contents = String.concat "" (List.map cmd_to_str t) in
	print_endline contents

