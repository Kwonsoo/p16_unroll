open Cil
open Visitors
open Report

let nidof_q : Report.query -> int
= fun q -> 
	let intra_node = InterCfg.Node.get_cfgnode q.node in
	IntraCfg.Node.getid intra_node

let query2nid query =
	let nid = nidof_q query in
	let vinfo = Cil.makeGlobalVar (string_of_int nid) (TVoid []) in
	[Cil.Lval (Cil.Var vinfo, NoOffset)]

let gen_airac_nid query =
	let loc = query.loc in
	let var = Cil.makeGlobalVar "airac_nid" (TFun (TVoid [], Some [], true, [])) in
	let airac_nid = Cil.Lval (Cil.Var var, Cil.NoOffset) in
	let nid_arg = query2nid query in
	Call (None, airac_nid, nid_arg, loc)

let is_break_stmt : Cil.stmt -> bool
= fun s ->
	let labels = s.labels in
	List.exists (fun label -> 
		match label with
		| Label (name, _, flag) when flag = false -> 
			BatString.exists name "while_break"
		| _ -> false) labels

(* TO DO *)
let unroll_body : Cil.fundec -> Cil.block -> int -> Cil.block
= fun fd body k ->
	match k with
	| 0 -> body
	| _ -> body

class unrollingVisitor (fd, k) = object(self)
	inherit nopCilVisitor
	method vstmt (s : stmt) =
		match s.skind with
		| Goto (target, _) when not (is_break_stmt (!target)) ->
			let skind = Instr [] in
			ChangeTo {labels = []; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds;}
		| Loop (body, _, _, _) ->
			let unrolled = unroll_body fd body k in
			let skind = Block unrolled in
			let body = {labels = []; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds;} in
			ChangeDoChildrenPost (body, (fun s -> s))
		| _ -> DoChildren
end

class insertNidVisitor (query) = object(self)
	inherit nopCilVisitor

	val alarm_exp = query.exp

	method vinst (i : instr) =
		match i with
		| Set (lhs, rhs, loc) when (found (rhs, loc, alarm_exp) || found (Lval lhs, loc, alarm_exp))
			-> ChangeTo [Set (lhs, rhs, loc ); gen_airac_nid query]
		| Call (ret, fexp, args, loc) when (found (fexp, loc, alarm_exp) || found_s (args, loc, alarm_exp))
			-> ChangeTo [Call (ret, fexp, args, loc); gen_airac_nid query]
		| _ -> DoChildren

	method vstmt (s : stmt) = 
    	match s.skind with 
    	| If (e, b1, b2, loc) when found (e, loc, alarm_exp) ->
        	let nid_stmt = {labels=[]; skind = Instr [gen_airac_nid query]; sid=0; succs=[]; preds=[]} in
        	let skind = Block { battrs = []; bstmts = [s;nid_stmt] } in
        	ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    	| Return (Some exp, loc) when found (exp, loc, alarm_exp) ->
        	let nid_stmt = {labels=[]; skind = Instr [gen_airac_nid query]; sid=0; succs=[]; preds=[]} in
        	let skind = Block { battrs = []; bstmts = [s;nid_stmt] } in
        	ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    	| Switch (exp, _,_,loc) when found (exp, loc, alarm_exp) ->
        	let nid_stmt = {labels=[]; skind = Instr [gen_airac_nid query]; sid=0; succs=[]; preds=[]} in
        	let skind = Block { battrs = []; bstmts = [s;nid_stmt] } in
        	ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    	| _ -> DoChildren 
end
