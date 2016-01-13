open Cil

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

