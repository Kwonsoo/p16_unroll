type dir = string

type fvector = bool list
type tdata = (fvector * bool)
	
let fvectorize : Global.t -> fvector
=fun global -> [true] (* TODO *)

let build_t_data : Global.t -> tdata
=fun global -> ([true], true)	(* TODO *)

	
module Trainer : sig

	(* Produce single-query programs from the given T2 directory. *)
	val copy_pgms : dir -> dir -> unit
	(* Build all training data from the single-query programs. *)
	val build_training_dataset : dir -> tdata BatSet.t

end = struct 

	let copy_pgms = fun t2dir sqdir -> ()	(* TODO *)

	let build_training_dataset = fun sqdir -> BatSet.empty	(* TODO *)

end

module Slicer =
struct
	open Cil

	let get_fundecs : Cil.file -> Cil.fundec list
	= fun file ->
		List.fold_left (fun acc glob ->
			match glob with
			| Cil.GFun (fd, _) -> fd::acc
			| _ -> acc
		) [] file.globals

	let is_var : Cil.lval -> bool
	= fun (lhost, _) ->
		match lhost with
		| Cil.Var vinfo -> true
		| _ -> false
	
	let is_observe_call : Cil.instr -> bool
	= fun instr ->
		match instr with
		| Cil.Call (ret, func, args, _) ->
				(match func with
				| Cil.Lval lv when is_var lv ->
						let (Cil.Var vinfo, _) = lv in 						
						vinfo.vname = "airac_observe"
				| _ -> false)
		| _ -> false

	let has_observer : Cil.fundec -> bool
	= fun fd ->
		let stmts = fd.sallstmts in
		List.exists (fun stmt ->
			match stmt.skind with
			| Cil.Instr il ->
					List.exists is_observe_call il
			| _ -> false
		) stmts
					
	let find_observe_fundec : Cil.file -> Cil.fundec
	= fun file ->
		let fundecs = get_fundecs file in
		List.find has_observer fundecs

end
