type dir = string

type fvector = bool list
type tdata = (fvector * bool)
	
let fvectorize : Global.t -> fvector
=fun global -> [true] (* TODO *)

(* NOTE: 첫번쩨 인자로 받는 프로그램은 Q(x)까지 extract된 프로그램이고,
				 이 extract 처리된 프로그램의 형태는 flang으로 표현된 path들의 집합이다.
				 두번째 인자로 받는 feature list는 실제 자동으로 만들어진 feature들의 리스트이다.*)
let build_t_data : Flang.t BatSet.t -> Flang.t list -> tdata
=fun extracted_prog feature_list -> (*([true], true)*)
	let feature_bool_vector = 
		List.fold_right (fun f accum ->
				(pred extracted_prog f) :: accum
			) feature_list []
	in
	
	
module Trainer : sig

	(* Produce single-query programs from the given T2 directory. *)
	val t2_to_singleq_progs : dir -> dir -> unit
	(* Build all training data from the single-query programs. *)
	val build_training_dataset : dir -> tdata BatSet.t

end = struct 

	let t2_to_singleq_progs = fun t2dir sqdir ->
		let files = Sys.readdir t2dir in
		let files = Array.to_list files in
		List.iter (fun f -> 
				Sys.command ("./main.native ../T2/" ^ f ^ " -insert_observe_imprecise -imprecise_type fs -dir ../T2_singleq"); ()
			) files

	let build_training_dataset = fun sqdir -> BatSet.empty (* TODO *)
		
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
