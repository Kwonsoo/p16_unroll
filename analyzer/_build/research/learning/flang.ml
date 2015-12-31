type t = Cil.stmt list (* TO DO *)

let trans_stmt : Cil.stmt -> t 
= fun cstmt -> [cstmt] (* TO DO *)

let union_over_set_list : t BatSet.t list -> t BatSet.t
= fun set_list -> 
	let sets = BatSet.of_list set_list in
	BatSet.fold BatSet.union sets BatSet.empty

let append_fl : t -> t -> t
= fun f1 f2 -> f1 @ f2

let get_instr_name : Cil.instr -> string
= fun i ->
	match i with
	| Set _ -> "Set"
	| Call _ -> "Call"
	| _ -> raise (Failure "get_instr_name")

let get_stmt_name : Cil.stmt -> string
= fun s ->
	match s.skind with
	|	Instr il -> String.concat "\n" (List.map get_instr_name il)
	| Return _ -> "Return"
	| Goto _ -> "Goto"
	| Break _ -> "Break"
	| If _ -> "If"
	| Switch _ -> "Switch"
	| Loop _ -> "Loop"
	| Block _ -> "Block"
	| _ -> raise (Failure "get_stmt_name: skind")

let flang_to_strs : t -> string
= fun fl -> String.concat "\n" (List.map get_stmt_name fl)

let print_flang : t -> unit
= fun fl -> 
	print_endline "*** ***";
	print_endline (flang_to_strs fl)
