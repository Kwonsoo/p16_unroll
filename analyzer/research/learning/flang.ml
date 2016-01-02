type t = cmd list

and cmd =
	| Skip
	| Assign of lv * exp
	| Alloc of lv * exp
	| Cond of exp

and exp =
	| Nothing
	| Const
	| Uexp of exp
	| Bexp of bops * exp * exp
	| Lval of lv
	| Addr of lv

and lv =
	| Id
	| Deref of lv
	| Array of lv * exp

and bops =
	| Arith
	| Compare

let rec trans_stmt : Cil.stmt -> t
= fun stmt ->
	match stmt.skind with
	|	Cil.Instr il -> List.map trans_il il
	| Cil.If (e, _, _, _) -> [Cond (trans_exp e)]
	| _ -> [Skip]

and trans_il : Cil.instr -> cmd
= fun instr ->
	match instr with
	| Cil.Set (lv, e, _) ->  
			let lv = trans_lv lv in
			let e = trans_exp e in
			Assign (lv, e)
	| Cil.Call (Some lv, Lval (Var vinfo, _), [size], _) ->
			if (vinfo.vname = "malloc" || vinfo.vname = "__builtin_alloca")
			then
				let lv = trans_lv lv in
				let size = trans_exp size in
				Alloc (lv, size)
			else
				Skip
	| _ -> Skip

and trans_exp : Cil.exp -> exp
= fun cexp ->
	match cexp with
	| Cil.Const _ -> Const
	| Cil.Lval lv -> Lval (trans_lv lv)
	| Cil.UnOp (_, e, _) -> Uexp (trans_exp e)
	| Cil.BinOp (cbop, e1, e2, _) -> 
			let bop =
			match cbop with
				| PlusA | PlusPI | IndexPI | MinusA | MinusPI | MinusPP
				| Mult | Div | Mod | Shiftlt | Shiftrt
					-> Arith
				| Lt | Gt | Le | Ge | Eq | Ne | BAnd | BXor | BOr
				| LAnd | LOr
					-> Compare
			in 
				let e1 = trans_exp e1 in
				let e2 = trans_exp e2 in
				Bexp (bop, e1, e2)
	|	Cil.AddrOf lv -> Lval (trans_lv lv)
	| _ -> Nothing (* TO DO *)

and trans_lv : Cil.lval -> lv
= fun (lh, off) -> Id (* TO DO *)

let union_over_set_list : t BatSet.t list -> t BatSet.t
= fun set_list ->
	let sets = BatSet.of_list set_list in
	BatSet.fold BatSet.union sets BatSet.empty

let append_fl : t -> t -> t
= fun f1 f2 -> f1 @ f2

let rec exp_to_str : exp -> string
= fun e ->
	match e with
	| Nothing -> "Nothing "
	| Const -> "Const "
	| Uexp e -> "Uexp " ^ (exp_to_str e)
	| Bexp (bo, e1, e2) -> 
			let bo_str = 
								match bo with
				| Arith -> "Arith "
				| Compare -> "Compare "
			in "Bexp " ^ bo_str ^ (exp_to_str e1) ^ (exp_to_str e2)
	| Lval lv -> "Lval " ^ (lv_to_str lv)
	| Addr lv -> "Addr " ^ (lv_to_str lv)

and lv_to_str : lv -> string
= fun lv ->
	match lv with
	| Id -> "ID "
	| _ -> "lv: Not yet done "

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
			"Alloc " ^ lv ^ e ^ "\n"
	| Cond e ->
			"Cond " ^ (exp_to_str e) ^ "\n"

let flang_to_str : t -> string
= fun t ->
	"*** F-lang *** \n" ^
	(String.concat "" (List.map cmd_to_str t))

let print_flang : t -> unit
= fun t ->
	let contents = flang_to_str t in
	print_endline contents
			
	
