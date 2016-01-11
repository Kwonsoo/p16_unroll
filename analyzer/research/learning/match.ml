open Flang

let rec match_exp : exp * exp -> bool
= fun (e1, e2) ->
	match e1, e2 with
	| Nothing, _ | _, Nothing -> true
	| Const, Const -> true
	| Uexp e1, Uexp e2 ->
		match_exp (e1, e2)
	| Bexp (bops1, e11, e12), Bexp (bops2, e21, e22) ->
		match_exp (e11, e21) && match_exp (e12, e22)
	| Lval l1, Lval l2 | Addr l1, Addr l2
		-> match_lv (l1, l2)
	| _ -> false

and match_lv : lv * lv -> bool
= fun (l1, l2) ->
	match l1, l2 with
	| Id, Id -> true
	| Array, Array -> true
	| Deref _, Deref _ -> true
	| _ -> false (* TO DO *)

let match_cmd : cmd * cmd -> bool
= fun (c1, c2) ->
	match c1, c2 with
	| Assign (l1, e1), Assign (l2, e2) 
	| Alloc (l1, e1), Alloc (l2, e2) 
		-> match_lv (l1, l2) && match_exp (e1, e2)
	| Cond e1, Cond e2 ->
		match_exp (e1, e2)
	| _ -> false


let rec remove_same_seq : cmd -> t -> t
= fun cmd rest ->
	match rest with
	| hd::tl ->
		if (match_cmd (cmd, List.hd rest))
		then
			remove_same_seq cmd (List.tl rest)
		else
			rest
	| _ -> []	

(* Too strict 
let match_fl : t -> t -> bool
= fun feat target ->
	let entire_feat = feat in
	let rec match_helper feat target =
		match feat, target with
		| hd_feat::tl_feat, hd_target::tl_target ->
			if match_cmd (hd_feat, hd_target)
			then
				let rest = remove_same_seq hd_feat tl_target in
				match_helper tl_feat rest
			else
				match_helper entire_feat tl_target
		| [],  _ -> true
		| _ -> false
	in match_helper feat target
*)

let match_fl : t -> t -> bool
= fun feat target ->
	let entire_feat = feat in
	let rec match_helper feat target =
		match feat, target with
		| hd_feat::tl_feat, hd_target::tl_target ->
			if match_cmd (hd_feat, hd_target)
			then
				let rest = remove_same_seq hd_feat tl_target in
				match_helper tl_feat rest
			else
				match_helper feat tl_target				
		| [], _ -> true
		| _ -> false
	in match_helper feat target

			

