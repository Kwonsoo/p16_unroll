open Cil
open IntraCfg
open Cmd

type t = alarm_exp

let to_string t = 
  match t with
  | ArrayExp (lv,e,_) -> Cil2str.s_lv lv ^ "[" ^ Cil2str.s_exp e ^ "]"
  | DerefExp (e,_) -> "*(" ^ Cil2str.s_exp e ^ ")"
  | Strcpy (e1, e2, _) -> "strcpy ("^(Cil2str.s_exp e1)^", "^(Cil2str.s_exp e2)^")"
  | Strncpy (e1, e2, e3, _) -> "strncpy ("^(Cil2str.s_exp e1)^", "^(Cil2str.s_exp e2)^", "^(Cil2str.s_exp e3)^")"
  | Memcpy (e1, e2, e3, _) -> "memcpy ("^(Cil2str.s_exp e1)^", "^(Cil2str.s_exp e2)^", "^(Cil2str.s_exp e3)^")"
  | Memmove (e1, e2, e3, _) -> "memmove ("^(Cil2str.s_exp e1)^", "^(Cil2str.s_exp e2)^", "^(Cil2str.s_exp e3)^")"
  | Strcat (e1, e2, _) -> "strcat ("^(Cil2str.s_exp e1)^", "^(Cil2str.s_exp e2)^")"
  | DivExp (e1, e2, _) -> Cil2str.s_exp e1 ^ " / " ^ Cil2str.s_exp e2

let eq ale1 ale2 = 
  match ale1, ale2 with
  | ArrayExp (lv1,e1,loc1), ArrayExp (lv2,e2,loc2)  -> 
      to_string ale1 = to_string ale2 && Cil2str.s_location loc1 = Cil2str.s_location loc2
  | DerefExp (e1,loc1), DerefExp (e2,loc2) -> 
      to_string ale1 = to_string ale2 && Cil2str.s_location loc1 = Cil2str.s_location loc2
  | _ -> false (* TODO: consider other cases *)

(* NOTE: you may use Cil.addOffset or Cil.addOffsetLval instead of
   add_offset, append_field, and append_index. *)
let rec add_offset : Cil.offset -> Cil.offset -> Cil.offset
=fun o orig_offset ->
  match orig_offset with
  | NoOffset -> o
  | Field (f, o1) -> Field (f, add_offset o o1)
  | Index (e, o1) -> Index (e, add_offset o o1)

let append_field : Cil.lval -> Cil.fieldinfo -> Cil.lval
=fun lv f -> (fst lv, add_offset (Field (f, NoOffset)) (snd lv))
let append_index : Cil.lval -> Cil.exp -> Cil.lval
=fun lv e -> (fst lv, add_offset (Index (e, NoOffset)) (snd lv))

let rec c_offset : Cil.lval -> Cil.offset -> Cil.location -> t list
=fun lv offset loc ->
  match offset with
  | NoOffset -> []
  | Field (f,o) -> c_offset (append_field lv f) o loc
  | Index (e,o) ->
    (ArrayExp (lv, e, loc)) :: (c_exp e loc)
    @ (c_offset (append_index lv e) o loc)

and  c_lv : Cil.lval -> Cil.location -> t list
=fun lv loc -> 
  match lv with
  | Var v, offset   -> c_offset (Var v, NoOffset) offset loc
  | Mem exp, offset ->
    (DerefExp (exp, loc)) :: (c_exp exp loc)
    @ (c_offset (Mem exp, NoOffset) offset loc)

and c_exp : Cil.exp -> Cil.location -> t list
=fun e loc -> 
  match e with
  | Lval lv -> c_lv lv loc
  | AlignOfE e -> c_exp e loc
  | UnOp (_,e,_) -> c_exp e loc
  | BinOp (Div,e1,e2,_) -> DivExp(e1,e2,loc):: (c_exp e1 loc) @ (c_exp e2 loc)
  | BinOp (_,e1,e2,_) -> (c_exp e1 loc) @ (c_exp e2 loc)
  | CastE (_,e) -> c_exp e loc
  | AddrOf lv -> c_lv lv loc
  | StartOf lv -> c_lv lv loc
  | _ -> []

and c_alloc alloc loc = 
  match alloc with
  | IntraCfg.Cmd.Array e -> c_exp e loc

and c_exps exps loc = List.fold_left (fun q e -> q @ (c_exp e loc)) [] exps

let collect : IntraCfg.cmd -> t list
=fun cmd ->
  match cmd with
  | Cmd.Cset (lv,e,loc) -> (c_lv lv loc) @ (c_exp e loc)
  | Cmd.Cexternal (lv,loc) -> c_lv lv loc
  | Cmd.Calloc (lv,alloc,_,loc) -> (c_lv lv loc) @ (c_alloc alloc loc) 
  | Cmd.Csalloc (lv,_,loc) -> c_lv lv loc
  | Cmd.Cassume (e,loc) -> c_exp e loc
  | Cmd.Ccall (_, Lval (Var f, NoOffset), es, loc)
    when f.vname = "strcpy" -> (Strcpy (List.nth es 0, List.nth es 1, loc)) :: (c_exps es loc)
  | Cmd.Ccall (_, Lval (Var f, NoOffset), es, loc)
    when f.vname = "memcpy" -> (Memcpy (List.nth es 0, List.nth es 1, List.nth es 2, loc))::(c_exps es loc)
  | Cmd.Ccall (_, Lval (Var f, NoOffset), es, loc)
    when f.vname = "memmove" -> (Memmove (List.nth es 0, List.nth es 1, List.nth es 2, loc))::(c_exps es loc)
  | Cmd.Ccall (_, Lval (Var f, NoOffset), es, loc)
    when f.vname = "strncpy" -> (Strncpy (List.nth es 0, List.nth es 1, List.nth es 2, loc))::(c_exps es loc)
  | Cmd.Ccall (_, Lval (Var f, NoOffset), es, loc)
    when f.vname = "strcat" -> (Strcat (List.nth es 0, List.nth es 1, loc)) :: (c_exps es loc)
  | Cmd.Ccall (None,e,es,loc) -> (c_exp e loc) @ (c_exps es loc) 
  | Cmd.Ccall (Some lv,e,es,loc) -> (c_lv lv loc) @ (c_exp e loc) @ (c_exps es loc)
  | Cmd.Creturn (Some e, loc) -> c_exp e loc
  | _ -> []


