(** Eval operations. *)
open Vocab
open AbsDom
open ItvDom
open ArrayBlk

let eraser_value = ref Itv.top

let eval_const : Cil.constant -> Val.t = fun cst ->
  match cst with
  | Cil.CInt64 (i64, _, _) ->
    if Cil.i64_to_int i64 = 614512 then make_eraser_value !eraser_value (* agreed to produce Top *)
    else
      let itv = try itv_of_int (Cil.i64_to_int i64) with _ -> Itv.top in
      val_of_itv itv
  | Cil.CStr s -> invalid_arg ("evalOp.ml: eval_const string "^s)
  | Cil.CWStr s -> invalid_arg "evalOp.ml: eval_const wide string"
  | Cil.CChr c -> val_of_itv (itv_of_int (int_of_char c))

  (* Float numbers are modified to itvs.  If you want to make a
     precise and sound analysis for float numbers, you have to
     develop a domain for them. *)
  | Cil.CReal (f, _, _) ->
    val_of_itv (itv_of_ints (int_of_float (floor f)) (int_of_float (ceil f)))

  (* BatEnum is not evaluated correctly in our analysis. *)
  | Cil.CEnum _ -> val_of_itv Itv.top


let eval_uop : Cil.unop -> Val.t -> Val.t = fun u v ->
  if Val.eq v Val.bot then Val.bot else
    let itv = itv_of_val v in
    let itv' =
      match u with
      | Cil.Neg -> Itv.minus (itv_of_int 0) itv
      | Cil.BNot -> Itv.b_not_itv itv
      | Cil.LNot -> Itv.not_itv itv in
    val_of_itv itv'


let eval_bop : Cil.binop -> Val.t -> Val.t -> Val.t = fun b v1 v2 ->

  let is_array_loc = function
    | (VarAllocsite.Inr _, _) -> true
    | _ -> false in

  let array_loc_of_val v =
    let l = pow_loc_of_val v in
    let l' = PowLoc.filter is_array_loc l in
    val_of_pow_loc l' in

  match b with
  | Cil.PlusA -> val_of_itv (Itv.plus (itv_of_val v1) (itv_of_val v2))
  | Cil.PlusPI
  | Cil.IndexPI ->
    let ablk = array_of_val v1 in
    let offset = itv_of_val v2 in
    let ablk = ArrayBlk.plus_offset ablk offset in
    Val.join (array_loc_of_val v1) (val_of_array ablk)
  | Cil.MinusA -> val_of_itv (Itv.minus (itv_of_val v1) (itv_of_val v2))
  | Cil.MinusPI ->
    let ablk = array_of_val v1 in
    let offset = itv_of_val v2 in
    let ablk = ArrayBlk.minus_offset ablk offset in
    Val.join (array_loc_of_val v1) (val_of_array ablk)
  | Cil.MinusPP -> val_of_itv Itv.top
  | Cil.Mult -> val_of_itv (Itv.times (itv_of_val v1) (itv_of_val v2))
  | Cil.Div -> val_of_itv (Itv.divide (itv_of_val v1) (itv_of_val v2))
  | Cil.Mod -> val_of_itv (Itv.mod_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Shiftlt -> val_of_itv (Itv.l_shift_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Shiftrt -> val_of_itv (Itv.r_shift_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Lt -> val_of_itv (Itv.lt_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Gt -> val_of_itv (Itv.gt_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Le -> val_of_itv (Itv.le_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Ge -> val_of_itv (Itv.ge_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Eq -> val_of_itv (Itv.eq_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.Ne -> val_of_itv (Itv.ne_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.BAnd -> val_of_itv (Itv.b_and_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.BXor -> val_of_itv (Itv.b_xor_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.BOr -> val_of_itv (Itv.b_or_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.LAnd -> val_of_itv (Itv.and_itv (itv_of_val v1) (itv_of_val v2))
  | Cil.LOr -> val_of_itv (Itv.or_itv (itv_of_val v1) (itv_of_val v2))

let elem_typ_of : Cil.typ -> Cil.typ option = function
  | Cil.TPtr (t, _)
  | Cil.TArray (t, _, _) -> Some t
  | _ -> None

let is_char_ptr t = 
  match t with
 | Cil.TPtr (Cil.TInt (Cil.IChar,_),_) -> true
 | _ -> false

let rec resolve_offset : Proc.t -> Val.t -> Cil.offset -> Mem.t -> PowLoc.t
= fun pid v os mem ->
  match os with
  | Cil.NoOffset ->
    let p1 = pow_loc_of_val v in
    let p2 = ArrayBlk.pow_loc_of_array (array_of_val v) in
    PowLoc.join p1 p2

  | Cil.Field (f, os') ->
    let l1 = pow_loc_append_field (pow_loc_of_val v) f.Cil.fname in
    let l2 = pow_loc_of_struct_w_field (array_of_val v) f.Cil.fname in
    let v = val_of_pow_loc (PowLoc.join l1 l2) in
    resolve_offset pid v os' mem

  | Cil.Index (e, os') ->
    let _ = eval pid e mem in (* NOTE: to sync with access function *)
    let a = array_of_val (Mem.lookup (pow_loc_of_val v) mem) in
    resolve_offset pid (val_of_pow_loc (ArrayBlk.pow_loc_of_array a)) os' mem

(* XXX : Cil.bitsSizeOf often fails: just return top for the moment
 * Adhoc solution: To avoid this failure, translate original C sources 
 * into "CIL" (using -il option) and analyze the CIL program. *)
and itv_of_byteSizeOf : Cil.typ -> Itv.t
=fun typ ->  
  try itv_of_int ((Cil.bitsSizeOf typ) / 8) 
  with _ -> prerr_endline ("warn: Cil.bitsSizeOf (" ^ Cil2str.s_type typ ^ ")") ; Itv.top

and eval : Proc.t -> Cil.exp -> Mem.t -> Val.t
= fun pid e mem ->
  match e with
  | Cil.Const c -> eval_const c 
  | Cil.Lval l -> Mem.lookup (eval_lv pid l mem) mem 
  | Cil.SizeOf t -> val_of_itv (itv_of_byteSizeOf t)
  | Cil.SizeOfE e -> val_of_itv (itv_of_byteSizeOf (Cil.typeOf e))
  | Cil.SizeOfStr s -> val_of_itv (itv_of_int (String.length s + 1))
  | Cil.AlignOf t -> val_of_itv (itv_of_int (Cil.alignOf_int t))
  (* TODO: type information is required for precise semantics of AlignOfE.  *)
  | Cil.AlignOfE _ -> val_of_itv Itv.top
  | Cil.UnOp (u, e, _) -> eval_uop u (eval pid e mem)
  | Cil.BinOp (b, e1, e2, _) ->
    eval_bop b (eval pid e1 mem) (eval pid e2 mem)

  | Cil.Question (e1, e2, e3, _) -> 
    let i1 = itv_of_val (eval pid e1 mem) in
    if Itv.is_bot i1 then
      Val.bot
    else if Itv.eq (itv_of_int 0) i1 then
      eval pid e3 mem
    else if not (Itv.le (itv_of_int 0) i1) then
      eval pid e2 mem
    else 
      Val.join (eval pid e2 mem) (eval pid e3 mem)

  | Cil.CastE (t, e) ->
    let v = eval pid e mem in
    let v_itv = itv_of_val v in
    let v' = 
      (match elem_typ_of t with
       | Some t' ->
         let new_stride = itv_of_byteSizeOf t' in
         let array = cast_array new_stride (array_of_val v) in
           modify_array v array
       | None -> v) in
    let v_null = 
      if Itv.eq Itv.zero v_itv && is_char_ptr t then val_of_pow_loc (PowLoc.singleton Loc.null) 
      else Val.bot in
      Val.join v' v_null
  | Cil.AddrOf l -> val_of_pow_loc (eval_lv pid l mem)
  | Cil.AddrOfLabel _ ->
    invalid_arg "evaOp.ml:eval AddrOfLabel mem. \
                 Analysis does not support label values."

  | Cil.StartOf l -> Mem.lookup (eval_lv pid l mem) mem

(* TODO: Syntax of lval in Cil is different to our design, but the
   essense is the same.  We can simply modify our design
   documentation. *)
and eval_lv : Proc.t -> Cil.lval -> Mem.t -> PowLoc.t
= fun pid lv  mem ->
  let v =
    match fst lv with
    | Cil.Var vi ->
      let x = var_of_varinfo vi pid in
        val_of_pow_loc (PowLoc.singleton (Loc.loc_of_var x))
    | Cil.Mem e -> eval pid e mem
  in
  resolve_offset pid v (snd lv) mem

and var_of_varinfo vi pid  =
  if vi.Cil.vglob then Var.var_of_gvar vi.Cil.vname 
  else Var.var_of_lvar (pid, vi.Cil.vname)

let eval_list : Proc.t -> Cil.exp list -> Mem.t -> Val.t list
= fun pid exps mem ->
  List.map (fun e -> eval pid e mem) exps

let eval_alloc : Node.t -> IntraCfg.Cmd.alloc -> bool -> Mem.t -> Val.t
= fun node a static mem ->
  let pid = Node.get_pid node in
  let allocsite = Allocsite.allocsite_of_node node in
  match a with
  | IntraCfg.Cmd.Array e ->
    let o = itv_of_int 0 in
    let sz = itv_of_val (eval pid e mem) in
    (* NOTE: stride is always one when allocating memory. *)
    let st = itv_of_int 1 in
    let pow_loc = (if not static then PowLoc.add Loc.null else id) (PowLoc.singleton (Loc.loc_of_allocsite allocsite)) in
    let array = ArrayBlk.make allocsite o sz st in
      Val.join (val_of_pow_loc pow_loc) (val_of_array array)

let eval_string : string -> Val.t = fun s ->
  val_of_itv (Itv.V (Itv.Int 0, Itv.PInf))

let eval_string_loc : string -> Allocsite.t -> PowLoc.t -> Val.t
= fun s allocsite pow_loc ->
  let o = itv_of_int 0 in
  let sz = itv_of_int (String.length s) in
  let st = itv_of_int 1 in
  let array = ArrayBlk.make allocsite o sz st in
  Val.join (val_of_pow_loc pow_loc) (val_of_array array)
