(** Pruning *)

open Cil
open AbsSem
open AbsDom
open EvalOp
open ItvDom

let rev_binop : binop -> binop = fun op ->
  match op with
  | Lt -> Gt
  | Gt -> Lt
  | Le -> Ge
  | Ge -> Le
  | Eq -> Eq
  | Ne -> Ne
  | _ -> invalid_arg "prune.ml: rev_binop"

let not_binop : binop -> binop = fun op ->
  match op with
  | Lt -> Ge
  | Gt -> Le
  | Le -> Gt
  | Ge -> Lt
  | Eq -> Ne
  | Ne -> Eq
  | LAnd -> LOr
  | LOr -> LAnd
  | _ -> invalid_arg "prune.ml: rev_binop"

let rec make_cond_simple : exp -> exp option = fun cond ->
  match cond with
  | BinOp (op, Lval _, _, _)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    Some cond
  | BinOp (op, e, Lval x, t)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    Some (BinOp (rev_binop op, Lval x, e, t))
  | BinOp (op, BinOp (PlusA, Lval x, Lval y, t2), e, t) ->
    make_cond_simple (BinOp (op, Lval x, BinOp (MinusA, e, Lval y, t2), t))
  | BinOp (op, BinOp (MinusA, Lval x, Lval y, t2), e, t) ->
    make_cond_simple (BinOp (op, Lval x, BinOp (PlusA, e, Lval y, t2), t))
 
  | UnOp (LNot, BinOp (op, e1, e2, t2), _)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    make_cond_simple (BinOp (not_binop op, e1, e2, t2))
  | UnOp (LNot, BinOp (op, e1, e2, t2), t1)
    when op = LAnd || op = LOr ->
    let not_e1 = UnOp (LNot, e1, t1) in
    let not_e2 = UnOp (LNot, e2, t1) in
    (match make_cond_simple not_e1, make_cond_simple not_e2 with
     | Some e1', Some e2' -> Some (BinOp (not_binop op, e1', e2', t2))
     | _, _ -> None)
  | UnOp (LNot, UnOp (LNot, e, _), _) -> make_cond_simple e
  | UnOp (LNot, Lval _, _) -> Some cond
  | Lval _ -> Some cond
  | BinOp (op, CastE (_, e1), e2, t) 
  | BinOp (op, e1, CastE (_, e2), t) -> 
    let newe = BinOp (op, e1, e2, t) in
    make_cond_simple newe
 
  | _ -> None

let rec prune_simple : AbsDom.update_mode -> Global.t -> Proc.t -> exp -> Mem.t
  -> Mem.t
= fun mode global pid cond mem ->
  match cond with
  | BinOp (op, Lval x, e, t)
    when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
    let x_lv = eval_lv pid x mem in
    let x_v = Mem.lookup x_lv mem in
    let v = eval pid e mem in
    let e_v = itv_of_val v in
    let x_itv = Itv.prune op (itv_of_val x_v) e_v in
    let x_ploc = PowLoc.prune op (pow_loc_of_val x_v) e in
    let x_pruned = modify_powloc (modify_itv x_v x_itv) x_ploc in
    Mem.update mode global x_lv x_pruned mem
  | BinOp (op, e1, e2, _) when op = LAnd || op = LOr ->
    let mem1 = prune_simple mode global pid e1 mem in
    let mem2 = prune_simple mode global pid e2 mem in
    if op = LAnd then Mem.meet mem1 mem2 else Mem.join mem1 mem2
  | UnOp (LNot, Lval x, _) ->
    let x_lv = eval_lv pid x mem in
    let x_v = Mem.lookup x_lv mem in
    let x_itv = itv_of_val x_v in
    let e_v = Itv.V (Itv.Int 0, Itv.Int 0) in
    let x_itv = Itv.meet x_itv e_v in
    let x_pruned = modify_itv x_v x_itv in
    Mem.update mode global x_lv x_pruned mem
  | Lval x ->
    let x_lv = eval_lv pid x mem in
    let x_v = Mem.lookup x_lv mem in
    let x_itv = itv_of_val x_v in
    let pos_x = Itv.meet x_itv (Itv.V (Itv.Int 1, Itv.PInf)) in
    let neg_x = Itv.meet x_itv (Itv.V (Itv.MInf, Itv.Int (-1))) in
    let x_itv = Itv.join pos_x neg_x in
    let x_pruned = modify_itv x_v x_itv in
    Mem.update mode global x_lv x_pruned mem
  | _ -> mem

let prune : AbsDom.update_mode -> Global.t -> Proc.t -> exp -> Mem.t -> Mem.t
= fun mode global pid cond mem ->
  match make_cond_simple cond with
  | None -> mem
  | Some cond_simple -> prune_simple mode global pid cond_simple mem


