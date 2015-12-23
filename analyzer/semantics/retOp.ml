(** Operators for return semantics *)
open AbsSem
open EvalOp
open AbsDom
open ItvDom

let is_local : Proc.t -> Loc.t -> Val.t -> bool = fun pid l _ ->
  match l with
  | (VarAllocsite.Inl (Var.Inr (pid', _)), _) -> pid' = pid
  | _ -> false

let is_not_local : Proc.t -> Loc.t -> Val.t -> bool = fun pid l v ->
  not (is_local pid l v)

let remove_local_variables : AbsDom.update_mode -> (Proc.t -> bool) -> Proc.t -> Mem.t -> Mem.t
= fun mode is_rec pid mem ->
  match mode with
  | Weak -> mem
  | Strong ->
    if is_rec pid then mem else
      Mem.filter (is_not_local pid) mem
