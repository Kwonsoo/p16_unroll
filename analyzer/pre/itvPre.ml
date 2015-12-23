open Vocab
open Cil
open Global
open AbsDom
open AbsSem
open EvalOp
open EvalOp
open Prune 
open RetOp
open ItvDom
open ArrayBlk

module ItvAccessSem =
struct 
  module Access = Access.Make(Loc)

  (** {6 def/use set on memory operation} *)

  let duof_mem_lookup : PowLoc.t -> Mem.t -> Access.t -> Access.t
  = fun locs mem access ->
    if Mem.eq mem Mem.bot then access else Access.add_set Access.USE locs access

  let duof_mem_update : update_mode -> Global.t -> PowLoc.t -> Access.t -> Access.t
  = fun mode global locs access ->
    let strong_add locs access = Access.add_set Access.DEF locs access in
    let weak_add locs access = Access.add_set Access.ALL locs access in
  
    if PowLoc.can_strong_update mode (Global.is_rec global) locs then strong_add locs access else
      weak_add locs access


  let duof_bind_lvar : update_mode -> Global.t -> LVar.t -> Access.t -> Access.t
  = fun mode global lvar access ->
    let l = PowLoc.singleton (Loc.loc_of_var (Var.var_of_lvar lvar)) in
    duof_mem_update mode global l access

  let duof_bind_arg_ids : update_mode -> Global.t -> LVar.t list -> Access.t -> Access.t
  = fun mode global arg_ids access ->
    list_fold (duof_bind_lvar mode global) arg_ids access

  let duof_bind_arg_ids_set : update_mode -> Global.t -> (LVar.t list) BatSet.t
    -> Val.t list -> Mem.t -> Access.t -> Access.t
  = fun mode global arg_ids_set vs mem access ->
    let is_same_length l = List.length l = List.length vs in
    let arg_ids_set = BatSet.filter is_same_length arg_ids_set in
    let mode = if BatSet.cardinal arg_ids_set > 1 then Weak else mode in
    BatSet.fold (duof_bind_arg_ids mode global) arg_ids_set access


  (** {6 def/use set on eval} *)

  let rec duof_resolve_offset : Proc.t -> Val.t -> Cil.offset -> Mem.t -> Access.t
    -> Access.t
  = fun pid v os mem access ->
    match os with
    | Cil.NoOffset -> access
    | Cil.Field (f, os') ->
      let l1 = pow_loc_append_field (pow_loc_of_val v) f.Cil.fname in
      let l2 = pow_loc_of_struct_w_field (array_of_val v) f.Cil.fname in
      let v = val_of_pow_loc (PowLoc.join l1 l2) in
      access |> duof_resolve_offset pid v os' mem
    | Cil.Index (e, os') ->
      let pow_loc = pow_loc_of_val v in
      let a = array_of_val (Mem.lookup pow_loc mem) in
      let v = val_of_pow_loc (ArrayBlk.pow_loc_of_array a) in
      access |> duof_eval pid e mem |>  duof_mem_lookup pow_loc mem
      |> duof_resolve_offset pid v os' mem

  and duof_eval : Proc.t -> Cil.exp -> Mem.t -> Access.t
    -> Access.t
  = fun pid e mem access ->
    match e with
    | Cil.Const c -> access
    | Cil.Lval l ->
      access |> duof_eval_lv pid l mem
      |> duof_mem_lookup (eval_lv pid l mem) mem
    | Cil.SizeOf _ -> access
    (* TODO: type information is required for precise semantics of SizeOfE.  *)
    | Cil.SizeOfE e -> duof_eval pid e mem access
    | Cil.SizeOfStr _ -> access
    | Cil.AlignOf _ -> access
    (* TODO: type information is required for precise semantics of AlignOfE.  *)
    | Cil.AlignOfE e -> duof_eval pid e mem access
    | Cil.UnOp (u, e, _) -> duof_eval pid e mem access
    | Cil.BinOp (b, e1, e2, _) ->
      access |> duof_eval pid e1 mem |> duof_eval pid e2 mem
    | Cil.Question (e1, e2, e3, _) ->
      let i1 = itv_of_val (eval pid e1 mem) in
      let access = duof_eval pid e1 mem access in
      if Itv.is_bot i1 then
        access
      else if Itv.eq (itv_of_int 0) i1 then
        duof_eval pid e3 mem access
      else if not (Itv.le (itv_of_int 0) i1) then
        duof_eval pid e2 mem access
      else 
        access |> duof_eval pid e2 mem |> duof_eval pid e3 mem
    | Cil.CastE (_, e) -> duof_eval pid e mem access
    | Cil.AddrOf l -> duof_eval_lv pid l mem access
    | Cil.AddrOfLabel _ ->
      invalid_arg "evaOp.ml:duof_eval AddrOfLabel mem.  Not suppoerted."
    | Cil.StartOf l ->
      access |> duof_eval_lv pid l mem
      |> duof_mem_lookup (eval_lv pid l mem) mem

  and duof_eval_lv : Proc.t -> Cil.lval -> Mem.t -> Access.t
    -> Access.t
  = fun pid lv mem access ->
    let (v, access) =
      match fst lv with
      | Cil.Var vi ->
        let x =
          if vi.Cil.vglob then Var.var_of_gvar vi.Cil.vname else
            Var.var_of_lvar (pid, vi.Cil.vname) in
        (val_of_pow_loc (PowLoc.singleton (Loc.loc_of_var x)), access)
      | Cil.Mem e -> (eval pid e mem, duof_eval pid e mem access)
    in
    duof_resolve_offset pid v (snd lv) mem access

  let duof_eval_list : Proc.t -> Cil.exp list -> Mem.t -> Access.t
    -> Access.t
  = fun pid exps mem access ->
    list_fold (fun e acc -> duof_eval pid e mem acc) exps access
  
  let duof_eval_alloc : Proc.t -> IntraCfg.Cmd.alloc -> Mem.t -> Access.t
    -> Access.t
  = fun pid a mem access ->
    match a with
    | IntraCfg.Cmd.Array e -> duof_eval pid e mem access

  (** {6 def/use set on prune} *)
  
  let rec duof_prune_simple : update_mode -> Global.t -> Proc.t -> exp -> Mem.t
    -> Access.t -> Access.t
  = fun mode global pid cond mem access ->
    match cond with
    | BinOp (op, Lval x, e, t)
      when op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne ->
      let x_lv = eval_lv pid x mem in
      access 
      |> duof_eval_lv pid x mem
      |> duof_mem_lookup x_lv mem
      |> duof_eval pid e mem
      |> duof_mem_update mode global x_lv
    | BinOp (op, e1, e2, _) when op = LAnd || op = LOr ->
      access |> duof_prune_simple mode global pid e1 mem
      |> duof_prune_simple mode global pid e2 mem
    | UnOp (LNot, Lval x, _)
    | Lval x ->
      let x_lv = eval_lv pid x mem in
      access
      |> duof_eval_lv pid x mem
      |> duof_mem_lookup x_lv mem
      |> duof_mem_update mode global x_lv
    | _ -> access

  let duof_prune : update_mode -> Global.t -> Proc.t -> exp -> Mem.t -> Access.t
    -> Access.t
  = fun mode global pid cond mem access ->
    match make_cond_simple cond with
    | None -> access
    | Some cond_simple -> 
      access 
      |> duof_prune_simple mode global pid cond_simple mem 

  let duof_refute : update_mode -> Global.t -> Proc.t -> IntraCfg.Cmd.alarm_exp -> Mem.t -> Access.t -> Access.t
  = fun mode global pid r mem access ->
    match r with 
    | IntraCfg.Cmd.DerefExp (Lval lv, _)  
    | IntraCfg.Cmd.ArrayExp (lv, Const _, _)
    | IntraCfg.Cmd.DerefExp (BinOp (_, Lval lv, Const _, _), _) ->
        let x_lv = eval_lv pid lv mem in
        access |> duof_eval_lv pid lv mem 
        |> duof_mem_lookup x_lv mem |> duof_mem_update mode global x_lv
    | IntraCfg.Cmd.ArrayExp (lv, Lval i, _)
    | IntraCfg.Cmd.ArrayExp (lv, (BinOp (_, Lval i, _, _)), _)
    | IntraCfg.Cmd.ArrayExp (lv, (BinOp (_, _, Lval i, _)), _)
    | IntraCfg.Cmd.DerefExp (BinOp (_, Lval lv, Lval i, _), _)
    | IntraCfg.Cmd.DerefExp (BinOp (_, Lval lv, (BinOp (_, _, Lval i, _)), _), _)
    | IntraCfg.Cmd.DerefExp (BinOp (_, Lval lv, (BinOp (_, Lval i, _, _)), _), _) ->
        let x_lv = eval_lv pid lv mem in
        let x_i = eval_lv pid i mem in
        access |> duof_eval_lv pid lv mem |> duof_eval_lv pid i mem 
        |> duof_mem_lookup x_lv mem |> duof_mem_lookup x_i mem
        |> duof_mem_update mode global x_lv |> duof_mem_update mode global x_i
    | _ -> access

  (** {6 def/use set for return semantics} *)

  let duof_remove_local_variables : update_mode -> Global.t -> Proc.t -> Mem.t -> Access.t
    -> Access.t
  = fun mode global pid mem access ->
    match mode with
    | Weak -> access
    | Strong ->
      if Global.is_rec global pid then access else
        let locals = Mem.keys (Mem.filter (is_local pid) mem) in
        Access.add_set Access.DEF locals access

  let duof_model_realloc mode pid (lvo, exps) (mem, global) access =
    match lvo with
    | Some lv ->
      begin
        match exps with
        | _::size::_ ->
          access |> duof_eval_lv pid lv mem
          |> duof_eval_alloc pid (IntraCfg.Cmd.Array size) mem
          |> duof_mem_update mode global (eval_lv pid lv mem)
        | _ -> failwith "arguments of realloc are not given"
      end
    | _ -> access

  let duof_model_calloc mode pid (lvo, exps) (mem, global) access =
    match lvo with
    | Some lv ->
      begin
        match exps with
        | size::_ ->
          access |> duof_eval_lv pid lv mem
          |> duof_eval_alloc pid (IntraCfg.Cmd.Array size) mem
          |> duof_mem_update mode global (eval_lv pid lv mem)
        | _ -> failwith "arguments of realloc are not given"
      end
    | _ -> access

  let duof_model_recv mode pid (lvo, exps) (mem, global) access = 
    match lvo with
    | Some lv ->
      begin
        match exps with
        | _::_::size::_ -> 
          access |> duof_eval_lv pid lv mem
        |> duof_eval pid size mem
        |> duof_mem_update mode global (eval_lv pid lv mem) 
        | _ -> access
      end
    | _ -> access

  let duof_model_strlen mode pid (lvo, exps) (mem, global) access =
    match lvo with
    | Some lv ->
      begin
      match exps with
      | _::src::_ -> 
        access |> duof_eval_lv pid lv mem
        |> duof_eval pid src mem
        |> duof_mem_update mode global (eval_lv pid lv mem)
      | _ -> access
      end
    | _ -> access

  let duof_model_scanf mode pid exps (mem, global) access =
      begin
        match exps with
        | _::t ->
          list_fold (fun e acc ->
            match e with
            | Cil.AddrOf lv -> 
              begin
                let allocsite = Allocsite.allocsite_of_ext None in
                let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
                acc 
                |> duof_eval_lv pid lv mem
                |> duof_mem_update mode global (eval_lv pid lv mem)
                |> duof_mem_update mode global ext_loc
              end
            | _ -> acc
          ) t access
        | _ -> failwith "arguments of realloc are not given"
      end

  let duof_model_fscanf mode pid exps (mem, global) access =
      begin
        match exps with
        | _::_::t ->
          list_fold (fun e acc ->
            match e with
            | Cil.AddrOf lv -> 
              begin
                let allocsite = Allocsite.allocsite_of_ext None in
                let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
                acc 
                |> duof_eval_lv pid lv mem
                |> duof_mem_update mode global (eval_lv pid lv mem)
                |> duof_mem_update mode global ext_loc
              end
            | _ -> acc
          ) t access
        | _ -> failwith "arguments of realloc are not given"
      end


  let duof_handle_undefined_functions mode node pid (lvo, f, exps) (mem, global) access =
    match f.vname with
    | "zoo_print"
    | "airac_print" -> access
    | "zoo_dump" -> access
    | "realloc" -> duof_model_realloc mode pid (lvo, exps) (mem, global) access
    | "calloc" -> duof_model_calloc mode pid (lvo, exps) (mem, global) access
    | "strlen" -> duof_model_strlen mode pid (lvo, exps) (mem, global) access
    | "scanf" -> duof_model_scanf mode pid exps (mem, global) access
    | "fscanf" -> duof_model_fscanf mode pid exps (mem, global) access
    | "recv" -> duof_model_recv mode pid (lvo, exps) (mem, global) access
    | _ ->
      (match lvo with
       | None -> access
       | Some lv ->
         let allocsite = Allocsite.allocsite_of_ext (Some f.vname) in
         let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
         access |> duof_eval_lv pid lv mem
         |> duof_mem_update mode global (eval_lv pid lv mem)
         |> duof_mem_update mode global ext_loc)

  let duof_update_nullpos pid mode mem global lv e access = 
    match lv with
    (Var vi, Index (idx, _)) -> 
      let l = eval_lv pid (Var vi, NoOffset) mem in
      access 
      |> duof_eval_lv pid (Var vi, NoOffset) mem 
      |> duof_mem_lookup l mem
      |> duof_mem_update mode global l
    | _ -> access

  let accessof ?(mode = Weak) ?(locset = BatSet.empty) : Global.t -> Node.t -> Mem.t -> Access.t
  = fun global node mem ->
    let pid = Node.get_pid node in
    let access = Access.empty in
    match InterCfg.cmdof global.icfg node with
    | IntraCfg.Cmd.Cinstr _ -> invalid_arg "absSem.ml: duof Cinstr"
    | IntraCfg.Cmd.Cif _ -> invalid_arg "absSem.ml: duof Cif" 
    | IntraCfg.Cmd.CLoop _ -> invalid_arg "absSem.ml: duof CLoop"
    | IntraCfg.Cmd.Cset (l, e, _) ->
      access |> duof_eval_lv pid l mem |> duof_eval pid e mem
      |> duof_mem_update mode global (eval_lv pid l mem)
      |> duof_update_nullpos pid mode mem global l e
    | IntraCfg.Cmd.Cexternal (l, _) ->
      let allocsite = Allocsite.allocsite_of_ext None in
      let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
      access |> duof_eval_lv pid l mem
      |> duof_mem_update mode global (eval_lv pid l mem)
      |> duof_mem_update mode global ext_loc
    | IntraCfg.Cmd.Calloc (l, a, _, _) ->
      access |> duof_eval_lv pid l mem |> duof_eval_alloc pid a mem
      |> duof_mem_update mode global (eval_lv pid l mem)
    | IntraCfg.Cmd.Csalloc (l, s, _) ->
      let allocsite = Allocsite.allocsite_of_node node in
      let pow_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
      access |> duof_eval_lv pid l mem |> duof_mem_update mode global pow_loc
      |> duof_mem_update mode global (eval_lv pid l  mem)
    | IntraCfg.Cmd.Cfalloc (l, fd, _) ->
      access |> duof_eval_lv pid l mem 
      |> duof_mem_update mode global (eval_lv pid l mem)
    | IntraCfg.Cmd.Cassume (e, _) 
    | IntraCfg.Cmd.Cassert (e, _) ->
      access
      |> duof_prune mode global pid e mem
      |> duof_eval pid e mem            (* NOTE: for inspection *)
    | IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f,Cil.NoOffset), arg_exps, loc)
      when f.vname = "zoo_refute" ->
      let r = List.hd (AlarmExp.c_exp (List.hd arg_exps) loc) in
      access |> duof_refute mode global pid r mem
    | IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f,Cil.NoOffset), arg_exps, _)
      when Global.is_undef f.vname global ->
      access 
      |> duof_eval_list pid arg_exps mem (* NOTE: for inspection *)
      |> duof_handle_undefined_functions mode node pid (lvo, f, arg_exps) (mem, global)
    | IntraCfg.Cmd.Ccall (lvo, f, arg_exps, _) ->
      let fs = pow_proc_of_val (eval pid f mem) in
      let access = duof_eval pid f mem access in
      if PowProc.eq fs PowProc.bot then access else
        let arg_lvars_of_proc f acc =
          let args = InterCfg.argsof global.icfg f in
          let lvars = List.map (fun x -> (f,x)) args in
          BatSet.add lvars acc in
        let arg_lvars_set = PowProc.fold arg_lvars_of_proc fs BatSet.empty in
        let arg_vals = eval_list pid arg_exps mem in
        let access = duof_eval_list pid arg_exps mem access in
        let access =
          match lvo with
          | None -> access
          | Some lv -> duof_eval_lv pid lv mem access
        in
        duof_bind_arg_ids_set mode global arg_lvars_set arg_vals mem access
    | IntraCfg.Cmd.Creturn (ret_opt, _) ->
      let (access, mem) =
        match ret_opt with
        | None -> (access, mem)
        | Some e ->
          let ret_locs = Dump.find (InterCfg.Node.get_pid node) global.dump in
          let access = access |> duof_eval pid e mem
                       |> duof_mem_update Weak global ret_locs in
          let mem = Mem.update mode global ret_locs (eval pid e mem) mem in
          (access, mem) in
      duof_remove_local_variables mode global pid mem access
    | IntraCfg.Cmd.Casm _ -> access        (* Not supported *)
    | IntraCfg.Cmd.Cskip -> access
end
