open Vocab
open Cil
open AbsSem
open AbsDom
open Global
open EvalOp
open ItvDom
open ArrayBlk

module Dom = Mem

let zoo_print silent pid exps mem loc =
  if silent then ()
  else 
    let vs = eval_list pid exps mem in
    let vs_str = string_of_list Val.to_string vs in
    prerr_endline
      ("zoo_print (" ^ Cil2str.s_location loc ^ ") : "
       ^ vs_str)

let zoo_dump silent mem loc =
  if silent then ()
  else prerr_endline
    ("zoo_dump (" ^ Cil2str.s_location loc ^ ") : "
     ^ Mem.to_string mem)

let model_realloc mode node pid (lvo, exps) (mem, global) =
  match lvo with
  | Some lv ->
    begin
      match exps with
      | _::size::_ -> 
        (Mem.update mode global (eval_lv pid lv mem) (eval_alloc node (IntraCfg.Cmd.Array size) false mem) mem, global)
      | _ -> raise (Failure "Error: arguments of realloc are not given")
    end
  | _ -> (mem,global)

let model_calloc mode node pid (lvo, exps) (mem, global) =
  match lvo with
  | Some lv ->
    begin
      match exps with
      | n::size::_ -> 
        let new_size = Cil.BinOp (Cil.Mult, n, size, Cil.uintType) in
        (Mem.update mode global (eval_lv pid lv mem) (eval_alloc node (IntraCfg.Cmd.Array new_size) false mem) mem, global)
      | _ -> raise (Failure "Error: arguments of realloc are not given")
    end
  | _ -> (mem,global)

let model_recv mode node pid (lvo, exps) (mem, global) =
  match lvo with
  | Some lv ->
    begin
      match exps with
      | _::_::size::_ -> 
        (Mem.update mode global (eval_lv pid lv mem) (eval pid size mem) mem, global)
      | _ -> raise (Failure "Error: arguments of realloc are not given")
    end
  | _ -> (mem,global)


let model_strlen mode node pid (lvo,exps) (mem, global) =
  match lvo with
  | Some lv ->
    begin
      match exps with
      | _::src::_ ->
        (Mem.update mode global (eval_lv pid lv mem) (eval pid src mem) mem, global)
      | _ -> (mem, global)
    end
  | _ -> (mem,global)

let model_scanf mode node pid exps (mem, global) =
  match exps with 
    _::t -> 
      List.fold_left (fun (mem, global) e -> 
          match e with 
            Cil.AddrOf lv -> 
              let allocsite = Allocsite.allocsite_of_ext None in
              let ext_v = ItvDom.external_value allocsite in
              let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
              let mem = Mem.update mode global (eval_lv pid lv mem) ext_v mem in
              let mem = Mem.update mode global ext_loc ext_v mem in
              (mem, global)
          | _ -> (mem,global)) (mem,global) t
  | _ -> (mem, global)

let model_fscanf mode node pid exps (mem, global) =
  match exps with 
    _::_::t -> 
      List.fold_left (fun (mem, global) e -> 
          match e with 
            Cil.AddrOf lv -> 
              let allocsite = Allocsite.allocsite_of_ext None in
              let ext_v = ItvDom.external_value allocsite in
              let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
              let mem = Mem.update mode global (eval_lv pid lv mem) ext_v mem in
              let mem = Mem.update mode global ext_loc ext_v mem in
              (mem, global)
          | _ -> (mem,global)) (mem,global) t
  | _ -> (mem, global)
 
let model_strdup mode node pid (lvo, exps) (mem, global) =
  match lvo with
  | Some lv ->
    begin
      match exps with
      | str::_ -> 
        let allocsite = Allocsite.allocsite_of_node node in
        let str_val = eval pid str mem in
        let size = ArrayBlk.sizeof (ItvDom.array_of_val str_val) in
        let offset = ArrayBlk.offsetof (ItvDom.array_of_val str_val) in
        let nullpos = ArrayBlk.nullposof (ItvDom.array_of_val str_val) in
        let arr_val = ArrayBlk.make allocsite Itv.zero (Itv.minus size offset) Itv.one nullpos in 
        let loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
        let v = Val.join (ItvDom.val_of_array arr_val) (ItvDom.val_of_pow_loc loc) in
        let mem = Mem.update mode global (eval_lv pid lv mem) v mem in
        let mem = Mem.update mode global loc (Mem.lookup (pow_loc_of_val str_val) mem) mem in
        (mem,global)
      | _ -> (mem, global)
    end
  | _ -> (mem,global)
     
 
let handle_undefined_functions mode silent node pid (lvo,f,exps) (mem,global) loc = 
  match f.vname with
  | "zoo_print"
  | "airac_print" -> zoo_print silent pid exps mem loc; (mem,global)
  | "zoo_dump" -> zoo_dump silent mem loc; (mem, global)
  | "realloc" -> model_realloc mode node pid (lvo, exps) (mem, global)
  | "calloc" -> model_calloc mode node pid (lvo, exps) (mem, global)
  | "strlen" -> model_strlen mode node pid (lvo, exps) (mem, global)
  | "scanf" -> model_scanf mode node pid exps (mem, global)
  | "fscanf" -> model_fscanf mode node pid exps (mem, global)
  | "recv" -> model_recv mode node pid (lvo,exps) (mem, global)
(*  | "strdup" -> model_strdup mode node pid (lvo, exps) (mem, global)*)
  | _ -> 
      match lvo with
      | None -> (mem, global)
      | Some lv -> 
        let allocsite = Allocsite.allocsite_of_ext (Some f.vname) in
        let ext_v = ItvDom.external_value allocsite in
        let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
        let mem = Mem.update mode global (eval_lv pid lv mem) ext_v mem in
        let mem = Mem.update mode global ext_loc ext_v mem in
        (mem,global)

let bind_lvar : AbsDom.update_mode -> Global.t -> LVar.t -> Val.t -> Mem.t -> Mem.t
= fun mode global lvar v mem ->
  let l = PowLoc.singleton (Loc.loc_of_var (Var.var_of_lvar lvar)) in
  Mem.update mode global l v mem

let bind_arg_ids : AbsDom.update_mode -> Global.t -> Val.t list -> LVar.t list -> Mem.t -> Mem.t
= fun mode global vs arg_ids mem ->
  list_fold2 (bind_lvar mode global) arg_ids vs mem

(** Binds a list of values to a set of argument lists.  If |args_set|
    > 1, the argument binding does weak update. *)
let bind_arg_lvars_set : AbsDom.update_mode -> Global.t -> (LVar.t list) BatSet.t -> Val.t list -> Mem.t -> Mem.t
= fun mode global arg_ids_set vs mem ->
  let is_same_length l = List.length l = List.length vs in
  let arg_ids_set = BatSet.filter is_same_length arg_ids_set in
  let mode = if BatSet.cardinal arg_ids_set > 1 then AbsDom.Weak else mode in
  BatSet.fold (bind_arg_ids mode global vs) arg_ids_set mem

let update_nullpos pid mode lv e (mem,global) = 
  match lv, itv_of_val (eval pid e mem) with
    (Var vi, Index (idx, _)), itv when itv = Itv.zero ->
      let l = eval_lv pid (Var vi, NoOffset) mem in
      let v = Mem.lookup l mem in
      let arrays = array_of_val v in
      let nullpos = itv_of_val (eval pid idx mem) in
      let arrays = ArrayBlk.update_nullpos arrays nullpos in
      let v = modify_array v arrays in
        Mem.update mode global l v mem
  | _ -> mem

(** Default update option is weak update. *)
let run ?(mode = Weak) ?(locset = BatSet.empty) ?(premem = Mem.bot) ?(silent = false) 
    : Node.t -> Mem.t * Global.t -> Mem.t * Global.t
= fun node (mem, global) ->
  let pid = Node.get_pid node in
  match InterCfg.cmdof global.icfg node with
  | IntraCfg.Cmd.Cinstr _ -> invalid_arg "absSem.ml: run Cinstr" 
  | IntraCfg.Cmd.Cif _ -> invalid_arg "absSem.ml: run Cif" 
  | IntraCfg.Cmd.CLoop _ -> invalid_arg "absSem.ml: run CLoop" 
  | IntraCfg.Cmd.Cset (l, e, _) ->
    let (mem, global) = (Mem.update mode global (eval_lv pid l mem) (eval pid e mem) mem, global) in
      (update_nullpos pid mode l e (mem,global), global)
  | IntraCfg.Cmd.Cexternal (l, _) ->
    let allocsite = Allocsite.allocsite_of_ext None in
    let ext_v = ItvDom.external_value allocsite in
    let ext_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
    let mem = Mem.update mode global (eval_lv pid l mem) ext_v mem in
    let mem = Mem.update mode global ext_loc ext_v mem in
      (mem,global)
  | IntraCfg.Cmd.Calloc (l, a, static, _) ->
    (Mem.update mode global (eval_lv pid l mem) (eval_alloc node a static mem) mem, global)
  | IntraCfg.Cmd.Csalloc (l, s, _) ->
    let allocsite = Allocsite.allocsite_of_node node in
    let pow_loc = PowLoc.singleton (Loc.loc_of_allocsite allocsite) in
    let v1 = eval_string s in
    let v2 = eval_string_loc s allocsite pow_loc in
    let mem = mem |> Mem.update mode global pow_loc v1
              |> Mem.update mode global (eval_lv pid l mem) v2 in
    (mem, global)
  | IntraCfg.Cmd.Cfalloc (l, fd, _) -> 
    let clos = val_of_pow_proc (PowProc.singleton fd.svar.vname) in
    (Mem.update mode global (eval_lv pid l mem) clos mem, global)
  | IntraCfg.Cmd.Cassume (e, _) 
  | IntraCfg.Cmd.Cassert (e, _) -> (Prune.prune mode global pid e mem, global)
  | IntraCfg.Cmd.Ccall (lvo, Cil.Lval (Cil.Var f, Cil.NoOffset), arg_exps, loc)
    when Global.is_undef f.vname global -> (* undefined library functions *)
    handle_undefined_functions mode silent node pid (lvo,f,arg_exps) (mem,global) loc
  | IntraCfg.Cmd.Ccall (lvo, f, arg_exps, _) -> (* user functions *)
    let fs = pow_proc_of_val (eval pid f mem) in
    if PowProc.eq fs PowProc.bot 
    then (mem, global)
    else
      let arg_lvars_of_proc f acc =
        let args = InterCfg.argsof global.icfg f in
        let lvars = List.map (fun x -> (f,x)) args in
        BatSet.add lvars acc in
      let arg_lvars_set = PowProc.fold arg_lvars_of_proc fs BatSet.empty in
      let arg_vals = eval_list pid arg_exps mem in
      let dump =
         match lvo with
         | None -> global.dump
         | Some lv ->
           PowProc.fold (fun f d ->
              Dump.weak_add f (eval_lv pid lv mem) d
           ) fs global.dump in
      let mem = bind_arg_lvars_set mode global arg_lvars_set arg_vals mem in
      (mem, {global with dump = dump})
  | IntraCfg.Cmd.Creturn (ret_opt, _) ->
    let mem =
      match ret_opt with
      | None -> mem
      | Some e ->
        let ret_locs = Dump.find (InterCfg.Node.get_pid node) global.dump in
        let mem = Mem.update Weak global ret_locs (eval pid e mem) mem in
        mem in 
    let mem = RetOp.remove_local_variables mode (Global.is_rec global) pid mem in
    (mem, global)
  | IntraCfg.Cmd.Casm _ -> (mem, global)    (* Not supported *)
  | IntraCfg.Cmd.Cskip -> (mem, global)
