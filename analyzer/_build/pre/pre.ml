(** Access Pre-Analysis Framework *)
open Vocab
open Global
open AbsDom
open AbsSem
open Access
open ItvSem
open ItvDom

module type ACCESS_SEMANTICS =
sig
  module Access : ACCESS
  val accessof : ?mode:update_mode -> ?locset: Access.Loc.t BatSet.t -> Global.t -> Node.t -> Mem.t -> Access.t
end

module type PRE = 
sig
  module Access : ACCESS
  type t
  val empty : t
  val get_mem : t -> Mem.t
  val get_total_abslocs : t -> Access.Loc.t BatSet.t
  val get_access : t -> Node.t -> Access.t
  val get_access_proc : t -> Proc.t -> Access.t
  val get_access_reach : t -> Proc.t -> Access.t
  val get_access_local : t -> Proc.t -> Access.Loc.t BatSet.t
  val get_access_local_program : t -> Access.Loc.t BatSet.t
  val get_defs_of : t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  val get_uses_of : t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  val get_single_defs : (Access.Loc.t, Node.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  val filter_out_access : (Node.t, Access.t) BatMap.t -> Access.Loc.t BatSet.t -> (Node.t, Access.t) BatMap.t
  val restrict_access   : t -> Access.Loc.t BatSet.t -> t
  val init_access : ?locset: Access.Loc.t BatSet.t -> Global.t -> Mem.t
    -> Access.Loc.t BatSet.t * (Node.t, Access.t) BatMap.t
  val init_access_proc : (Node.t, Access.t) BatMap.t -> (Proc.t, Access.t) BatMap.t
  val init_access_reach : Proc.t list -> Callgraph.t -> (Proc.t, Access.t) BatMap.t -> (Proc.t, Access.t) BatMap.t
  val init_access_local_proc : (Proc.t, Access.t) BatMap.t -> (Proc.t, Access.Loc.t BatSet.t) BatMap.t
  val init_access_local_program : (Proc.t, Access.Loc.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  val onestep_transfer : bool -> Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
  val fixpt : bool -> Node.t list -> int -> Mem.t * Global.t -> Mem.t * Global.t
  val init_callgraph : Node.t list -> Global.t -> Mem.t -> Global.t
  val do_preanalysis : ?locset: Access.Loc.t BatSet.t -> ?silent:bool -> Global.t -> t * Global.t
end

module Make(AccessSem : ACCESS_SEMANTICS) =
struct
  module Access = AccessSem.Access
  module Loc = Access.Loc

  type t = {
    mem : Mem.t;
    total_abslocs : Access.Loc.t BatSet.t;
    access : (Node.t, Access.t) BatMap.t;
    access_proc : (Proc.t, Access.t) BatMap.t;
    access_reach : (Proc.t, Access.t) BatMap.t;
    access_local_proc : (Proc.t, Access.Loc.t BatSet.t) BatMap.t;
    access_local_program : Access.Loc.t BatSet.t;
    defs_of : (Access.Loc.t, Node.t BatSet.t) BatMap.t;
    uses_of : (Access.Loc.t, Node.t BatSet.t) BatMap.t
  }

  let empty = {
    mem = Mem.bot;
    total_abslocs = BatSet.empty;
    access = BatMap.empty;
    access_proc = BatMap.empty;
    access_reach = BatMap.empty;
    access_local_proc = BatMap.empty;
    access_local_program = BatSet.empty;
    defs_of = BatMap.empty;
    uses_of = BatMap.empty
  }

  let get_mem : t -> Mem.t
  =fun i -> i.mem

  let get_total_abslocs : t -> Loc.t BatSet.t
  =fun i -> i.total_abslocs

  let get_access : t -> Node.t -> Access.t
  =fun i n -> try BatMap.find n i.access with _ -> Access.empty

  let get_access_proc : t -> Proc.t -> Access.t
  =fun i pid -> try BatMap.find pid i.access_proc with _ -> Access.empty

  (* abstract locations exclusively accessed in the given function *)
  let get_access_local : t -> Proc.t -> Loc.t BatSet.t
  =fun i pid -> try BatMap.find pid i.access_local_proc with _ -> BatSet.empty

  let get_access_local_program : t -> Loc.t BatSet.t
  =fun i -> i.access_local_program

  let get_access_reach : t -> Proc.t -> Access.t
  =fun i pid -> try BatMap.find pid i.access_reach with _ -> Access.empty

  let get_defs_of : t -> (Loc.t, Node.t BatSet.t) BatMap.t
  =fun t -> t.defs_of

  let get_uses_of : t -> (Loc.t, Node.t BatSet.t) BatMap.t
  =fun t -> t.uses_of

  let restrict : Loc.t BatSet.t -> (Node.t, Access.t) BatMap.t -> (Node.t, Access.t) BatMap.t
  =fun abslocs map ->
    BatMap.map (fun access -> Access.restrict abslocs access) map

  let compute_access_info ?locset : Global.t -> Mem.t -> Access.Loc.t BatSet.t * (Node.t, Access.t) BatMap.t
  =fun global mem ->
    let nodes = InterCfg.nodesof (Global.get_icfg global) in
    let map =
      list_fold (fun node ->
        BatMap.add node (AccessSem.accessof ~mode:Strong ?locset global node mem)
      ) nodes BatMap.empty in
    let abslocs = BatMap.foldi (fun _ access acc ->
        let acc = set_union_small_big (Access.useof access) acc in
       set_union_small_big (Access.defof access) acc) map BatSet.empty in
    (abslocs, restrict abslocs map)

  let init_access ?locset : Global.t -> Mem.t
    -> Access.Loc.t BatSet.t * (Node.t, Access.t) BatMap.t
  =fun global mem -> compute_access_info ?locset global mem

  let filter_out_access : (Node.t, Access.t) BatMap.t -> Access.Loc.t BatSet.t -> (Node.t, Access.t) BatMap.t
  =fun access locs ->  
    BatMap.foldi (fun node access ->
      BatMap.add node (Access.filter_out locs access) 
    ) access BatMap.empty

  let restrict_access : t -> Access.Loc.t BatSet.t -> t
  =fun t locs ->  
    { t with access = 
        BatMap.foldi (fun node access ->
          BatMap.add node (Access.restrict locs access) 
        ) t.access BatMap.empty }

  let init_access_proc : (Node.t, Access.t) BatMap.t -> (Proc.t, Access.t) BatMap.t
  =fun access ->
    let add_access_proc pid access m =
      BatMap.modify_def Access.empty pid (Access.union access) m in
    let add_access_node node access m =
      add_access_proc (Node.get_pid node) access m in
    BatMap.foldi add_access_node access BatMap.empty

  let make_n2access n2pids access_proc =
    let add_access pid = Access.union (BatMap.find pid access_proc) in
    let access_procs pids = BatSet.fold add_access pids Access.empty in
    BatMap.map access_procs n2pids

  let rec make_n2reach_access n2access n2next_ns cur_n n2reach_access =
    if BatMap.mem cur_n n2reach_access then n2reach_access else
    let next_ns = BatSet.remove cur_n (BatMap.find cur_n n2next_ns) in
    let add_reach_access = make_n2reach_access n2access n2next_ns in
    let n2reach_access = BatSet.fold add_reach_access next_ns n2reach_access in
    let add_access n = Access.union (BatMap.find n n2reach_access) in
    let trans_access = BatSet.fold add_access next_ns Access.empty in
    let cur_access = BatMap.find cur_n n2access in
    let reach_access = Access.union cur_access trans_access in
    BatMap.add cur_n reach_access n2reach_access

  (* NOTE: Not_found cases are unreachable functions from _G_.  They
     will be removed later. *)
  let make_pid2reach_access n2reach_access pid2n =
    let get_reach_access n =
      try BatMap.find n n2reach_access with Not_found -> Access.empty in
    BatMap.map get_reach_access pid2n

  let init_access_reach : InterCfg.pid list -> Callgraph.t
    -> (Proc.t, Access.t) BatMap.t -> (Proc.t, Access.t) BatMap.t
  = fun pids callgraph access_proc ->
    let (scc_num, pid2n_func) = Callgraph.SCC.scc callgraph.Callgraph.graph in
    let calls = callgraph.Callgraph.calls in
    let (pid2n, group_num) = Callgraph.make_pid2n pids pid2n_func scc_num in
    let n2pids = Callgraph.make_n2pids pids group_num pid2n in
    let n2next_pids = Callgraph.make_n2next_pids calls n2pids in
    let n2next_ns = Callgraph.make_n2next_ns n2next_pids pid2n in
    let start_n = BatMap.find "_G_" pid2n in
    let n2access = make_n2access n2pids access_proc in
    let n2reach_access =
      make_n2reach_access n2access n2next_ns start_n BatMap.empty in
    let pid2reach_access = make_pid2reach_access n2reach_access pid2n in
    pid2reach_access

  let init_access_local_proc : (Proc.t, Access.t) BatMap.t
      -> (Proc.t, Access.Loc.t BatSet.t) BatMap.t
  = fun access_proc ->
    let update_loc2proc_1 pid loc (loc2proc, nonlocals) =
      if BatMap.mem loc loc2proc then
        let loc2proc = BatMap.remove loc loc2proc in
        let nonlocals = BatSet.add loc nonlocals in
        (loc2proc, nonlocals)
      else
        let loc2proc = BatMap.add loc pid loc2proc in
        (loc2proc, nonlocals) in
    let update_loc2proc pid acc_of_pid (loc2proc, nonlocals) =
      let locs = BatSet.diff (Access.accessof acc_of_pid) nonlocals in
      BatSet.fold (update_loc2proc_1 pid) locs (loc2proc, nonlocals) in
    let (loc2proc, _) =
      BatMap.foldi update_loc2proc access_proc (BatMap.empty, BatSet.empty) in
    let proc2locs = BatMap.map (fun _ -> BatSet.empty) access_proc in
    let add_loc_pid loc pid = BatMap.modify pid (BatSet.add loc) in
    BatMap.foldi add_loc_pid loc2proc proc2locs

  let init_access_local_program : (Proc.t, Access.Loc.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  =fun access_local_proc ->
    BatMap.foldi (fun proc access -> BatSet.union access) access_local_proc BatSet.empty

  let init_defs_of : (Node.t, Access.t) BatMap.t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  =fun access_map ->
    BatMap.foldi (fun node access defs_of ->
      BatSet.fold (fun loc defs_of ->
        let old_nodes = try BatMap.find loc defs_of with _ -> BatSet.empty
        in  BatMap.add loc (BatSet.add node old_nodes) defs_of
      ) (Access.defof access) defs_of
    ) access_map BatMap.empty

  let init_uses_of : (Node.t, Access.t) BatMap.t -> (Access.Loc.t, Node.t BatSet.t) BatMap.t
  =fun access_map ->
    BatMap.foldi (fun node access uses_of ->
      BatSet.fold (fun loc uses_of ->
        let old_nodes = try BatMap.find loc uses_of with _ -> BatSet.empty
        in  BatMap.add loc (BatSet.add node old_nodes) uses_of
      ) (Access.useof access) uses_of
    ) access_map BatMap.empty

  let onestep_transfer silent : Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
  =fun nodes (mem,global) ->
    list_fold (fun node (mem,global) ->
      ItvSem.run ~silent:silent node (mem,global)
    ) nodes (mem,global) 

  let rec fixpt silent : Node.t list -> int -> Mem.t * Global.t -> Mem.t * Global.t
  =fun nodes k (mem,global) ->
    my_prerr_string silent ("\riteration : " ^ string_of_int k);
    flush stderr;
    let (mem',global') = onestep_transfer silent nodes (mem,global) in
    let mem' = Mem.widen mem mem' in
      if Mem.le mem' mem && Dump.le global'.dump global.dump
      then (my_prerr_newline silent (); (mem',global'))
      else fixpt silent nodes (k+1) (mem',global')

  let callees_of : InterCfg.t -> InterCfg.Node.t -> Mem.t -> PowProc.t
  = fun icfg node mem ->
    let pid = InterCfg.Node.get_pid node in
    let c = InterCfg.cmdof icfg node in
    match c with
    | IntraCfg.Cmd.Ccall (_, Cil.Lval (Cil.Var vi, Cil.NoOffset), _, _)
      when vi.Cil.vname = "zoo_print" -> PowProc.bot
    | IntraCfg.Cmd.Ccall (_, Cil.Lval (Cil.Var vi, Cil.NoOffset), _, _)
      when vi.Cil.vname = "zoo_dump" -> PowProc.bot
    | IntraCfg.Cmd.Ccall (_, e, _, _) ->
      pow_proc_of_val (EvalOp.eval pid e mem)
    | _ -> PowProc.bot

  let add_node_calls : InterCfg.t -> Mem.t -> (InterCfg.Node.t, InterCfg.pid BatSet.t) BatMap.t -> InterCfg.Node.t -> (InterCfg.Node.t, InterCfg.pid BatSet.t) BatMap.t = fun icfg mem m node ->
    if InterCfg.is_callnode node icfg then
      let callees = callees_of icfg node mem in
      BatMap.add node callees m
    else m

  let init_node_calls : InterCfg.t -> InterCfg.Node.t list -> Mem.t -> Callgraph.t -> Callgraph.t
  = fun icfg nodes mem callgraph ->
    let node_calls = List.fold_left (add_node_calls icfg mem) BatMap.empty nodes in
    Callgraph.init_node_calls node_calls callgraph

  let init_callgraph : Node.t list -> Global.t -> Mem.t -> Global.t
  =fun nodes global mem ->
    let icfg = get_icfg global in
    let callgraph = get_callgraph global in
    let callgraph = (init_node_calls icfg nodes mem callgraph) in
    let callgraph = Callgraph.init_graph icfg nodes callgraph in
    let callgraph = Callgraph.init_calls (InterCfg.pidsof icfg) callgraph in
    let callgraph = Callgraph.init_trans_calls (InterCfg.pidsof icfg) callgraph in
    { global with callgraph = callgraph }

  let remove_unreachable_functions silent : t * Global.t -> Global.t 
  =fun (info,global) -> 
    let pids_all = list2set (InterCfg.pidsof global.icfg) in
    let reachable = Callgraph.get_reachable InterCfg.global_proc global.callgraph in
    let unreachable = BatSet.diff pids_all reachable in
    let global = BatSet.fold remove_function unreachable global in

      my_prerr_newline silent ();
      my_prerr_endline silent ("#functions all : " ^ string_of_int (BatSet.cardinal pids_all));
      my_prerr_endline silent ("#unreachable   : " ^ string_of_int (BatSet.cardinal unreachable));
      my_prerr_string silent "unreachable functions : ";
      my_prerr_endline silent (string_of_set id unreachable);
      global

  let remove_unreachable_nodes silent : Global.t -> Global.t
  =fun global ->
    let nodes_all = InterCfg.nodesof global.icfg in
    let unreachable = InterCfg.unreachable_node global.icfg in
    let global = BatSet.fold remove_node unreachable global in

    my_prerr_newline silent ();
    my_prerr_string silent "#nodes all   : ";
    my_prerr_endline silent (string_of_int (List.length nodes_all));
    my_prerr_string silent "#unreachable : ";
    my_prerr_endline silent (string_of_int (BatSet.cardinal unreachable));
(*    prerr_string "unreachable nodes : ";
    prerr_endline (string_of_set InterCfg.Node.to_string unreachable); *)
    global

  let get_single_defs : (Access.Loc.t, Node.t BatSet.t) BatMap.t -> Access.Loc.t BatSet.t
  =fun defs_of -> 
    BatMap.foldi (fun loc nodes -> 
      if BatSet.cardinal nodes = 1 then BatSet.add loc else id
    ) defs_of BatSet.empty

  let do_preanalysis ?(locset = BatSet.empty) ?(silent=false): Global.t -> t * Global.t
  =fun global ->
    let global = remove_unreachable_nodes silent global in
    let nodes = InterCfg.nodesof (get_icfg global) in
    let pids = InterCfg.pidsof (get_icfg global) in
    let (mem,global) = fixpt silent nodes 1 (Mem.bot,global) in
    let _ = my_prerr_endline silent ("mem size : " ^ i2s(Mem.fold (fun _ c -> c+1) mem 0)) in
    let _ = if !Options.opt_debug then 
              BatSet.iter (fun loc ->
                print_endline (ItvDom.Mem.A.to_string loc)
              ) (Mem.keys mem) 
            else () in
    let global = init_callgraph nodes global mem in
    let (total_abslocs, access) = init_access ~locset:locset global mem in
    let _ = my_prerr_endline silent ("#total abstract locations  = " ^ string_of_int (BatSet.cardinal total_abslocs)) in
    let defs_of = init_defs_of access in
    let uses_of = init_uses_of access in
    let access_proc = init_access_proc access in
    let access_reach = init_access_reach pids global.callgraph access_proc in
    let access_local_proc = init_access_local_proc access_proc in
    let access_local_program = init_access_local_program access_local_proc in
    let info = { mem = mem;
                 total_abslocs = total_abslocs;
                 access = access;
                 access_proc = access_proc;
                 access_reach = access_reach;
                 access_local_proc = access_local_proc;
                 access_local_program = access_local_program;
                 defs_of = defs_of;
                 uses_of = uses_of
                 } in
    let global = remove_unreachable_functions silent (info,global) in
    (info, global)
end
