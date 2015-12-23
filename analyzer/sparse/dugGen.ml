(** Def/Use Graph *)

open Vocab
open Global 
open InterCfg
open IntraCfg
open AbsDom
open Yojson.Safe
open Pre
open Dug

module Make(Pre: PRE)(DUGraph : DUG with type vertex = Node.t and type loc = Pre.Access.Loc.t) = 
struct
type node = Index.t
module Loc = Pre.Access.Loc
module Access = Pre.Access
type loc = Loc.t

type dom_fronts = (IntraCfg.node, IntraCfg.node BatSet.t) BatMap.t
type phi_points = (node,  loc BatSet.t) BatMap.t

type access_map = (IntraCfg.node, loc BatSet.t) BatMap.t
type access_map_inv = (loc, IntraCfg.node BatSet.t) BatMap.t

let get_dom_fronts : IntraCfg.t -> dom_fronts 
=fun cfg -> IntraCfg.get_dom_fronts cfg

let get_dom_tree cfg = IntraCfg.get_dom_tree cfg

let get_ordinary_defs : Pre.t -> node -> loc BatSet.t 
=fun preinfo node -> Access.defof (Pre.get_access preinfo node)

let get_ordinary_uses : Pre.t -> node -> loc BatSet.t 
=fun preinfo node -> Access.useof (Pre.get_access preinfo node)

let get_single_defs : Pre.t -> loc BatSet.t
=fun preinfo -> Pre.get_single_defs (Pre.get_defs_of preinfo)

let get_def_points_of : Pre.t -> loc -> InterCfg.node BatSet.t
=fun preinfo x ->
  try BatMap.find x (Pre.get_defs_of preinfo)
  with _ -> BatSet.empty

let get_use_points_of : Pre.t -> loc -> InterCfg.node BatSet.t
=fun preinfo x -> 
  try BatMap.find x (Pre.get_uses_of preinfo)
  with _ -> BatSet.empty

let locals_of_function : Pre.t -> pid -> loc BatSet.t
=fun preinfo pid -> Pre.get_access_local preinfo pid

let locals_of_program : Pre.t -> loc BatSet.t
=fun preinfo -> Pre.get_access_local_program preinfo 

let uses_of_function : Pre.t -> pid -> loc BatSet.t -> loc BatSet.t
=fun preinfo pid locset -> 
  let uses = Access.useof (Pre.get_access_reach preinfo pid) in
    BatSet.intersect locset uses

let defs_of_function : Pre.t -> pid -> loc BatSet.t -> loc BatSet.t
=fun preinfo pid locset -> 
  let defs = Access.defof (Pre.get_access_reach preinfo pid) in
    BatSet.intersect locset defs

let access_of_function : Pre.t -> pid -> Loc.t BatSet.t -> loc BatSet.t
=fun preinfo pid locset ->
  let defs = Access.defof (Pre.get_access_reach preinfo pid) in
  let uses = Access.useof (Pre.get_access_reach preinfo pid) in
  let access = BatSet.union defs uses in
    BatSet.intersect locset access

let find_callees : node -> Global.t -> Pre.t -> pid list
=fun node global _ -> Global.get_callees node global

(* TODO: filter local variables *)
let get_local_locations : Global.t -> Pre.t -> pid -> loc BatSet.t
=fun global preinfo pid -> Access.accessof (Pre.get_access_proc preinfo pid)

(** locations considered as being used in the given node *)
let lhsof : Global.t * Pre.t -> node -> loc BatSet.t -> loc BatSet.t 
=fun (global,preinfo) node locset -> 
  let (pid,cfgnode) = Index.get_pid node, Index.get_cfgnode node in
  let icfg = Global.get_icfg global in
  let ordinary_defs = get_ordinary_defs preinfo node in
  (* entry defines access(f) *)
  let defs_at_entry = 
    if IntraCfg.is_entry cfgnode 
    then access_of_function preinfo pid locset
    else BatSet.empty in
  (* return nodes define defs(callees) *)
  let defs_at_return =  
    if InterCfg.is_returnnode node icfg then
      List.fold_left (fun acc callee ->
        BatSet.union (defs_of_function preinfo callee locset) acc
      ) BatSet.empty (find_callees (InterCfg.callof node icfg) global preinfo)
    else BatSet.empty
  in
    BatSet.union ordinary_defs (BatSet.union defs_at_entry defs_at_return)

(** locations considered as being defined in the given node *)
let rec rhsof : Global.t * Pre.t -> node -> loc BatSet.t -> loc BatSet.t
=fun (global,preinfo) node locset -> 
  let (pid,cfgnode) = Index.get_pid node, Index.get_cfgnode node in
  let icfg = Global.get_icfg global in
  let ordinary_uses = get_ordinary_uses preinfo node in
  (* exit uses defs(f) *)
  let uses_at_exit = 
    if IntraCfg.is_exit cfgnode 
    then defs_of_function preinfo pid locset
    else BatSet.empty in
  (* return node inside recursive functions uses local variables of pid *)
  let uses_at_rec_return = 
    if InterCfg.is_returnnode node icfg && Global.is_rec global pid then
      get_local_locations global preinfo pid
    else BatSet.empty in
  (* return node uses defined variables(e.g. arguments) of call node *)
  let uses_at_return1 =
    if InterCfg.is_returnnode node icfg then
      lhsof (global, preinfo) (InterCfg.callof node icfg) locset
    else BatSet.empty in
  (* return node uses not-localized variables of call node *)
  let uses_at_return2 =
    if InterCfg.is_returnnode node icfg then
      let call_node = InterCfg.callof node icfg in
      let callees = find_callees call_node global preinfo in
      let defs_of_callees =
        let add_defs callee =
          BatSet.union (defs_of_function preinfo callee locset) in
        list_fold add_defs callees BatSet.empty in
      let add_not_defined callee =
        let defs = defs_of_function preinfo callee locset in
        BatSet.union (BatSet.diff defs_of_callees defs) in
      list_fold add_not_defined callees BatSet.empty
    else BatSet.empty in
  (* call nodes uses access(callees) *)
  let uses_at_call = 
    if InterCfg.is_callnode node icfg then
      List.fold_left (fun acc callee ->
        BatSet.union (access_of_function preinfo callee locset) acc
      ) BatSet.empty (find_callees node global preinfo)
    else BatSet.empty 
  in
  list_fold BatSet.union 
    [ ordinary_uses; uses_at_exit; uses_at_call; uses_at_rec_return;
      uses_at_return1; uses_at_return2 ]
    BatSet.empty
 
let prepare_defs_uses : Global.t * Pre.t * loc BatSet.t -> IntraCfg.t -> access_map * access_map
=fun (global,preinfo,locset) cfg -> 
  let collect f = 
    List.fold_left (fun m node -> 
      BatMap.add node (f (global,preinfo) (Index.make (IntraCfg.get_pid cfg) node) locset) m
    ) BatMap.empty (IntraCfg.nodesof cfg) in
    (collect lhsof, collect rhsof)
 
let prepare_defnodes : Global.t * Pre.t -> IntraCfg.t -> access_map -> access_map_inv
=fun (global,preinfo) cfg defs_of ->
  try
    list_fold (fun node ->
        BatSet.fold (fun addr map ->
            let set = try BatMap.find addr map with _ -> BatSet.empty in
            BatMap.add addr (BatSet.add node set) map
          ) (BatMap.find node defs_of))
      (IntraCfg.nodesof cfg)
      BatMap.empty
  with _ -> failwith "Dug.prepare_defnodes" 

let get_phi_points cfg preinfo dom_fronts (defs_of,uses_of,defnodes_of) : phi_points =
  let pid = IntraCfg.get_pid cfg in
  let variables = BatMap.foldi (fun k _ -> BatSet.add k) defnodes_of BatSet.empty in
  let map_set_add k v map = 
    try BatMap.add k (BatSet.add v (BatMap.find k map)) map
    with _ -> BatMap.add k (BatSet.singleton v) map in
  let rec iterate_node w var itercount hasalready work joinpoints = 
    match w with
      | node::rest ->
          let (rest,hasalready,work,joinpoints) = 
            BatSet.fold (fun y (rest,hasalready,work,joinpoints) ->
                if BatMap.find y hasalready < itercount then
                  let joinpoints = map_set_add (Index.make pid y) var joinpoints  in
                  let hasalready = BatMap.add y itercount hasalready in
                  let (work,rest) = 
                    if BatMap.find y work < itercount then
                      (BatMap.add y itercount work, y::rest)
                    else (work,rest) in
                  (rest, hasalready,work,joinpoints)
                else (rest,hasalready,work,joinpoints)
              ) (BatMap.find node dom_fronts) (rest,hasalready,work,joinpoints) in
          let (hasalready,work,joinpoints) = iterate_node rest var itercount hasalready work joinpoints in
          (hasalready,work,joinpoints)
      | [] -> (hasalready,work,joinpoints) in
  let rec iterate_variable vars itercount hasalready work joinpoints = 
    match vars with
      | v::rest ->
          let itercount = itercount + 1 in
          let (w,work) = BatSet.fold (fun node (w,work) -> 
                                      (node::w, BatMap.add node itercount work)
                                   ) (BatMap.find v defnodes_of) ([],work) in
          let (hasalready, work, joinpoints) = iterate_node w v itercount hasalready work joinpoints in 
            iterate_variable rest itercount hasalready work joinpoints
      | [] -> joinpoints in
  let init_vars = (BatSet.elements variables) in
  let init_hasalready = list_fold (fun x -> BatMap.add x 0) (IntraCfg.nodesof cfg) BatMap.empty in
  let init_work = list_fold (fun x -> BatMap.add x 0) (IntraCfg.nodesof cfg) BatMap.empty in
  let init_itercount = 0 in
    iterate_variable init_vars init_itercount init_hasalready init_work BatMap.empty

let cfg2dug : Global.t * Pre.t * loc BatSet.t -> IntraCfg.t -> DUGraph.t -> DUGraph.t
=fun (global,preinfo,locset) cfg dug ->
  Profiler.start_event "DugGen.cfg2dug init";
  let pid = IntraCfg.get_pid cfg in
  let dom_fronts = get_dom_fronts cfg in
  let dom_tree = get_dom_tree cfg in
  Profiler.finish_event "DugGen.cfg2dug init";
  Profiler.start_event "DugGen.cfg2dug prepare_du";
  let (node2defs,node2uses) = prepare_defs_uses (global,preinfo,locset) cfg in
  Profiler.finish_event "DugGen.cfg2dug prepare_du";
  Profiler.start_event "DugGen.cfg2dug prepare_def";
  let loc2defnodes = prepare_defnodes (global,preinfo) cfg node2defs in
  Profiler.finish_event "DugGen.cfg2dug prepare_def";
  Profiler.start_event "DugGen.cfg2dug get_phi";
  let phi_points = get_phi_points cfg preinfo dom_fronts (node2defs,node2uses,loc2defnodes) in
  Profiler.finish_event "DugGen.cfg2dug get_phi";
  let defs_of node = BatMap.find node node2defs in
  let uses_of node = BatMap.find node node2uses in
  let phi_vars_of cfgnode = try BatMap.find (Index.make pid cfgnode) phi_points with _ -> BatSet.empty in
  let draw_from_lastdef loc2lastdef loc here dug = 
    try 
      let src = Index.make pid (BatMap.find loc loc2lastdef) in
      let dst = Index.make pid here in
        if BatSet.mem loc locset then DUGraph.add_absloc src loc dst dug
        else dug
    with _ -> dug in
  let rec search loc2lastdef node dug = 
    let uses = uses_of node in 
    let non_phi_uses = BatSet.diff uses (phi_vars_of node) in
    (* 1: draw non-phi uses from their last definition points 
     *    do not draw for phi-vars since their the last defpoints are the
     *    current node *)
    Profiler.start_event "DugGen.cfg2dug draw_from_lastdef";
    let dug = 
        BatSet.fold (fun loc ->
          draw_from_lastdef loc2lastdef loc node
        ) non_phi_uses dug in
    Profiler.finish_event "DugGen.cfg2dug draw_from_lastdef";

    (* 2: update the last definitions:
     * (1) phi-variables are defined here
     * (2) lhs are defined here *)
    Profiler.start_event "DugGen.cfg2dug loc2lastdef";
    let loc2lastdef = 
      BatSet.fold (fun loc ->
        BatMap.add loc node
      ) (BatSet.union (defs_of node) (phi_vars_of node)) loc2lastdef in
    Profiler.finish_event "DugGen.cfg2dug loc2lastdef";
        
    (* 3: draw phi-vars of successors from their last definitions *)
    Profiler.start_event "DugGen.cfg2dug draw phi";
    let dug = 
      list_fold (fun succ ->
        BatSet.fold (fun phi_var ->
          draw_from_lastdef loc2lastdef phi_var succ
       ) (phi_vars_of succ) 
      ) (IntraCfg.succ node cfg) dug in
    Profiler.finish_event "DugGen.cfg2dug draw phi";

    (* 4: process children of dominator trees *)
    BatSet.fold (search loc2lastdef) (IntraCfg.children_of_dom_tree node dom_tree) dug 
  in search BatMap.empty IntraCfg.Node.ENTRY dug

let draw_intraedges ?(silent = false) : Global.t * Pre.t * Loc.t BatSet.t -> DUGraph.t -> DUGraph.t 
=fun (global,preinfo,locset) dug ->
  Profiler.start_event "DugGen.draw_intraedges";
  let n_pids = List.length (InterCfg.pidsof (Global.get_icfg global)) in
  my_prerr_endline silent "draw intra-procedural edges";
  let r =snd (
      InterCfg.fold_cfgs (fun pid cfg (k,dug) ->
        prerr_progressbar silent k n_pids;
        (k+1,cfg2dug (global,preinfo,locset) cfg dug)
      ) (Global.get_icfg global) (1,dug)) in
  Profiler.finish_event "DugGen.draw_intraedges";
  r

let draw_interedges ?(silent = false) : Global.t * Pre.t * Loc.t BatSet.t -> DUGraph.t -> DUGraph.t
=fun (global,preinfo,locset) dug -> 
  let calls = InterCfg.callnodesof global.icfg in
  let n_calls = List.length calls in
  my_prerr_endline silent "draw inter-procedural edges";
  snd (
  list_fold (fun call (k,dug) ->
    prerr_progressbar silent k n_calls;
    let return = InterCfg.returnof call global.icfg in
      (k+1, list_fold (fun callee dug ->
        let entry = InterCfg.entryof global.icfg callee in
        let exit  = InterCfg.exitof  global.icfg callee in
        let locs_on_call = access_of_function preinfo callee locset in
        let locs_on_return = defs_of_function preinfo callee locset in
          dug
          |> DUGraph.add_abslocs call locs_on_call entry
          |> DUGraph.add_abslocs exit locs_on_return return
      ) (Global.get_callees call global) dug)
  ) calls (1,dug))

let draw_singledefs : Global.t * Pre.t * loc BatSet.t -> InterCfg.Node.t list -> DUGraph.t -> DUGraph.t
=fun (global,preinfo,locset) nodes dug -> 
  let nodes = list2set nodes in
  let single_defs = BatSet.intersect locset (get_single_defs preinfo) in
    BatSet.fold (fun x dug ->
      let def_points = get_def_points_of preinfo x in
      let _ = assert (BatSet.cardinal def_points = 1) in
      let def_point = BatSet.choose def_points in
      let use_points = get_use_points_of preinfo x in
        BatSet.fold (fun use_point ->
          if BatSet.mem def_point nodes && BatSet.mem use_point nodes
          then DUGraph.add_absloc def_point x use_point
          else id
        ) use_points dug
    ) single_defs dug

let icfg2dug ?(silent = false) ?(skip_nodes = BatSet.empty) : Global.t * Pre.t * Loc.t BatSet.t  -> DUGraph.t 
=fun (global,preinfo,locset) ->
  let nodes = InterCfg.nodesof global.icfg in
  let preinfo = Pre.restrict_access preinfo locset in
  (DUGraph.init nodes (Pre.get_total_abslocs preinfo))
  |> draw_intraedges ~silent:silent (global,preinfo,locset)
  |> draw_interedges ~silent:silent (global,preinfo,locset)

let to_json_local : DUGraph.t -> Pre.t -> json
= fun g preinfo ->
  let nodes = `List (DUGraph.fold_node (fun v nodes -> 
                (`String (Index.to_string v))::nodes) g []) 
  in
  let edges = `List (DUGraph.fold_edges (fun src dst edges ->
                let spid = InterCfg.Node.get_pid src in
                let dpid = InterCfg.Node.get_pid dst in
                if spid = dpid then
                  let addrset = DUGraph.get_abslocs src dst g in
                  let access_proc = Pre.get_access_proc preinfo spid in
                  let localset = BatSet.filter (fun x -> 
                                  Access.mem x access_proc) addrset in
                  if BatSet.is_empty localset then edges
                  else
                    (`List [`String (Index.to_string src); `String (Index.to_string dst); 
                            `String (BatSet.fold (fun addr s -> (Loc.to_string addr)^","^s) localset "")])
                    ::edges
                else edges
                ) g [])
  in
  `Assoc [("nodes", nodes); ("edges", edges)]

let to_json_inter : DUGraph.t -> Pre.t -> json
= fun g preinfo ->
  let nodes = `List (DUGraph.fold_node (fun v nodes -> 
                (`String (Index.to_string v))::nodes) g []) 
  in
  let edges = `List (DUGraph.fold_edges (fun src dst edges ->
                let spid = InterCfg.Node.get_pid src in
                let dpid = InterCfg.Node.get_pid dst in
                if not (spid = dpid) then
                  let addrset = DUGraph.get_abslocs src dst g in
                  let access_proc = Pre.get_access_proc preinfo spid in
                  let localset = BatSet.filter (fun x -> 
                                  Access.mem x access_proc) addrset in
                  if BatSet.is_empty localset then edges
                  else
                    (`List [`String (Index.to_string src); `String (Index.to_string dst); 
                            `String (BatSet.fold (fun addr s -> (Loc.to_string addr)^","^s) localset "")])
                    ::edges
                else edges
                ) g [])
  in
  `Assoc [("nodes", nodes); ("edges", edges)]
end
