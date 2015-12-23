open Yojson.Safe
open AbsDom
open Vocab

module Callgraph = struct

  (* TODO: check performane of Graph.imperative *)
  module G = Graph.Persistent.Digraph.Concrete (Proc)
  module SCC = Graph.Components.Make (G)

  type t = { node_calls : (InterCfg.Node.t, InterCfg.pid BatSet.t) BatMap.t ;
             graph : G.t ;
             calls : (InterCfg.pid, InterCfg.pid BatSet.t) BatMap.t ;
             trans_calls : (InterCfg.pid, InterCfg.pid BatSet.t) BatMap.t }

  let empty = { node_calls = BatMap.empty ;
                graph = G.empty ;
                calls = BatMap.empty ;
                trans_calls = BatMap.empty }

  let init_node_calls : (InterCfg.Node.t, InterCfg.pid BatSet.t) BatMap.t -> t -> t
  = fun node_calls callgraph->
    { callgraph with node_calls = node_calls }

  let init_graph : InterCfg.t -> InterCfg.Node.t list ->  t -> t = fun icfg nodes callgraph ->
    let add_call_edge caller callee graph = G.add_edge graph caller callee in
    let add_call_edges callnode callees graph =
      let caller = InterCfg.Node.get_pid callnode in
      BatSet.fold (add_call_edge caller) callees graph in
    let graph = BatMap.foldi add_call_edges callgraph.node_calls G.empty in
    { callgraph with graph = graph }
 
  let init_calls : InterCfg.pid list -> t -> t = fun pids callgraph ->
    let init_map =
      let add_empty_set m pid = BatMap.add pid BatSet.empty m in
      List.fold_left add_empty_set BatMap.empty pids in
    let calls_add caller callee calls =
      let callees = BatMap.find caller calls in
      let callees = BatSet.add callee callees in
      BatMap.add caller callees calls in
    let calls = G.fold_edges calls_add callgraph.graph init_map in
    { callgraph with calls = calls }

  let make_pid2n pids pid2n_func scc_num =
    let add_pid2n (m, n) pid =
      let (k, n) = try (pid2n_func pid, n) with Not_found -> (n, n + 1) in
      (BatMap.add pid k m, n)
    in
    List.fold_left add_pid2n (BatMap.empty, scc_num) pids

  let make_n2pids pids group_num pid2n =
    let n2pids_empty =
      let rec n2pids_empty_rec n m =
        if n <= 0 then m else
          let n' = n - 1 in
          n2pids_empty_rec n' (BatMap.add n' BatSet.empty m) in
      n2pids_empty_rec group_num BatMap.empty in
    let add_n2pid m pid =
      let n = BatMap.find pid pid2n in
      let pids = BatSet.add pid (BatMap.find n m) in
      BatMap.add n pids m in
    List.fold_left add_n2pid n2pids_empty pids

  let make_n2next_pids calls n2pids =
    let add_pid2next_pids pid acc = BatSet.union acc (BatMap.find pid calls) in
    let pids2next_pids pids = BatSet.fold add_pid2next_pids pids BatSet.empty in
    BatMap.map pids2next_pids n2pids

  let make_n2next_ns n2next_pids pid2n =
    let add_pid2n pid acc = BatSet.add (BatMap.find pid pid2n) acc in
    let pids2ns pids = BatSet.fold add_pid2n pids BatSet.empty in
    BatMap.map pids2ns n2next_pids

  let make_n2trans_ns group_num n2next_ns =
    let rec one_update_n2trans_ns n m =
      if BatMap.mem n m then m else
        let next_ns = BatMap.find n n2next_ns in
        let (is_rec, next_ns') =
          if BatSet.mem n next_ns then (true, BatSet.remove n next_ns) else
            (false, next_ns) in
        let m = BatSet.fold one_update_n2trans_ns next_ns' m in
        let add_trans_ns n acc = BatSet.union acc (BatMap.find n m) in
        let trans_ns = next_ns in
        let trans_ns = BatSet.fold add_trans_ns next_ns' trans_ns in
        BatMap.add n trans_ns m
    in
    let rec all_update_n2trans_ns n m =
      if n <= 0 then m else
        let n' = n - 1 in
        let m = one_update_n2trans_ns n' m in
        all_update_n2trans_ns n' m
    in
    all_update_n2trans_ns group_num BatMap.empty

  let make_n2trans_pids n2trans_ns n2pids =
    let add_n2pids n acc = BatSet.union acc (BatMap.find n n2pids) in
    let ns2pids ns = BatSet.fold add_n2pids ns BatSet.empty in
    BatMap.map ns2pids n2trans_ns

  let make_trans_calls n2trans_pids n2pids =
    let add_pid2trans_pids trans_pids pid acc = BatMap.add pid trans_pids acc in
    let add_n2trans_pids n trans_pids acc =
      let pids = BatMap.find n n2pids in
      BatSet.fold (add_pid2trans_pids trans_pids) pids acc in
    BatMap.foldi add_n2trans_pids n2trans_pids BatMap.empty

  let init_trans_calls : InterCfg.pid list -> t -> t = fun pids callgraph ->
    let (scc_num, pid2n_func) = SCC.scc callgraph.graph in
    let calls = callgraph.calls in

    let (pid2n, group_num) = make_pid2n pids pid2n_func scc_num in
    let n2pids = make_n2pids pids group_num pid2n in
    let n2next_pids = make_n2next_pids calls n2pids in
    let n2next_ns = make_n2next_ns n2next_pids pid2n in
    let n2trans_ns = make_n2trans_ns group_num n2next_ns in
    let n2trans_pids = make_n2trans_pids n2trans_ns n2pids in
    let trans_calls = make_trans_calls n2trans_pids n2pids in

    { callgraph with trans_calls = trans_calls }

  let is_rec : t -> InterCfg.pid -> bool = fun callgraph pid ->
    try 
    let trans_calls_of_pid = BatMap.find pid callgraph.trans_calls in
    BatSet.mem pid trans_calls_of_pid
    with _ -> true (* conservative answer for exceptional cases (e.g., unreachable functions) *)

  let get_reachable : InterCfg.pid -> t -> InterCfg.pid BatSet.t
  =fun pid t -> BatSet.add pid (BatMap.find pid t.trans_calls)

  let to_json : t -> json 
  = fun g ->
    let nodes = `List (G.fold_vertex (fun v nodes -> 
              (`String (Proc.to_string v))::nodes) g.graph [])
    in
    let edges = `List (G.fold_edges (fun v1 v2 edges ->
              (`List [`String (Proc.to_string v1); 
                      `String (Proc.to_string v2)
                     ])::edges) g.graph []) in
    `Assoc [("nodes", nodes); ("edges", edges)]

  let remove_function : InterCfg.pid -> t -> t = fun pid callgraph ->
    let is_not_pid_node node _ = InterCfg.Node.get_pid node <> pid in
    { node_calls = BatMap.filter is_not_pid_node callgraph.node_calls
    ; graph = G.remove_vertex callgraph.graph pid
    ; calls = BatMap.remove pid callgraph.calls
    ; trans_calls = BatMap.remove pid callgraph.trans_calls }
end

type t = {
  file : Cil.file;
  icfg : InterCfg.t;
  callgraph : Callgraph.t;
  dump : Dump.t;
}

let dummy = {
  file = Cil.dummyFile;
  icfg = InterCfg.dummy;
  callgraph = Callgraph.empty;
  dump = Dump.empty;
}

let get_icfg : t -> InterCfg.t
=fun pgm -> pgm.icfg

let get_callgraph : t -> Callgraph.t
=fun pgm -> pgm.callgraph

let init file = 
  let global = 
  { file = file;
    icfg = InterCfg.init file;
    callgraph = Callgraph.empty;
    dump = Dump.empty; } in
  let global = { global with icfg = InterCfg.compute_dom_and_scc (Optil.perform global.icfg) } in
  global 

let is_rec : t -> InterCfg.pid -> bool = fun global pid ->
  Callgraph.is_rec (get_callgraph global) pid

let is_undef : InterCfg.pid -> t -> bool = fun pid global ->
  InterCfg.is_undef pid global.icfg

let get_callees : InterCfg.node -> t -> InterCfg.pid list
=fun node t -> 
  try BatSet.elements (BatMap.find node t.callgraph.Callgraph.node_calls) 
  with _ -> []

let get_leaf_procs : t -> string BatSet.t
=fun global ->
  let icfg = get_icfg global in
  let pids = list2set (InterCfg.pidsof icfg) in
  let cg = get_callgraph global in
  let leafs = BatSet.fold (fun fid -> 
        if BatSet.cardinal (Callgraph.get_reachable fid cg) = 1 
        then BatSet.add fid
        else id) pids BatSet.empty
  in  leafs
 
let remove_function : InterCfg.pid -> t -> t = fun pid global ->
  { global with
    icfg = InterCfg.remove_function pid global.icfg
  ; callgraph = Callgraph.remove_function pid global.callgraph
  ; dump = Dump.remove pid global.dump }

let remove_node : InterCfg.node -> t -> t = fun node global ->
  { global with
    icfg = InterCfg.remove_node node global.icfg }

let to_json : t -> json
= fun g ->
  `Assoc 
      [ ("callgraph", Callgraph.to_json g.callgraph); 
        ("cfgs", InterCfg.to_json g.icfg)
      ]

let print_json : out_channel -> t -> unit 
= fun chan g ->
  Yojson.Safe.pretty_to_channel chan (to_json g)
