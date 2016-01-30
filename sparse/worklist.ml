open Vocab
open Dug
open AbsDom
open Lat

module NGraph = struct
  module NOrder = struct
    type t = int
    let compare = compare
    let hash = Hashtbl.hash
    let equal = ( = )
  end

  module G = Graph.Persistent.Digraph.Concrete (NOrder)
  include G
  include Graph.Components.Make (G)
end


module Make (Node : SET) (DUGraph : DUG with type vertex = Node.t) = struct

module Workorder = struct

type t = {
  order : (DUGraph.vertex, int * bool) BatMap.t;
  headorder: (DUGraph.vertex, int) BatMap.t;
  loopheads : DUGraph.vertex BatSet.t
}

let empty = {
  order = BatMap.empty;
  headorder = BatMap.empty;
  loopheads = BatSet.empty
}

let make : DUGraph.t -> NGraph.t * (int, DUGraph.vertex) BatMap.t
= fun g ->
  let i = ref 0 in
  let new_i () = let v = !i in i := !i + 1; v in
  let create n i2n n2i =
    try (BatMap.find n n2i, i2n, n2i) with Not_found ->
      let i = new_i () in
      (i, BatMap.add i n i2n, BatMap.add n i n2i) in
  let add_edge src dst (ng, i2n, n2i) =
    let (i_src, i2n, n2i) = create src i2n n2i in
    let (i_dst, i2n, n2i) = create dst i2n n2i in
    (NGraph.add_edge ng i_src i_dst, i2n, n2i) in
  let (ng, i2n, _) =
    DUGraph.fold_edges add_edge g (NGraph.empty, BatMap.empty, BatMap.empty) in
  (ng, i2n)

let projection : NGraph.vertex BatSet.t -> NGraph.t -> NGraph.t
= fun scc ng ->
  let add_back_edge e newg =
    let succ = NGraph.succ ng e in
    let succ = List.filter (fun x -> BatSet.mem x scc) succ in
    list_fold (fun s newg -> NGraph.add_edge newg e s) succ newg in
  BatSet.fold add_back_edge scc NGraph.empty

let loophead_of scc ng =
  let add_entry src dst acc =
    if not (BatSet.mem src scc) && BatSet.mem dst scc then BatSet.add dst acc
    else acc in
  let entries = NGraph.fold_edges add_entry ng BatSet.empty in
  let get_score n =
    let preds = NGraph.pred ng n in
    let preds = List.filter (fun n -> BatSet.mem n scc) preds in
    List.length preds in
  let compare_score n (candidate, score) =
    let score_n = get_score n in
    if score_n > score then (n, score_n) else (candidate, score) in
  fst (BatSet.fold compare_score entries (BatSet.choose scc, 0))

let cut_backedges ng entry =
  let preds = NGraph.pred ng entry in
  let cut_edge pred ng = NGraph.remove_edge ng pred entry in
  list_fold cut_edge preds ng

let rec get_order sccs ng (wo, lhs, ho) order =
  match sccs with
  | scc :: t ->
    if List.length scc > 1 then
      let headorder = order + 3 * (List.length scc) in
      let scc = list2set scc in
      let ng' = projection scc ng in
      let lh = loophead_of scc ng in
      let (lhs, ho) = (BatSet.add lh lhs, BatMap.add lh headorder ho) in
      let (wo, lhs, ho, _) = get_order t ng (wo, lhs, ho) (headorder + 1) in
      let ng' = cut_backedges ng' lh in
      let sccs' = List.rev (Array.to_list (NGraph.scc_array ng')) in
      get_order sccs' ng' (wo, lhs, ho) order
    else
      let n = List.hd scc in
      get_order t ng (BatMap.add n order wo, lhs, ho) (order + 1)
  | [] -> (wo, lhs, ho, order)

let is_loopheader here info = BatSet.mem here info.loopheads

let rec perform g =
  let (ng, i2n) = make g in
  let sccs = List.rev (Array.to_list (NGraph.scc_array ng)) in
  let (wo, lhs, ho, _) =
    get_order sccs ng (BatMap.empty, BatSet.empty, BatMap.empty) 0 in

  let add_rec_node src dst nodes =
    if Pervasives.compare src dst = 0 then BatSet.add src nodes else nodes in
  let lhs = NGraph.fold_edges add_rec_node ng lhs in

  let trans_map trans_k trans_v m =
    let add_1 k v = BatMap.add (trans_k k) (trans_v k v) in
    BatMap.foldi add_1 m BatMap.empty in
  let trans_set trans_v s =
    let add_1 v = BatSet.add (trans_v v) in
    BatSet.fold add_1 s BatSet.empty in
  let trans_k k = BatMap.find k i2n in

  let wo = trans_map trans_k (fun k v -> (v, BatSet.mem k lhs)) wo in
  let lhs = trans_set (fun v -> BatMap.find v i2n) lhs in
  let ho = trans_map trans_k (fun _ v -> v) ho in

  { order = wo; headorder = ho; loopheads = lhs }

end


module WorkSet = struct
  type workorder = int * bool

  module Ord = struct
    type t = workorder * DUGraph.vertex

    let compare ((o1, _), v1) ((o2, _), v2) =
      let cmp_o = o1 - o2 in
      let cmp_v = Pervasives.compare v1 v2 in
      if cmp_o = 0 then cmp_v else cmp_o
  end

  include Set.Make (Ord)
end

type t = WorkSet.t
let empty = WorkSet.empty
let cardinal = WorkSet.cardinal
let mem = WorkSet.mem
let compare_order x y = WorkSet.Ord.compare x y <= 0

let queue is_inneredge ho n o ws =
  (* change order if,
     - the n node has a loophead order, and
     - an inneredge to the n node is updated
  *)
  let rec change_order n o is_inneredge =
    let is_loophead = snd o in
    if is_inneredge && is_loophead then
      try (BatMap.find n ho, is_loophead) with Not_found -> o
    else o in
  let new_o = change_order n o is_inneredge in
  WorkSet.add (new_o, n) ws

let init_nodes wo nodes =
  let init_v v = queue false BatMap.empty v (BatMap.find v wo) in
  BatSet.fold init_v nodes WorkSet.empty

let init wo dug =
  let init_v v = queue false BatMap.empty v (BatMap.find v wo) in
  BatSet.fold init_v (DUGraph.nodesof dug) WorkSet.empty

let pick ws =
  try
    let (o, n) = WorkSet.min_elt ws in
    let ws = WorkSet.remove (o, n) ws in
    Some (n, ws)
  with Not_found -> None

end
