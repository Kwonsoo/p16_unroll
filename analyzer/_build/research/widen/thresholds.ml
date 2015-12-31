open Graph
open Cil
open Global
open AbsDom
open Vocab
open Frontend
open ItvDom
open ItvPre
open ItvSem
open IntraCfg
open IntraCfg.Cmd
open EvalOp
open Itv

module C = Cil
module F = Frontc
module E = Errormsg

module Access = Access.Make (Loc)
module Pre = Pre.Make(ItvAccessSem)
module DUGraph = Dug.MakeSet(InterCfg.Node)(Loc)
module SSA = DugGen.Make(Pre)(DUGraph)
module ItvSparseAnalysis = SparseAnalysis.Make(Pre)(ItvSem)(Table)(DUGraph)

(* a heuristic for thresholds: collect constant array-sizes, array-sizes-1, and 0 *)
let collect_thresholds : Pre.t -> Global.t -> int BatSet.t
=fun pre global -> 
  let mem = Pre.get_mem pre in
  let itvs : Itv.t BatSet.t = 
    Mem.fold (fun v set ->
      let itvs_arrblk = 
        ArrayBlk.fold (fun ai -> BatSet.add (ArrayBlk.ArrInfo.size ai)) (array_of_val v) set in 
        BatSet.union itvs_arrblk set
    ) mem BatSet.empty in
  let bufsizes : int BatSet.t = 
    BatSet.fold (fun (itv: Itv.t) set ->
      match itv with
      | V (Int l, Int u) -> BatSet.add l (BatSet.add u set)
      | V (Int i, _)
      | V (_, Int i) -> BatSet.add i set
      | _ -> set
    ) (itvs : Itv.t BatSet.t) BatSet.empty in
    BatSet.add 0 (BatSet.union (BatSet.map (fun n -> n -1) bufsizes) bufsizes)
