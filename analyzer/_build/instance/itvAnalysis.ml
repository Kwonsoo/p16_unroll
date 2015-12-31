open Graph
open Cil
open Global
open AbsDom
open Vocab
open Frontend
open ItvDom
open ItvPre
open ItvSem

module Access = Access.Make (Loc)
module ItvPre = Pre.Make(ItvAccessSem)
module DUGraph = Dug.MakeSet(InterCfg.Node)(Loc)
module ItvSSA = DugGen.Make(ItvPre)(DUGraph)
module ItvSparseAnalysis = SparseAnalysis.Make(ItvPre)(ItvSem)(Table)(DUGraph)
module ItvWorklist = Worklist.Make(Node)(DUGraph)


