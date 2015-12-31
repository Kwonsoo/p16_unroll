(** Abstract domain. *)

open Lat
open Sum
open Prod
open Pow
open DMap
open DList
open DUnit
open Vocab

module Node = InterCfg.Node
module GVar = DStr
module Proc =
struct
  include DStr
  let equal = (=)
  let hash = Hashtbl.hash
  let compare = String.compare
end
module LVar = ProdSet (Proc) (DStr)

module Field = DStr
module Fields = 
struct 
  include DList (Field)
  let to_string fs =
    list_fold (fun f acc -> acc ^ "." ^ Field.to_string f) fs ""
end

module ExtAllocsite =
struct
  include SumSet (DUnit) (DStr)
  let to_string = function
    | Inl _ -> "__extern__"
    | Inr s -> "__extern__" ^ DStr.to_string s
  let to_json : t -> Yojson.Safe.json = fun x ->
    let this_to_string = to_string in
    Yojson.Safe.(`String (this_to_string x))
end
module Allocsite = 
struct 
  include SumSet (Node) (ExtAllocsite)
  let allocsite_of_node : Node.t -> t = fun n -> Inl n
  let allocsite_of_ext : string option -> t = fun fid_opt ->
    let ext_allocsite_of_ext = function
      | None -> ExtAllocsite.Inl ()
      | Some fid -> ExtAllocsite.Inr fid in
        Inr (ext_allocsite_of_ext fid_opt)
end

module Ctx = DList (Node)
module Index = Node (* ProdSet (Node) (Ctx) *)
module IndexPair = ProdSet (Index) (Index)
module DGraph = Pow (IndexPair)

module Var = 
struct 
  include SumSet (GVar) (LVar)
  let var_of_gvar : GVar.t -> t = fun x -> Inl x
  let var_of_lvar : LVar.t -> t = fun x -> Inr x
end

let is_gvar : Var.t -> bool = function
  | Var.Inl _ -> true
  | _ -> false

let is_lvar : Var.t -> bool = function
  | Var.Inr _ -> true
  | _ -> false


module VarAllocsite = 
struct 
  include SumSet (Var) (Allocsite)
  let is_var = function Inl _ -> true | _ -> false
  let is_allocsite = function Inr _ -> true | _ -> false
  let is_global = function Inl (Var.Inl _) -> true | _ -> false
  let get_pid = function Inl (Var.Inl _) -> InterCfg.global_proc 
  | Inl (Var.Inr (pid, _)) -> pid
  | Inr _ -> raise (Failure "pid of allocsite")
end

module Loc = 
struct 
  include ProdSet (VarAllocsite) (Fields)
  let get_var_allocsite = fst
  let get_fields = snd

  let to_string (var, fields) = 
    VarAllocsite.to_string var ^ Fields.to_string fields
  let to_json x = 
    let this_to_string = to_string in
    Yojson.Safe.(`String (this_to_string x))
  let dummy_loc = (VarAllocsite.Inl (Var.Inl "dummy"), [])
  let null = (VarAllocsite.Inl (Var.Inl "null"), [])

  let is_loc_gvar : t -> bool = function
    | (VarAllocsite.Inl v, _) -> is_gvar v
    | _ -> false

  let is_loc_lvar : t -> bool = function
    | (VarAllocsite.Inl v, _) -> is_lvar v
    | _ -> false 

  let is_loc_allocsite : t -> bool = function
    | (VarAllocsite.Inr _, _) -> true
    | _ -> false

  let is_loc_node_alloc : t -> bool = function
    | (VarAllocsite.Inr (Allocsite.Inl _), _) -> true
    | _ -> false

  let is_loc_ext_alloc : t -> bool = function
    | (VarAllocsite.Inr (Allocsite.Inr _), _) -> true
    | _ -> false

  let is_loc_field : t -> bool = function
    | (_,fl) when fl <> [] -> true
    | _ -> false

  let loc_of_var : Var.t -> t = fun x -> (VarAllocsite.Inl x, [])
  let loc_of_allocsite : Allocsite.t -> t = fun x -> (VarAllocsite.Inr x, [])
  let append_field : t -> Field.t -> t = fun l f -> (fst l, snd l @ [f])

  let can_strong_update_lv : (Proc.t -> bool) -> t -> bool = fun is_rec lv ->
    let x = get_var_allocsite lv in
    (VarAllocsite.is_var x) &&
      ((VarAllocsite.is_global x) || not (is_rec (VarAllocsite.get_pid x)))
end

module PowField = Pow (Field)

type update_mode =
  | Weak
  | Strong


module PowLoc = 
struct 
  include Pow (Loc)
  let can_strong_update : update_mode -> (Proc.t -> bool) -> t -> bool
  = fun mode is_rec lvs ->
    match mode,lvs with
    | Weak, _ -> false
    | Strong, lvs' ->
      if BatSet.cardinal lvs' = 1 then
        Loc.can_strong_update_lv is_rec (BatSet.choose lvs')
      else false
  let prune : Cil.binop -> t -> Cil.exp -> t
  =fun op x e ->
    match op with 
      Cil.Eq when Cil.isZero e -> singleton Loc.null
    | Cil.Ne when Cil.isZero e -> remove Loc.null x
    | _ -> x
end

module PowProc = Pow (Proc)

module Dump = DMap (Proc) (PowLoc)

let pow_loc_append_field : PowLoc.t -> Field.t -> PowLoc.t = fun ls f ->
  let add_appended l acc = PowLoc.add (Loc.append_field l f) acc in
  PowLoc.fold add_appended ls PowLoc.bot
