(** Abstract domain. *)
open Cil
open Lat
open Sum
open Prod
open Pow
open DMap
open DList
open AbsDom
open Vocab
open Cil2str

module Val = Prod4 (Itv) (PowLoc) (ArrayBlk) (PowProc)
module Mem = 
struct 
  include DMap (Loc) (Val)

  let name = "Mem"
  let to_string : t -> string = fun x ->
    if BatMap.is_empty x then name^".bot" else
      string_of_map Loc.to_string Val.to_string x

  let lookup : PowLoc.t -> t -> Val.t = fun locs mem ->
    if eq mem bot then Val.bot else
      let find_join loc acc = Val.join acc (find loc mem) in
      PowLoc.fold find_join locs Val.bot

  let update : update_mode -> Global.t -> PowLoc.t -> Val.t -> t -> t
  = fun mode global locs v mem ->
    let strong_add_v lv m = add lv v m in
    let weak_add_v lv m =
      let orig_v = find lv m in
      if Val.le v orig_v then m else add lv (Val.join orig_v v) m in
    if PowLoc.can_strong_update mode (Global.is_rec global) locs then PowLoc.fold strong_add_v locs mem
    else PowLoc.fold weak_add_v locs mem
end

module Table =
struct
  include DMap (Index) (Mem)

  let add idx m tbl = BatMap.add idx (b_hashcons m) tbl
  let find idx m = try BatMap.find idx m with Not_found -> Mem.bot

  let to_string2 : Proc.t BatSet.t -> t -> string = fun funcs x ->
    let is_in_funcs node _ = BatSet.mem (InterCfg.Node.get_pid node) funcs in
    let x = BatMap.filter is_in_funcs x in
    if BatMap.is_empty x then name^".bot" else
      string_of_map Node.to_string Mem.to_string x
end

(** {6 Auxiliary Functions}*)
let itv_of_val : Val.t -> Itv.t = Val.fst
let pow_loc_of_val : Val.t -> PowLoc.t = Val.snd
let array_of_val : Val.t -> ArrayBlk.t = Val.trd
let pow_proc_of_val : Val.t -> PowProc.t = Val.frth
let val_of_itv : Itv.t -> Val.t = fun x ->
  (x, PowLoc.bot, ArrayBlk.bot, PowProc.bot)
let val_of_pow_loc : PowLoc.t -> Val.t = fun x ->
  (Itv.bot, x, ArrayBlk.bot, PowProc.bot)
let val_of_array : ArrayBlk.t -> Val.t = fun x ->
  (Itv.bot, PowLoc.bot, x,PowProc.bot)
let val_of_pow_proc : PowProc.t -> Val.t = fun x ->
  (Itv.bot, PowLoc.bot, ArrayBlk.bot, x)
let itv_of_int : int -> Itv.t = fun i -> Itv.V (Itv.Int i, Itv.Int i)
let itv_of_ints : int -> int -> Itv.t = fun lb ub ->
  Itv.V (Itv.Int lb, Itv.Int ub)

let modify_itv : Val.t -> Itv.t -> Val.t = fun x i ->
  (i, pow_loc_of_val x, array_of_val x, pow_proc_of_val x)

let modify_powloc : Val.t -> PowLoc.t -> Val.t = fun x p ->
  (itv_of_val x, p, array_of_val x, pow_proc_of_val x)

let modify_array : Val.t -> ArrayBlk.t -> Val.t = fun x a ->
  (itv_of_val x, pow_loc_of_val x, a, pow_proc_of_val x)

let modify_powproc : Val.t -> PowProc.t -> Val.t = fun x p ->
  (itv_of_val x, pow_loc_of_val x, array_of_val x, p)


let external_value : Allocsite.t -> Val.t = fun allocsite ->
  let loc = Loc.loc_of_allocsite allocsite in
  let pow_loc = PowLoc.singleton loc in
  let array = ArrayBlk.extern allocsite in
  (Itv.top, pow_loc, array, PowProc.bot)

let make_eraser_value : Itv.t -> Val.t 
=fun itv -> 
  let allocsite = Allocsite.allocsite_of_ext None in
  let loc = Loc.loc_of_allocsite allocsite in
  let pow_loc = PowLoc.singleton loc in 
  let array = ArrayBlk.eraser itv allocsite in
  (itv, pow_loc, array, PowProc.bot)
