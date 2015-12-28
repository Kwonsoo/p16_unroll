(* Abstract Array Block *)

open Lat
open Sum
open Prod
open Pow
open DMap
open DList
open AbsDom
open Vocab

module ArrInfo = 
struct
  type offset = Itv.t
  and size = Itv.t
  and stride = Itv.t
  include Prod3 (Itv) (Itv) (Itv)
  let top = (Itv.top, Itv.top, Itv.top)
  let eraser itv = (itv, itv, itv)

  let offset = fst
  let size = snd
  let stride = trd

  let make o sz st = (o, sz, st)
  let plus_offset (o, s, st) i = (Itv.plus i o, s, st)
  let minus_offset (o, s, st) i = (Itv.minus o i, s, st)
end

include DMap (Allocsite) (ArrInfo)

let make : Allocsite.t -> Itv.t -> Itv.t -> Itv.t -> t
= fun a o sz st ->
  add a (ArrInfo.make o sz st) bot

let offsetof : t -> Itv.t
= fun a ->
  fold (fun arr -> Itv.join (ArrInfo.offset arr)) a Itv.bot

let sizeof : t -> Itv.t
= fun a ->
  fold (fun arr -> Itv.join (ArrInfo.size arr)) a Itv.bot

let nullposof : t -> Itv.t (* TODO *)
= fun a -> sizeof a

let extern allocsite = 
  add allocsite ArrInfo.top empty

let eraser itv allocsite = 
  add allocsite (ArrInfo.eraser itv) empty

let plus_offset : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.plus_offset a i) arr

let minus_offset : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.minus_offset a i) arr

let cast_array : Itv.t -> t -> t 
= fun new_st a ->
  let resize orig_st x = Itv.divide (Itv.times x orig_st) new_st in
  let cast_offset (o, s, orig_st) =
    let new_o = resize orig_st o in
    let new_s = resize orig_st s in
    (new_o, new_s, new_st) in
  map cast_offset a


let pow_loc_of_array : t -> PowLoc.t = fun array ->
  let pow_loc_of_allocsite k _ acc = BatSet.add (Loc.loc_of_allocsite k) acc in
  foldi pow_loc_of_allocsite array BatSet.empty

let pow_loc_of_struct_w_field : t -> Field.t -> PowLoc.t = fun s f ->
  let add_to_pow_loc a _ = PowLoc.add (Loc.append_field (Loc.loc_of_allocsite a) f) in
  foldi add_to_pow_loc s PowLoc.bot
