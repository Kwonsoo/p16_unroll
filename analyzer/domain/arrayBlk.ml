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
  include Prod4 (Itv) (Itv) (Itv) (Itv) (* the last itv tracks null position *)
  let top = (Itv.top, Itv.top, Itv.top, Itv.top)

  let offset = fst
  let size = snd
  let stride = trd
  let nullpos = frth

  let make o sz st sl = (o, sz, st, sl)
  let plus_offset (o, s, st, sl) i = (Itv.plus i o, s, st, sl)
  let minus_offset (o, s, st, sl) i = (Itv.minus o i, s, st, sl)
  let update_nullpos (o, s, st, np) i = (o, s, st, Itv.join np i)
end

include DMap (Allocsite) (ArrInfo)

let make : Allocsite.t -> Itv.t -> Itv.t -> Itv.t -> Itv.t -> t
= fun a o sz st sl ->
  add a (ArrInfo.make o sz st sl) bot

let offsetof : t -> Itv.t
= fun a ->
  fold (fun arr -> Itv.join (ArrInfo.offset arr)) a Itv.bot

let sizeof : t -> Itv.t
= fun a ->
  fold (fun arr -> Itv.join (ArrInfo.size arr)) a Itv.bot

let nullposof : t -> Itv.t
= fun a -> 
  fold (fun arr -> Itv.join (ArrInfo.nullpos arr)) a Itv.bot

let extern allocsite = 
  add allocsite ArrInfo.top empty 

let plus_offset : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.plus_offset a i) arr

let minus_offset : t -> Itv.t -> t
= fun arr i ->
  map (fun a -> ArrInfo.minus_offset a i) arr

let cast_array : Itv.t -> t -> t 
= fun new_st a ->
  let resize orig_st x = Itv.divide (Itv.times x orig_st) new_st in
  let cast_offset (o, s, orig_st, sl) =
    let new_o = resize orig_st o in
    let new_s = resize orig_st s in
    (new_o, new_s, new_st, sl) in
  map cast_offset a

let update_nullpos : t -> Itv.t -> t
= fun arr nullpos -> 
  map (fun a -> ArrInfo.update_nullpos a nullpos) arr

let pow_loc_of_array : t -> PowLoc.t = fun array ->
  let pow_loc_of_allocsite k _ acc = BatSet.add (Loc.loc_of_allocsite k) acc in
  foldi pow_loc_of_allocsite array BatSet.empty

let pow_loc_of_struct_w_field : t -> Field.t -> PowLoc.t = fun s f ->
  let add_to_pow_loc a _ = PowLoc.add (Loc.append_field (Loc.loc_of_allocsite a) f) in
  foldi add_to_pow_loc s PowLoc.bot
