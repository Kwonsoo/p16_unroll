open Vocab
open Cil
open AbsSem
open AbsDom
open Global
open EvalOp
open ItvDom
open ArrayBlk
open IntraCfg
open Cmd
open Itv

type refined_vals = (Loc.t * Val.t) BatSet.t

let refine_size mode : Global.t -> PowLoc.t -> Val.t -> Itv.t -> Itv.t -> Mem.t -> refined_vals -> refined_vals  
= fun global lv value size idx mem set ->
  let (arr, ploc) = (array_of_val value, pow_loc_of_val value) in
  match (idx, size) with
    (V (Int lo, _), V (ls, us)) when lo >= 0 ->
      let new_size = 
        if Itv.le' (Int (lo+1)) ls then size
        else V (Int (lo+1), us) 
      in
      let arr = ArrayBlk.map (fun (o,sz,st) -> (o,Itv.meet sz new_size,st)) arr in
      let v = (Val.join (val_of_array arr) (val_of_pow_loc ploc)) in
			PowLoc.fold (fun loc set -> BatSet.add (loc, v) set) lv set
  | _ -> set

let refine_index mode : Global.t -> PowLoc.t -> Itv.t -> Itv.t -> Mem.t -> refined_vals -> refined_vals
= fun global locs idx size mem set ->
  match (idx, size) with 
    (V (li, ui), V (_, Int us)) ->
      let new_idx =
        let ui' = if Itv.le' ui (Int (us-1)) then ui
                  else Int (us-1) in
        let li' = if Itv.le' li (Int (-1)) then Int 0
                  else li in
        V (li', ui') 
      in
      let v = val_of_itv new_idx in
      PowLoc.fold (fun loc set -> BatSet.add (loc, v) set) locs set
  | _ -> set

let refine_index2 mode : Global.t -> PowLoc.t -> Itv.t -> Itv.t -> Mem.t -> refined_vals -> refined_vals
= fun global locs idx offset mem set -> (*-i < -offset*)
  match (offset, idx) with 
    (V (Int ls, us), V (li, ui)) ->
      let new_idx = 
        if Itv.le' (Int (ls+1)) li  then idx
        else V (Int (ls+1), ui) 
      in
      let v = val_of_itv new_idx in
      PowLoc.fold (fun loc set -> BatSet.add (loc, v) set) locs set
  | _ -> set

let refine_offset mode : Global.t -> PowLoc.t -> Val.t -> Itv.t -> Itv.t -> Mem.t -> refined_vals -> refined_vals
= fun global locs value offset size mem set ->
  let (arr, ploc) = (array_of_val value, pow_loc_of_val value) in
  match (offset, size) with
  | (V (lo, uo), V (_, Int us)) ->
    let new_offset = 
      if Itv.le' (Int 0) lo && (Itv.le' uo (Int (us-1))) then offset
      else if Itv.le' (Int 0) lo then V (lo, Int (us-1)) 
      else V (Int 0, Int (us-1)) 
    in
    let arr = ArrayBlk.map (fun (o,sz,st) -> (Itv.meet o new_offset,sz,st)) arr in
    let v = (Val.join (val_of_pow_loc ploc) (val_of_array arr)) in
    PowLoc.fold (fun loc set -> BatSet.add (loc, v) set) locs set
  | _ -> set

let rec refute mode : Global.t -> AbsDom.Proc.t -> IntraCfg.Cmd.alarm_exp -> ItvDom.Mem.t -> refined_vals
=fun global pid cmd_refute mem ->
  match cmd_refute with (*refining size, offset, index variable*)
  | DerefExp (Lval lv, loc) -> 
    refute mode global pid (DerefExp (BinOp (PlusPI, Lval lv, Cil.zero, Cil.typeOfLval lv), loc)) mem
  | ArrayExp (lv, (Const _ as idx), _)
  | DerefExp (BinOp (IndexPI, Lval lv, (Const _ as idx), _), _) 
  | DerefExp (BinOp (PlusPI, Lval lv, (Const _ as idx), _), _) ->                          (* *(lv + const) *)
      let arr_value = EvalOp.eval pid (Lval lv) mem in
      let (arr, ploc) = (array_of_val arr_value, pow_loc_of_val arr_value) in
      if ArrayBlk.eq arr ArrayBlk.bot then BatSet.empty
      else 
        let (offset, size) = (*join all offset and size in an array block*)
          ArrayBlk.foldi 
            (fun _ (offset,size,_) (joined_offset,joined_size) ->
              (Itv.join offset joined_offset, Itv.join size joined_size)
            ) arr (Itv.bot, Itv.bot) 
        in
        let index = itv_of_val (EvalOp.eval pid idx mem) in
        let lv_locs = eval_lv pid lv mem in
        BatSet.empty |> refine_size mode global lv_locs arr_value size (Itv.plus offset index) mem
        |> refine_offset mode global lv_locs arr_value offset (Itv.minus size index) mem
  | ArrayExp (lv, Lval idx, loc) 
  | DerefExp (BinOp (IndexPI, Lval lv, Lval idx, _), loc) 
  | DerefExp (BinOp (PlusPI, Lval lv, Lval idx, _), loc) -> refute mode global pid (ArrayExp (lv, BinOp (PlusA, Lval idx, Cil.zero, Cil.intType), loc)) mem
  | ArrayExp (lv, (BinOp (PlusA, Lval i, e, _) as idx), _)
  | ArrayExp (lv, (BinOp (PlusA, e, Lval i, _) as idx), _)
  | DerefExp (BinOp (IndexPI, Lval lv, (BinOp (PlusA, e, Lval i, _) as idx), _), _)
  | DerefExp (BinOp (IndexPI, Lval lv, (BinOp (PlusA, Lval i, e, _) as idx), _), _) 
  | DerefExp (BinOp (PlusPI, Lval lv, (BinOp (PlusA, e, Lval i, _) as idx), _), _)
  | DerefExp (BinOp (PlusPI, Lval lv, (BinOp (PlusA, Lval i, e, _) as idx), _), _) ->      (* *(lv + i + e) *)
      let arr_value = EvalOp.eval pid (Lval lv) mem in
      let (arr, ploc) = (array_of_val arr_value, pow_loc_of_val arr_value) in
      if ArrayBlk.eq arr ArrayBlk.bot then BatSet.empty
      else 
        let (offset, size) = (*join all offset and size in an array block*)
          ArrayBlk.foldi 
            (fun _ (offset,size,_) (joined_offset,joined_size) ->
              (Itv.join offset joined_offset, Itv.join size joined_size)
            ) arr (Itv.bot, Itv.bot) 
        in
        let idx_val = itv_of_val (EvalOp.eval pid idx mem) in
        let i_val = itv_of_val (EvalOp.eval pid (Lval i) mem) in
        let (lv_locs, i_locs) = (eval_lv pid lv mem, eval_lv pid i mem) in
        let const = itv_of_val (eval pid e mem) in
        BatSet.empty |> refine_size mode global lv_locs arr_value size (Itv.plus offset idx_val) mem 
        |> refine_offset mode global lv_locs arr_value offset (Itv.minus size idx_val) mem
        |> refine_index mode global i_locs i_val (Itv.minus (Itv.minus size offset) const) mem
  | ArrayExp (lv, (BinOp (MinusA, Lval i, e, _) as idx), _)
  | DerefExp (BinOp (MinusPI, Lval lv, (BinOp (MinusA, e, Lval i, _) as idx), _), _)
  | DerefExp (BinOp (IndexPI, Lval lv, (BinOp (MinusA, Lval i, e, _) as idx), _), _) 
  | DerefExp (BinOp (PlusPI, Lval lv, (BinOp (MinusA, Lval i, e, _) as idx), _), _) ->     (* *(lv + idx - exp) *)
      let arr_value = EvalOp.eval pid (Lval lv) mem in
      let (arr, ploc) = (array_of_val arr_value, pow_loc_of_val arr_value) in
      if ArrayBlk.eq arr ArrayBlk.bot then BatSet.empty
      else 
        let (offset, size) = (*join all offset and size in an array block*)
          ArrayBlk.foldi 
            (fun _ (offset,size,_) (joined_offset,joined_size) ->
              (Itv.join offset joined_offset, Itv.join size joined_size)
            ) arr (Itv.bot, Itv.bot) 
        in
        let index = itv_of_val (EvalOp.eval pid idx mem) in
        let lv_index = itv_of_val (EvalOp.eval pid (Lval i) mem) in
        let (lv_locs, i_locs) = (eval_lv pid lv mem, eval_lv pid i mem) in
        let const = itv_of_val (eval pid e mem) in
        BatSet.empty |> refine_size mode global lv_locs arr_value size (Itv.plus offset index) mem 
        |> refine_offset mode global lv_locs arr_value offset (Itv.minus size index) mem
        |> refine_index mode global i_locs lv_index (Itv.plus (Itv.minus size offset) const) mem
  | DerefExp (BinOp (MinusPI, Lval lv, (BinOp (PlusA, e, Lval i, _) as idx), _), _)
  | DerefExp (BinOp (MinusPI, Lval lv, (BinOp (PlusA, Lval i, e, _) as idx), _), _) ->     (* *(lv - (idx + e)) *)
      let arr_value = EvalOp.eval pid (Lval lv) mem in
      let (arr, ploc) = (array_of_val arr_value, pow_loc_of_val arr_value) in
      if ArrayBlk.eq arr ArrayBlk.bot then BatSet.empty
      else 
        let (offset, size) = (*join all offset and size in an array block*)
          ArrayBlk.foldi 
            (fun _ (offset,size,_) (joined_offset,joined_size) ->
              (Itv.join offset joined_offset, Itv.join size joined_size)
            ) arr (Itv.bot, Itv.bot) 
        in
        let index = itv_of_val (EvalOp.eval pid idx mem) in
        let lv_index = itv_of_val (EvalOp.eval pid (Lval i) mem) in
        let (lv_locs, i_locs) = (eval_lv pid lv mem, eval_lv pid i mem) in
        let const = itv_of_val (eval pid e mem) in
        BatSet.empty |> refine_size mode global lv_locs arr_value size (Itv.minus offset index) mem 
        |> refine_offset mode global lv_locs arr_value offset (Itv.plus size index) mem
        |> refine_index2 mode global i_locs lv_index (Itv.minus (Itv.minus offset size) const) mem
  | ArrayExp (lv, (BinOp (MinusA, e, Lval i, _) as idx), _)
  | DerefExp (BinOp (IndexPI, Lval lv, (BinOp (MinusA, e, Lval i, _) as idx), _), _)
  | DerefExp (BinOp (PlusPI, Lval lv, (BinOp (MinusA, e, Lval i, _) as idx), _), _)
  | DerefExp (BinOp (MinusPI, Lval lv, (BinOp (MinusA, Lval i, e, _) as idx), _), _) ->    (* *(lv + (e - idx)) *)
      let arr_value = EvalOp.eval pid (Lval lv) mem in
      let (arr, ploc) = (array_of_val arr_value, pow_loc_of_val arr_value) in
      if ArrayBlk.eq arr ArrayBlk.bot then BatSet.empty
      else 
        let (offset, size) = (*join all offset and size in an array block*)
          ArrayBlk.foldi 
            (fun _ (offset,size,_) (joined_offset,joined_size) ->
              (Itv.join offset joined_offset, Itv.join size joined_size)
            ) arr (Itv.bot, Itv.bot) 
        in
        let index = itv_of_val (EvalOp.eval pid idx mem) in
        let lv_index = itv_of_val (EvalOp.eval pid (Lval i) mem) in
        let (lv_locs, i_locs) = (eval_lv pid lv mem, eval_lv pid i mem) in
        let const = itv_of_val (eval pid e mem) in
        BatSet.empty |> refine_size mode global lv_locs arr_value size (Itv.minus offset index) mem 
        |> refine_offset mode global lv_locs arr_value offset (Itv.plus size index) mem
        |> refine_index2 mode global i_locs lv_index (Itv.plus (Itv.minus offset size) const) mem
  | _ -> BatSet.empty
