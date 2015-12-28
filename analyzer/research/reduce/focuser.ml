open Cil

let top_expr = Cil.Const (Cil.CInt64 (Int64.of_int 614512, IInt, None)) (* agreed in semantics to produce Top *)

class assignRmVisitor () = object(self)
  inherit nopCilVisitor 
  method vinst (i : instr) = 
      match i with
      | Set((Var vi, NoOffset), _, loc) -> 
        let r = Random.int 100 in
          prerr_endline (string_of_int r);
          if r < 30 then
           ( E.log "%a: transform : %a\n" d_loc loc d_instr i;
            ChangeTo [Set((Var vi, NoOffset), top_expr, loc)] )
          else DoChildren
      | _ -> DoChildren
end
(*
let get_eraser_value (pre, global, inputof_FI, inputof) = 
  match observe (global, inputof_FI, inputof) with
  | OBS_BO (size_FI, offset_FI, index_FI, size_FS, offset_FS, index_FS, _, _, _, _) ->
    Itv.meet (if not (size_FI = size_FS) then size_FI else Itv.top)
      (Itv.meet 
        (if not (offset_FI = offset_FS) then offset_FI else Itv.top)
        (if not (index_FI = index_FS) then index_FI else Itv.top))
  | OBS_PTSTO _ -> Itv.top
  | _ -> Itv.top
 
let decrease_precision : Cil.file -> unit
=fun file -> 
  let analyze file =  (* analyze the file and produce both pre- and main-analysis results *)
    let (pre,global) = init_analysis file in
    let premem = ItvPre.get_mem pre in
    let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis (pre,global) in
      (pre, global, premem, inputof) in
  let (pre, global, premem, inputof) = analyze file in
  let inputof_FI = 
    list_fold (fun n t -> 
      if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
      else t
     ) (InterCfg.nodesof (Global.get_icfg global)) Table.empty in
  let _ = EvalOp.eraser_value := get_eraser_value (pre, global, inputof_FI, inputof) in
  let _ = prerr_endline ("Eraser value : " ^ Itv.to_string !EvalOp.eraser_value) in
  let obs_orig = observe (global, inputof_FI, inputof) in (* original observation for FI/FS difference *)
  let _ = prerr_endline ("ORIGINAL OBSERVATION: " ^ string_of_observe obs_orig) in
  let vis = new assignRmVisitor () in
  let rec iter file k = 
    if k > !Options.opt_dec_prec then file
    else
      let _ = Cil.saveBinaryFile file "/tmp/__backup.cil" in (* backup *)
      let _ = visitCilFile vis file in (* apply transformation once *)
      let (pre,global,premem,inputof) = analyze file in (* analyze the transformed file *)
      let inputof_FI = 
        list_fold (fun n t -> 
          if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
          else t
         ) (InterCfg.nodesof (Global.get_icfg global)) Table.empty in
      let obs_after = observe (global, inputof_FI, inputof) in (* observe again *)
        if obs_after = obs_orig then (* OK, observation is preserved *)
          iter file (k+1)
        else
          let backup = Cil.loadBinaryFile "/tmp/__backup.cil" in
            iter backup (k+1) in
  let transformed_file =  iter file 0 in
  let (pre, global, premem, inputof) = analyze transformed_file in
  let inputof_FI = 
    list_fold (fun n t -> 
      if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
      else t
     ) (InterCfg.nodesof (Global.get_icfg global)) Table.empty in
  let _ = prerr_endline ("FINAL OBSERVATION: " ^ string_of_observe (observe (global, inputof_FI, inputof))) in
    print_cil stdout transformed_file
*)
