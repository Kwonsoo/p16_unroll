(** Main. *)

open Graph
open Cil
open Global
open AbsDom
open Vocab
open Frontend
open ItvDom
open ItvAnalysis

let get_alarm_type () = 
  match !Options.opt_alarm_type with
  | "bo" -> Report.BO
  | "nd" -> Report.ND
  | "dz" -> Report.DZ 
  | "ptsto" -> Report.PTSTO
  |  _   -> raise (Failure ("Unknown alarm type: " ^ !Options.opt_alarm_type))
 

let print_abslocs_info locs = 
  let lvars = BatSet.filter Loc.is_loc_lvar locs in
  let gvars = BatSet.filter Loc.is_loc_gvar locs in
  let allocsites = BatSet.filter Loc.is_loc_allocsite locs in
  let fields = BatSet.filter Loc.is_loc_field locs in
    prerr_endline ("#abslocs    : " ^ i2s (BatSet.cardinal locs));
    prerr_endline ("#lvars      : " ^ i2s (BatSet.cardinal lvars));
    prerr_endline ("#gvars      : " ^ i2s (BatSet.cardinal gvars));
    prerr_endline ("#allocsites : " ^ i2s (BatSet.cardinal allocsites));
    prerr_endline ("#fields     : " ^ i2s (BatSet.cardinal fields))

let do_sparse_analysis (pre, global) = 
  let _ = prerr_memory_usage () in
  let abslocs = ItvPre.get_total_abslocs pre in
  let _ = print_abslocs_info abslocs in
  let locs_ranked = StepManager.stepf false "Ranking locations" PartialFlowSensitivity.rank (abslocs,pre,global) in
  let locset = PartialFlowSensitivity.take_top !Options.opt_pfs locs_ranked in
  let _ = prerr_endline ("#Selected locations : " ^ i2s (BatSet.cardinal locset)
          ^ " / " ^ i2s (BatSet.cardinal (ItvPre.get_total_abslocs pre))) in
  let dug = StepManager.stepf false "Def-use graph construction" ItvSSA.icfg2dug (global, pre, locset) in
  prerr_memory_usage ();
  prerr_endline ("#Nodes in def-use graph : "
                 ^ i2s (BatSet.cardinal (DUGraph.nodesof dug)));
  prerr_endline ("#Locs  on def-use graph : " ^ i2s (DUGraph.sizeof dug));
  let order = StepManager.stepf_s false false "Workorder computation"
    ItvWorklist.Workorder.perform dug in
  let (inputof, outputof, memFI) =
    ItvSparseAnalysis.perform (global, dug, pre, locset, Table.empty,order) in
  prerr_memory_usage ();
  (inputof, outputof, dug, memFI, locset, order)

let do_itv_analysis : ItvPre.t -> Global.t -> Report.query list
= fun pre global ->
  (* Set widening threshold *)
  let thresholds = 
      if !Options.opt_auto_thresholds then
        Thresholds.collect_thresholds pre global
      else 
        list2set (List.map int_of_string (Str.split (Str.regexp "[ \t]+") !Options.opt_widen_thresholds)) in
  let _ = prerr_endline ("Widening threshold : " ^ string_of_set string_of_int thresholds) in
  let _ = Itv.set_threshold thresholds in
  (* Perform the analysis -- sparse analysis by default *)
  let (inputof, _, _, _, _, _) =
    StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis (pre,global) in
  let inputof = 
    if !Options.opt_deadcode then inputof
    else list_fold (fun n t -> 
           if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
           else t
         ) (InterCfg.nodesof (Global.get_icfg global)) inputof in
 let queries = StepManager.stepf true "Generate report" Report.generate (global,inputof,get_alarm_type()) in 
   queries
 (*
    Report.print !Options.opt_noalarm queries (get_alarm_type())
*)
let preprocess one = 
  let global = try StepManager.stepf true "Translation to graphs" Global.init one with _ -> raise (Failure "Translation") in
  let (pre, global) = StepManager.stepf true "Pre-analysis" ItvPre.do_preanalysis global in
  let candidates = list2set (InterCfg.pidsof (Global.get_icfg global)) in
  let b_inlined = inline candidates !Options.opt_inline one (fun fid -> Global.is_rec global fid) in

  let (pre, global) = 
    if b_inlined then (* something inlined *)
      begin
        (* CFG must be re-computed after inlining *)
        let _ = makeCFGinfo one in
        let global = StepManager.stepf true "Translation to graphs (after inline)" Global.init one in
          StepManager.stepf true "Pre-analysis (after inline)" ItvPre.do_preanalysis global
      end
    else (pre, global) (* nothing inlined *) in

  let pids = InterCfg.pidsof (Global.get_icfg global) in
  let nodes = InterCfg.nodesof (Global.get_icfg global) in

  prerr_endline ("#Procs : " ^ string_of_int (List.length pids));
  prerr_endline ("#Nodes : " ^ string_of_int (List.length nodes));

  (pre, global)


let do_global_analysis () = 
  let one = StepManager.stepf true "Parse-and-merge" Frontend.parse_and_merge () in

  makeCFGinfo one; (*if !E.hadErrors then E.s (E.error "Cabs2cil had some errors");*)
  
  if !Options.opt_il then  ( C.dumpFile !C.printerForMaincil stdout "" one; exit 1);

  let (pre,global) = preprocess one in

  if !Options.opt_cfgs then ( 
     InterCfg.store_cfgs !Options.opt_cfgs_dir global.icfg;
     exit 1);
  if !Options.opt_dug then (
      let dug = ItvSSA.icfg2dug (global, pre, ItvPre.get_total_abslocs pre) in
      let json = `Assoc 
          [ ("callgraph", Callgraph.to_json global.callgraph); 
            ("cfgs", InterCfg.to_json global.icfg);
            ("dugraph", ItvSSA.to_json_local dug pre);
            ("dugraph-inter", ItvSSA.to_json_inter dug pre);
          ]
      in
      Yojson.Safe.pretty_to_channel stdout json;
      exit 1);

  let queries = do_itv_analysis pre global in
    Report.print !Options.opt_noalarm queries (get_alarm_type())

let prerr_header file =
  prerr_endline ("==============================================================================");
  prerr_endline ("  Analyzing a file: " ^ file);
  prerr_endline ("==============================================================================") 

let do_per_file_analysis () = 
  let (s,f,queries) = 
    list_fold (fun file (s,f,queries) ->
      try
        let _ = prerr_header file in
        let one = Frontend.parseOneFile file in
        let _ = makeCFGinfo one in
        let (pre, global) = preprocess one in
          (s+1,f,queries @ do_itv_analysis pre global)
      with _ -> (prerr_endline ("(probably parsing) errors occurred; ignore " ^ file); 
                (s,f+1,queries))
    ) !files (0,0,[]) in
  Report.print !Options.opt_noalarm queries (get_alarm_type());
  prerr_endline ("#Files total    : " ^ string_of_int (s+f));
  prerr_endline ("#Files analyzed : " ^ string_of_int s);
  prerr_endline ("#Files ignored  : " ^ string_of_int f)

let main () =
  let t0 = Sys.time () in
  let _ = Profiler.start_logger () in

  let usageMsg = "Usage: main.native [options] source-files" in

  Printexc.record_backtrace true;

  (* process arguments *)
  Arg.parse Options.opts args usageMsg;
  List.iter (fun f -> prerr_string (f ^ " ")) !files;
  prerr_endline "";
  
  Cil.initCIL ();

  try 
    if !Options.opt_pf then     (* per-file analysis *)
      ignore (do_per_file_analysis ())
    else                        (* global analysis *)
      do_global_analysis ();

    prerr_endline "Finished properly.";
    Profiler.report stdout;
    prerr_endline (string_of_float (Sys.time () -. t0));

  with exc ->
    prerr_endline (Printexc.to_string exc);
    prerr_endline (Printexc.get_backtrace())

let _ = main ()
