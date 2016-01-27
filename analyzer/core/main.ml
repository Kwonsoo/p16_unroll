open Graph
open Cil
open Global
open AbsDom
open Vocab
open Frontend
open ItvDom
open ItvAnalysis
open Visitors
open Observe
open Report
open IntraCfg
open InterCfg

open Reduce
open Classify
open Training
open Apply
open Feature
open Types
open Printf

open Depend
open Unroll

module SS = Set.Make(String)

type fifsmap = (Report.query, bool) BatMap.t

let _ = Random.self_init ()

let print_cil out file = C.dumpFile !C.printerForMaincil out "" file

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


let do_sparse_analysis_autopfs : (ItvPre.t * Global.t * Loc.t BatSet.t) -> (Table.t * Table.t * DUGraph.t * Mem.t * Loc.t BatSet.t * ItvWorklist.Workorder.t)
= fun (pre, global, matched_locset) ->
	let _ = prerr_memory_usage () in
	let abslocs = ItvPre.get_total_abslocs pre in
	let _ = print_abslocs_info abslocs in
	(*NOTE*)
	let _ = prerr_endline ("#Selected locations : " ^ i2s (BatSet.cardinal matched_locset)
					^ " / " ^ i2s (BatSet.cardinal (ItvPre.get_total_abslocs pre))) in
	let dug = StepManager.stepf false "Def-use graph construction" ItvSSA.icfg2dug (global, pre, matched_locset) in
	prerr_memory_usage ();
	prerr_endline ("#Nodes in def-use graph : "
								^ i2s (BatSet.cardinal (DUGraph.nodesof dug)));
	prerr_endline ("Locs on def-use graph : " ^ i2s (DUGraph.sizeof dug));
	let order = StepManager.stepf_s false false "Workorder computation" ItvWorklist.Workorder.perform dug in
	let (inputof, outputof, memFI) = ItvSparseAnalysis.perform (global, dug, pre, matched_locset, Table.empty, order) in
	prerr_memory_usage ();
	(inputof, outputof, dug, memFI, matched_locset, order)

let fill_deadcode_with_premem pre global inputof =
  list_fold (fun n t -> 
    if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
    else t
  ) (InterCfg.nodesof (Global.get_icfg global)) inputof 

let is_proved (size,offset,index) =
	let idx = Itv.plus offset index in
	match idx, size with
	| Itv.V (Itv.Int ol, Itv.Int oh), Itv.V (Itv.Int sl, _) ->
		if oh >= sl || ol < 0 then false else true
	| _ -> false

let get_observe_info (buf,idx) node pre premem inputof_FS global =
	let mem_FI = premem in
	let mem_FS = Table.find node inputof_FS in
	let pid = InterCfg.Node.get_pid node in
	let size_FS = ArrayBlk.sizeof (ItvDom.array_of_val (EvalOp.eval pid buf mem_FS)) in
	let size_FI = ArrayBlk.sizeof (ItvDom.array_of_val (EvalOp.eval pid buf mem_FI)) in
	let offset_FS = ArrayBlk.offsetof (ItvDom.array_of_val (EvalOp.eval pid buf mem_FS)) in
	let offset_FI = ArrayBlk.offsetof (ItvDom.array_of_val (EvalOp.eval pid buf mem_FI)) in
	let index_FS = itv_of_val (EvalOp.eval pid idx mem_FS) in
	let index_FI = itv_of_val (EvalOp.eval pid idx mem_FI) in
	let proved_FI = is_proved (size_FI, offset_FI, index_FI) in
	let proved_FS = is_proved (size_FS, offset_FS, index_FS) in
	let buf_name = Cil2str.s_exp buf in
	let idx_name = Cil2str.s_exp idx in
		(size_FI, offset_FI, index_FI, size_FS, offset_FS, index_FS, proved_FI, proved_FS, buf_name, idx_name)
 
let observe (pre, global, premem, inputof_FS) =
	let g = Global.get_icfg global in
	let nodes = InterCfg.nodesof g in
		list_fold (fun n a ->
				match InterCfg.cmdof g n with
				| IntraCfg.Cmd.Ccall (None, Cil.Lval (Cil.Var f, Cil.NoOffset), exps, _) when f.vname = "airac_observe" ->
					begin
						match exps with
						| buf::idx::[] -> get_observe_info (buf,idx) n pre premem inputof_FS global
						| _ -> raise (Failure "airac_observe requires two arguments (buffer and index expressions)")
					end
				| _ -> a
		) nodes (Itv.bot, Itv.bot, Itv.bot, Itv.bot, Itv.bot, Itv.bot, false, false, "", "")
	 
let string_of_observe (size_FI, offset_FI, index_FI, size_FS, offset_FS, index_FS, proved_FI, proved_FS, buf_name, idx_name) =
	"(AIRAC_OBSERVE)" ^
	"FI (" ^ string_of_bool proved_FI ^")" ^
			" Size: " ^ Itv.to_string size_FI ^
			" Offset: "      ^ Itv.to_string offset_FI ^
			" Index : "      ^ Itv.to_string index_FI ^
	" FS (" ^ string_of_bool proved_FS ^ ")" ^
	" Size: " ^ Itv.to_string size_FS ^
	" Offset: "      ^ Itv.to_string offset_FS ^
	" Index : "      ^ Itv.to_string index_FS ^
	" " ^ buf_name ^ " " ^ idx_name ^ "\n"

let do_itv_analysis : ItvPre.t -> Global.t -> unit
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
      else fill_deadcode_with_premem pre global inputof in
  let inputof_FI = fill_deadcode_with_premem pre global Table.empty in
  (* print observation for flow-sensitivity *)
  (*NOTE: observe 타입에 맞지 않게 사용되어 수정함.*)
	(*let _ = print_endline (observe (global, inputof_FI) (global, inputof)) in*)
	let _ = print_endline (string_of_observe (observe (pre, global, ItvPre.get_mem pre, inputof))) in
  let alarm_type = get_alarm_type () in
  let queries_FS = StepManager.stepf true "Generate report (FS)" Report.generate (global,inputof,alarm_type) in 
  let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global,inputof_FI,alarm_type) in
    Report.print !Options.opt_noalarm !Options.opt_diff queries_FS queries_FI alarm_type


let do_itv_analysis_autopfs : ItvPre.t -> Global.t -> Loc.t BatSet.t -> unit
= fun pre global matched_locset ->
	(* Set widening threshold. *)
	let thresholds =
		if !Options.opt_auto_thresholds then Thresholds.collect_thresholds pre global
		else list2set (List.map int_of_string (Str.split (Str.regexp "[ \t]+") !Options.opt_widen_thresholds)) in
	let _ = prerr_endline ("Widening threshold : " ^ string_of_set string_of_int thresholds) in
	let _ = Itv.set_threshold thresholds in
	(*Perform the analysis -- sparse analysis by default *)
	let (inputof, _, _, _, _, _) =
		StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis_autopfs (pre, global, matched_locset) in
	let inputof =
		if !Options.opt_deadcode then inputof
		else list_fold (fun n t ->
				if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
				else t
				) (InterCfg.nodesof (Global.get_icfg global)) inputof in
	let inputof_FI =
		list_fold (fun n t ->
				if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
				else t
				) (InterCfg.nodesof (Global.get_icfg global)) Table.empty in
	let obs = observe (pre, global, ItvPre.get_mem pre, inputof) in
	let _ = print_endline (string_of_observe obs) in
	let alarm_type = get_alarm_type () in
	let queries_FS = StepManager.stepf true "Generate report (FS)" Report.generate (global, inputof, alarm_type) in
	let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global, inputof_FI, alarm_type) in
	Report.print !Options.opt_noalarm !Options.opt_diff queries_FS queries_FI alarm_type

let select_functions_to_inline (pre, global) = 
  if !Options.opt_inline_small_functions then
    let icfg = Global.get_icfg global in
    let pids = InterCfg.pidsof icfg in
    let small p = List.length (IntraCfg.nodesof (InterCfg.cfgof icfg p)) < 50 in
      List.filter small pids
  else !Options.opt_inline

let init_analysis one = 
  let global = StepManager.stepf true "Translation to graphs" Global.init one in
  let (pre, global) = StepManager.stepf true "Pre-analysis" ItvPre.do_preanalysis global in

  let candidates = list2set (InterCfg.pidsof (Global.get_icfg global)) in
  let to_inline = select_functions_to_inline (pre,global) in
  let b_inlined = inline candidates to_inline one (fun fid -> Global.is_rec global fid) in
  if !Options.opt_il then  ( print_cil stdout one; exit 1);

  let (pre, global) = 
    if b_inlined then (* something inlined *)
      begin
        (* CFG must be re-computed after inlining *)
        let _ = makeCFGinfo one in
        let global = StepManager.stepf true "Translation to graphs (after inline)" Global.init one in
          StepManager.stepf true "Pre-analysis (after inline)" ItvPre.do_preanalysis global
      end
    else (pre, global) (* nothing inlined *) in
  (pre, global)

let rec insert_observe_imprecise_fs : Cil.file -> unit
= fun file ->
	match get_alarm_type () with
	| Report.BO -> insert_observe_imprecise_bo_fs file
	| Report.PTSTO -> raise (Failure "not yet implemented")
	| _ -> raise (Failure "insert_observe_imprecise: unsupported alarmtype")

and insert_observe_fs : Cil.file -> unit
=fun file ->
  match get_alarm_type () with
  | Report.BO -> insert_observe_bo_fs file 
  | Report.PTSTO -> insert_observe_ptsto_fs file
  | _ -> raise (Failure "insert_observe: unsupported alarmtype")

and insert_observe_cs : Cil.file -> unit
=fun file -> 
  match get_alarm_type () with
  | Report.BO -> insert_observe_bo_cs file 
  | _ -> raise (Failure "insert_observe: unsupported alarmtype")

and insert_observe_ptsto_fs : Cil.file -> unit
=fun file ->
  let (pre,global) = init_analysis file in
  let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis (pre,global) in
  let inputof = 
    if !Options.opt_deadcode then inputof
    else fill_deadcode_with_premem pre global inputof in
  let inputof_FI = fill_deadcode_with_premem pre global Table.empty in
  let alarm_type = get_alarm_type () in
  let queries_FS = StepManager.stepf true "Generate report (FS)" Report.generate (global,inputof,alarm_type) in 
  let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global,inputof_FI,alarm_type) in
  let (map_FS,map_FS_alexp) = Report.ptstoqs2map queries_FS in (* "loc*alarmexp" -> "ptsto location" set: "" means strings *)
  let (map_FI,map_FI_alexp) = Report.ptstoqs2map queries_FI in
  let no = ref 0 in
  let skipped = ref 0 in
    BatMap.iter (fun key set_FI -> (* key: string representation of loc * alarmexp *)
      let set_FS = try BatMap.find key map_FS with _ -> BatSet.empty in
      let diff = BatSet.diff set_FI set_FS in
        if BatSet.is_empty diff then (* no difference *)
          ()
        else
          begin
            inserted := false;
            let _ = visitCilFile (new removeObserveVisitor ()) file in
            let vis = new insertObserveVisitorPtsto (BatMap.find key map_FI_alexp, key ) in
            let _ = visitCilFile vis file in
              prerr_endline ("inserted: " ^ string_of_bool !inserted);
              if !inserted then 
                begin
                  no:=!no+1;
                  let out = open_out (!Options.opt_dir ^ "/" ^ string_of_int !no ^ ".c") in 
                    print_cil out file; 
                    flush out;
                    close_out out
                end
              else begin
                  skipped := !skipped + 1;
                  prerr_endline ("Skip " ^ key);
              end
          end
    ) map_FI;
    prerr_endline ("Inserted " ^ string_of_int !no ^ " files");
    prerr_endline ("Skipped  " ^ string_of_int !skipped ^ " files")

(* Insert observe for all FI alarms. 
	 NOTE: Context-Sensitivity를 위해서는 또다른 비슷한 함수가 필요하겠지.*)
and insert_observe_imprecise_bo_fs : Cil.file -> unit
= fun file ->
	let (pre,global) = init_analysis file in
	let inputof_FI =
		list_fold (fun n t ->
				if Mem.bot = (Table.find n t) then Table.add n (ItvPre.get_mem pre) t
				else t
			) (InterCfg.nodesof (Global.get_icfg global)) Table.empty in
	let alarm_type = get_alarm_type () in
	let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global,inputof_FI,alarm_type) in
	let fi = Report.get_alarms_fi queries_FI in
	let _ = Report.display_alarms "" fi in
	let no = ref 0 in
	let skipped = ref 0 in
		BatMap.iter (fun loc (al::alarms) ->
				inserted := false;
				let _ = visitCilFile (new removeObserveVisitor ()) file in
				let vis = new insertObserveVisitor (al.Report.exp)
				in  visitCilFile vis file;
					if !inserted then (
						Report.display_alarms "Insert airac_observe for the following alarm" (BatMap.add loc [al] BatMap.empty);
						no:=!no+1;
						let which_benchmark = if (BatString.contains (snd (BatString.rsplit file.fileName "/")) '-')
																	then fst (BatString.split (snd (BatString.rsplit file.fileName "/")) "-")
																	else fst (BatString.split (snd (BatString.rsplit file.fileName "/")) ".") in
						let out = open_out (!Options.opt_dir ^ "/" ^ which_benchmark ^ "_" ^ string_of_int !no ^ ".c") in
							print_cil out file;
							flush out;
							close_out out
					)
					else (
						skipped := !skipped + 1;
						Report.display_alarms "Skip airac_observe for the following alarm" (BatMap.add loc [al] BatMap.empty)
					)
		) fi;
		prerr_endline ("Inserted " ^ string_of_int !no ^ " files");
		prerr_endline ("Skipped  " ^ string_of_int !skipped ^ " files")

(* 이게 기존의 diff에만 observe 삽입하는 insert_observe *)
and insert_observe_bo_fs : Cil.file -> unit
=fun file ->
  let (pre,global) = init_analysis file in
  let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis (pre,global) in
  let inputof = 
    if !Options.opt_deadcode then inputof
    else fill_deadcode_with_premem pre global inputof in
  let inputof_FI = fill_deadcode_with_premem pre global Table.empty in
  let alarm_type = get_alarm_type () in
  let queries_FS = StepManager.stepf true "Generate report (FS)" Report.generate (global,inputof,alarm_type) in 
  let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global,inputof_FI,alarm_type) in
  let diff,_,_ = Report.get_alarm_diff queries_FI queries_FS in
  let _ = Report.display_alarms "" diff in
  let no = ref 0 in
  let skipped = ref 0 in
    BatMap.iter (fun loc (al::_) ->  (* take the first alarm only *)
        inserted := false;
        let _ = visitCilFile (new removeObserveVisitor ()) file in
        let vis = new insertObserveVisitor (al.Report.exp)
        in  visitCilFile vis file;
          if !inserted then (
            Report.display_alarms "Insert airac_observe for the following alarm" (BatMap.add loc [al] BatMap.empty);
            no:=!no+1;
            let out = open_out (!Options.opt_dir ^ "/" ^ string_of_int !no ^ ".c") in 
              print_cil out file; 
              flush out;
              close_out out
              )
          else (
            skipped := !skipped + 1;
            Report.display_alarms "Skip airac_observe for the following alarm" (BatMap.add loc [al] BatMap.empty)
          )
    ) diff;
    prerr_endline ("Inserted " ^ string_of_int !no ^ " files");
    prerr_endline ("Skipped  " ^ string_of_int !skipped ^ " files")

and insert_observe_bo_cs : Cil.file -> unit
=fun file ->
(*prerr_endline ("opt: "^ string_of_bool !Options.opt_insert_observe_save_diff);
if !Options.opt_insert_observe_save_diff then*)
  begin
  prerr_endline "save diff";
  let global = StepManager.stepf true "Translation to graphs" Global.init file in
  let (pre, global) = StepManager.stepf true "Pre-analysis" ItvPre.do_preanalysis global in
  let (inputof_CI, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis without Context-Sensitivity" do_sparse_analysis (pre,global) in

  let inputof_CI = fill_deadcode_with_premem pre global inputof_CI in
  let alarm_type = get_alarm_type () in
  let queries_CI = StepManager.stepf true "Generate report (FSCI)" Report.generate (global,inputof_CI,alarm_type) in
  let _ = Cil.saveBinaryFile file "/tmp/__tmp.cil" in

  let candidates = list2set (InterCfg.pidsof (Global.get_icfg global)) in
  let to_inline = select_functions_to_inline (pre,global) in
  let b_inlined = inline candidates to_inline file (fun fid -> Global.is_rec global fid) in

  let (pre, global) = 
    if b_inlined then (* something inlined *)
      begin
        (* CFG must be re-computed after inlining *)
        let _ = makeCFGinfo file in
        let global = StepManager.stepf true "Translation to graphs (after inline)" Global.init file in
          StepManager.stepf true "Pre-analysis (after inline)" ItvPre.do_preanalysis global
      end
    else (pre, global) (* nothing inlined *) in

  let (inputof_CS, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis with Context-Sensitivity" do_sparse_analysis (pre,global) in
  let inputof_CS = fill_deadcode_with_premem pre global inputof_CS in
  let queries_CS = StepManager.stepf true "Generate report (FSCS)" Report.generate (global,inputof_CS,alarm_type) in 
  let diff,m1,m2 = Report.get_alarm_diff queries_CI queries_CS in
(*  let _ = Marshal.to_channel (open_out ("/tmp/__diff")) diff [] in *)
  let _ = print_endline ("#CI alarms: "^ string_of_int (List.length queries_CI)) in
  let _ = Report.display_alarms "CI" m1 ;
          Report.display_alarms "CS" m2 in
  let _ = Report.display_alarms "Alarm Diff" diff in
(*  end
else (* load from /tmp/__diff the diff results *)
  begin *)
  let no = ref 0 in
(*  let diff = Marshal.from_channel (open_in ("/tmp/__diff")) in *)
  let _ = Report.display_alarms "Diff" diff in
  let file = Cil.loadBinaryFile "/tmp/__tmp.cil" in
  let skipped = ref 0 in
    BatMap.iter (fun loc (al::_) ->  (* take the first alarm only *)
        prerr_endline "iter";
        inserted := false;
        let _ = visitCilFile (new removeObserveVisitor ()) file in
        let vis = new insertObserveVisitor (al.Report.exp)
        in  visitCilFile vis file;
          if !inserted then (
            Report.display_alarms "Insert airac_observe for the following alarm" (BatMap.add loc [al] BatMap.empty);
            no:=!no+1;
            let out = open_out (!Options.opt_dir ^ "/" ^ string_of_int !no ^ ".c") in 
              print_cil out file; 
              flush out;
              close_out out
              )
          else (
            skipped := !skipped + 1;
            Report.display_alarms "Skip airac_observe for the following alarm" (BatMap.add loc [al] BatMap.empty)
          )
    ) diff;
    prerr_endline ("Inserted " ^ string_of_int !no ^ " files");
    prerr_endline ("Skipped  " ^ string_of_int !skipped ^ " files")
  end

let analysis_and_observe file =  
  if !Options.opt_diff_type = Options.FS then
    let (pre,global) = init_analysis file in
      do_itv_analysis pre global
  else
    let global = StepManager.stepf true "Translation to graphs" Global.init file in
    let (pre, global) = StepManager.stepf true "Pre-analysis" ItvPre.do_preanalysis global in
    let (inputof_CI, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis without Context-Sensitivity" do_sparse_analysis (pre,global) in

    let candidates = list2set (InterCfg.pidsof (Global.get_icfg global)) in
    let to_inline = select_functions_to_inline (pre,global) in
    let b_inlined = inline candidates to_inline file (fun fid -> Global.is_rec global fid) in

    let (pre, global_CS) = 
      if b_inlined then (* something inlined *)
        begin
          (* CFG must be re-computed after inlining *)
          let _ = makeCFGinfo file in
          let global = StepManager.stepf true "Translation to graphs (after inline)" Global.init file in
            StepManager.stepf true "Pre-analysis (after inline)" ItvPre.do_preanalysis global
        end
      else (pre, global) (* nothing inlined *) in

    let (inputof_CS, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis with Context-Sensitivity" do_sparse_analysis (pre,global_CS) in
		(*NOTE: observe 타입에 맞지 않게 사용되어 수정함.*)
    (*let _ = print_endline (observe (global, inputof_CI) (global_CS, inputof_CS)) in *)
		let _ = print_endline (string_of_observe (observe (pre, global, ItvPre.get_mem pre, inputof_CS))) in
     ()
(*---------------------------------------------------------------------*)
(* Generate features from one reduce code. *)
let gen_features_from_one_file = fun file ->
	let one = Frontend.parseOneFile file in
	makeCFGinfo one;
	let _ = makeCFGinfo one in
	let (pre, global) = init_analysis one in
	let cfgs = global.icfg.cfgs in
	let unrolled = BatMap.map (fun cfg -> 
		cfg |> Unroller.unroll_cfg |> Depend.get_dep_graph) cfgs in
	let icfg = {global.icfg with cfgs = unrolled} in
	let global = {global with icfg = icfg} in
	let feature_set = Feature.gen_t1 global in
	feature_set

(* Generate feature list from the given reduced directory. *)
let gen_feature_set : dir -> Flang.t BatSet.t = fun reduced_dir ->
	try
	(*각 reduced code를 읽어와서 extract 해서 나온 feature set들을 모두 union 하면 된다.*)
	let files = Sys.readdir reduced_dir in
	let files = Array.to_list files in 
	let features = List.fold_left (fun accum elem ->
			let full_file_path = "../reduced/" ^ elem in
			let a_feature_set = gen_features_from_one_file full_file_path in
			BatSet.union accum a_feature_set
		) BatSet.empty files in
	features
	with _ -> raise (Failure "gen_feature_set")

(*----------------------------------------------------------------------*)
(* Parse the given single-query source file to CIL. *)
let parse_to_cil : dir -> Cil.file
=fun file -> Frontend.parseOneFile file

let one_tdata_to_str : tdata -> string
= fun (fbvector, answer) ->
	let str_vec = List.fold_left (fun acc column ->
		let mark = if (column = true) then "1 " else "0 " in
		acc ^ mark) "" fbvector in
	let str_ans = if (answer = true) then "1" else "0" in
	str_vec ^ ": " ^ str_ans
	
(* Write all training data to the classifier directory. *)
let write_all_tdata_to_file : tdata list -> dir -> unit
=fun tdata_list outfile ->
	let out = open_out outfile in
	List.iter (fun tdata ->
			(*write_a_tdata_to_file tdata out;*)
			Printf.fprintf out "%s\n" (one_tdata_to_str tdata)
		) tdata_list;
	close_out out

(* Test the performance with classifier along with the written tdata. *)
let test_with_classifier : unit -> unit
=fun () ->
	Sys.command ("python ../classifier/classifier.py test ../classifier/tdata.txt ../classifier/tdata.txt"); ()

let rec tdata_from_allT2_benchmarks : Flang.t BatSet.t -> tdata list
= fun features ->
	let files = Array.to_list (Sys.readdir "../T2") in
	let all_training_data =
		List.fold_left (fun acc file ->
			let tdata_from_one_bench = tdata_from_one_bench ("../T2/" ^ file) features in
			tdata_from_one_bench @ acc
		) [] files in
	all_training_data

and tdata_from_one_bench : dir -> Flang.t BatSet.t -> tdata list
= fun file features ->
	let cilfile = parse_to_cil file in
	let _ = makeCFGinfo cilfile in
	let (pre, global) = init_analysis cilfile in
	let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis (pre, global) in
	let inputof_FI = fill_deadcode_with_premem pre global Table.empty in
	let queries_FS = StepManager.stepf true "Generate report (FS)" Report.generate (global, inputof, Report.BO) in
	let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global, inputof_FI, Report.BO) in
	let queries_FI = List.filter (fun q -> q.status <> Report.BotAlarm) queries_FI in
	let fifsmap = List.fold_left (fun acc fiq ->
		BatMap.add fiq (not (List.exists (fun fsq -> AlarmExp.eq fsq.exp fiq.exp) queries_FS)) acc) BatMap.empty queries_FI in
	let _ = List.iter (fun q ->
		let vis = new Unroller.insertNidVisitor (q) in
		visitCilFile vis cilfile) queries_FI in
	let _ = makeCFGinfo cilfile in
	let (pre, global) = init_analysis cilfile in
	let q2pmap = Training.get_query_to_paths_map global.icfg queries_FI in
	let q2flmap = BatMap.mapi (fun query paths -> Feature.gen_t2 query paths) q2pmap in
	let tdata_list = BatMap.foldi (fun query flset acc ->
		let tdata = tdata_from_one_query fifsmap query flset features in
		tdata::acc) q2flmap [] in
	tdata_list
	
and tdata_from_one_query : fifsmap -> Report.query -> Flang.t BatSet.t -> Flang.t BatSet.t -> tdata
= fun fifsmap query flset features ->
	let fbvector = BatSet.fold (fun feature acc ->
		let column = BatSet.exists (fun fl -> Match.match_fl feature fl) flset in
		column::acc) features [] in
	let answer = BatMap.find query fifsmap in
	(fbvector, answer)

(*----------------------------------------------------*)

let make_a_loc : (string * string) -> Loc.t
=fun (funname, vname) ->
	let var = Var.var_of_lvar (funname, vname) in
	let loc = Loc.loc_of_var var in
	loc

(* Check, given the single-query source file, if the query is proven when only the given selected locations are given precision (flow-sensitivity). *)
let check_answer file selected =
	let global = StepManager.stepf true "Translation to graphs" Global.init file in
	let (pre, global) = StepManager.stepf true "Pre-analysis" ItvPre.do_preanalysis global in
	let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis_autopfs (pre, global, selected) in
	let observation = observe (pre, global, ItvPre.get_mem pre, inputof) in
	let (_, _, _, _, _, _, proved_FI, proved_FS, _, _) = observation in
	proved_FS 

let fbvector_to_str : fbvector -> string
=fun fbvec ->
	let as_string = List.fold_right (fun elm accum ->
			(match elm with
			 | true -> "1 " ^ accum
			 | false -> "0 ")
		) fbvec "" in
	String.trim as_string

let write_fbvector_to_file : fbvector -> dir -> unit
=fun fbvec outfile ->
	let out = open_out outfile in (*open_out : truncate if the file already exists*)
	let fbvector_as_str = fbvector_to_str fbvec in
	Printf.fprintf out "%s" fbvector_as_str

(* Ask classifier if the given new extracted program deserves precision. *)
let candidate_deserve_precision : fbvector -> bool
=fun fbvector ->
	write_fbvector_to_file fbvector "../a_new_fbvector_temp";
	(*exit code 10 : true, 11 : false*)
	let classifier_say_yes = Sys.command ("python ../classifier/classifier.py fbvector_predict LR train-small.txt a_new_fbvector_temp") in
	if classifier_say_yes = 10 then true else false

(*Collect all (funname,vname) set from the given paths.*)
let get_participants : IntraCfg.t BatSet.t -> (string * string) BatSet.t
=fun paths ->
	BatSet.fold (fun path acc -> 
			let funname = path.fd.svar.vname in
			let vnames_from_path = IntraCfg.all_vnames_from_singlepath path IntraCfg.Node.ENTRY SS.empty in
			(*Just transform from SS.t to BatSet.t*)
			let vnames_from_path = SS.fold (fun v acc ->
					BatSet.add v acc
				) vnames_from_path BatSet.empty in
			let funname_vname_set = BatSet.map (fun v -> (funname, v)) vnames_from_path in
			BatSet.union acc funname_vname_set
		) paths BatSet.empty

(*Return fbvector list and participants list (with correct order).*)
let rec fbvector_participants_list_from_newprog : dir -> Flang.t BatSet.t -> (fbvector * (string * string) BatSet.t) list
=fun file features ->
	let cilfile = parse_to_cil file in
	let _ = makeCFGinfo cilfile in
	let (pre, global) = init_analysis cilfile in
	let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis (pre, global) in
	let inputof_FI = fill_deadcode_with_premem pre global Table.empty in
	let queries_FI = StepManager.stepf true "Generate report (FI)" Report.generate (global, inputof_FI, Report.BO) in
	let queries_FI = List.filter (fun q -> q.status <> Report.BotAlarm) queries_FI in
	let _ = List.iter (fun q ->
		let vis = new Unroller.insertNidVisitor (q) in
		visitCilFile vis cilfile) queries_FI in
	let fd = Cil.dummyFunDec in
	let vis = new Unroller.unrollingVisitor (fd, 0) in
	let _ = visitCilFile vis cilfile in
	let _ = makeCFGinfo cilfile in
	let (pre, global) = init_analysis cilfile in
	let q2pmap = Training.get_query_to_paths_map global.icfg queries_FI in
	(*NOTE: Collect participants.*)
	let participants_list = BatMap.foldi (fun query paths acc ->
			let participants = get_participants paths in
			participants::acc
		) q2pmap [] in
	let q2flmap = BatMap.mapi (fun query paths -> Feature.gen_t2 query paths) q2pmap in
	let fbvector_list = BatMap.foldi (fun query flset acc ->
			let fbvector = fbvector_from_query flset features in
			fbvector::acc
		) q2flmap [] in
	let fbvector_participants_list = List.fold_right2 (fun vec participants acc ->
			(vec, participants)::acc
		) fbvector_list participants_list [] in
	fbvector_participants_list
	
and fbvector_from_query : Flang.t BatSet.t -> Flang.t BatSet.t -> fbvector
=fun flset features ->
	let fbvector = BatSet.fold (fun feature acc -> 
			let column = BatSet.exists (fun fl -> Match.match_fl feature fl) flset in
			column::acc
		) features [] in
	fbvector

let collect_promising_participants : (fbvector * (string * string) BatSet.t) list -> locset
=fun vec_parts_list ->
	let participants = List.fold_right (fun (fbvector, participants) acc ->
			if candidate_deserve_precision fbvector then BatSet.union acc participants
			else acc
		) vec_parts_list BatSet.empty in
	let participants = BatSet.map (fun (funname, vname) ->
			make_a_loc (funname, vname)
		) participants in
	participants

let main () =
  let t0 = Sys.time () in
  let _ = Profiler.start_logger () in

  let usageMsg = "Usage: main.native [options] source-files" in
  Printexc.record_backtrace true;

	(* process arguments *)
	Arg.parse Options.opts args usageMsg;
	List.iter (fun f -> prerr_string (f ^ " ")) !files;
	prerr_endline "";

	(* P16 *)
	if !Options.opt_auto_learn then (
		(* 1. Generate features from the reduced. *)
		prerr_endline "STEP1: Generate Features";
		let features = gen_feature_set !Options.opt_reduced in
		
		(* 2. Collect and write tdata to file. *)
		prerr_endline "\nSTEP2: Generate Training Data";
		let all_training_data = tdata_from_allT2_benchmarks features in
		write_all_tdata_to_file all_training_data "../classifier/tdata.txt";
		prerr_endline ">> TRAINING DATA : done --> Test with classifier..";
		test_with_classifier ();
		prerr_endline ">> TEST : done";
		exit 1;
	)		
	else if !Options.opt_auto_apply then (	
		(* 3. Learn a classifier from the tdata and select promising locations. *)
		prerr_endline "\nSTEP3: Select Promising Locations";
		if (List.length !files) <> 1 then raise (Failure "The one and only one file as new program");
		let newprog = List.nth !files 0 in
		Options.opt_pfs := 0;
		
		let features = gen_feature_set !Options.opt_reduced in
		let vec_parts_list = fbvector_participants_list_from_newprog newprog features in
		let promising_participants = collect_promising_participants vec_parts_list in
		
		(* 4. Give full precision to the selected locset. *)
		prerr_endline "\nSTEP4: Apply Precision to the Selected";
		
		Cil.initCIL (); 
		let one = StepManager.stepf true "Parse-and-merge" Frontend.parse_and_merge () in

		makeCFGinfo one;
		let (pre, global) = init_analysis one in
		let pids = InterCfg.pidsof (Global.get_icfg global) in
		let nodes = InterCfg.nodesof (Global.get_icfg global) in
		
		prerr_endline ("#Procs : " ^ string_of_int (List.length pids));
		prerr_endline ("#Nodes : " ^ string_of_int (List.length nodes));

		do_itv_analysis_autopfs pre global promising_participants;
		prerr_endline "Finished properly.";
		Profiler.report stdout;
		prerr_endline (string_of_float (Sys.time () -. t0));
		exit 1
	);

	Cil.initCIL ();
	let one = StepManager.stepf true "Parse-and-merge" Frontend.parse_and_merge () in

	try 
    makeCFGinfo one; (*if !E.hadErrors then E.s (E.error "Cabs2cil had some errors");*)

   
(*    if !Options.opt_dec_prec > 0 then (decrease_precision one; exit 1); *)

		if !Options.opt_insert_observe_imprecise then (
			(match !Options.opt_imprecise_type with
			 | Options.FS -> insert_observe_imprecise_fs one
			 | Options.CS -> raise (Failure "insert observe for all the imprecise: Not Yet Implemented"));
			exit 1
		);

    if !Options.opt_insert_observe then 
      ((match !Options.opt_diff_type with
      | Options.FS -> insert_observe_fs one
      | Options.CS -> insert_observe_cs one
      ); exit 1);

    if !Options.opt_observe then (analysis_and_observe one; exit 1);

    let (pre, global) = init_analysis one in


    let pids = InterCfg.pidsof (Global.get_icfg global) in
    let nodes = InterCfg.nodesof (Global.get_icfg global) in

    prerr_endline ("#Procs : " ^ string_of_int (List.length pids));
    prerr_endline ("#Nodes : " ^ string_of_int (List.length nodes));

	if !Options.opt_cfgs then (
			InterCfg.store_cfgs (!Options.opt_cfgs_dir) (global.icfg));
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
    
    do_itv_analysis pre global;

    prerr_endline "Finished properly.";
    Profiler.report stdout;
    prerr_endline (string_of_float (Sys.time () -. t0));

  with exc ->
    prerr_endline (Printexc.to_string exc);
    prerr_endline (Printexc.get_backtrace())

let _ = main ()
