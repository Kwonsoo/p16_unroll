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

open Reduce
open Classify
open Training
open Apply
open Feature
open Types

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

(* Generate features from one reduce code. *)
let gen_features_from_one_file = fun file ->
	(* 소스파일 path로부터 global 데이터를 만들어낸 다음에 gen을 사용해서 모든 feature를 extract 해내면 된다.*)
	let one = Frontend.parseOneFile file in
	makeCFGinfo one;
	let (pre, global) = init_analysis one in
	let feature_set = Feature.gen global in
	feature_set

(* Generate feature list from the reduced given directory. *)
let gen_feature_list : dir -> Flang.t list = fun reduced_dir ->
	(*각 reduced code를 읽어와서 extract 해서 나온 feature set들을 모두 union 하면 된다.*)
	let files = Sys.readdir reduced_dir in
	let files = Array.to_list files in 
	let features = List.fold_left (fun accum elem ->
			let full_file_path = "../reduced/" ^ elem in
			let a_feature_set = gen_features_from_one_file full_file_path in
			BatSet.union accum a_feature_set
		) BatSet.empty files in
	BatSet.to_list features

(* Return local variable names from the given function.
	 NOTE: Formal Parameter들은 여기에 포함 안 되어있다. *)
let extract_locl_vnames : Cil.fundec -> (string * string) BatSet.t
=fun fd ->
	let funname = fd.svar.vname in
	let vinfos = fd.slocals in
	let funname_vname_tuples = List.map (fun vinfo -> (funname, vinfo.vname)) vinfos in
	BatSet.of_list funname_vname_tuples

let make_a_loc : (string * string) -> Loc.t
=fun (funname, vname) ->
	let var = Var.var_of_lvar (funname, vname) in
	let loc = Loc.loc_of_var var in
	loc


(* Produce single-query programs into the given directory, from the given source file. *)
let imprecise_singleq_progs : dir -> dir -> unit
=fun file outdir ->
	Sys.command ("mkdir " ^ outdir);
	Sys.command ("./main.native " ^ file ^ " -insert_observe_imprecise -imprecise_type fs -dir " ^ outdir);
	()

let clear_singleq_dir : dir -> unit
=fun singleq_dir ->
	Sys.command ("rm -r " ^ singleq_dir); ()

let parse_to_cil : dir -> Cil.file
=fun file -> Frontend.parseOneFile file

let check_answer file selected =
	let global = StepManager.stepf true "Translation to graphs" Global.init file in
	let (pre, global) = StepManager.stepf true "Pre-analysis" ItvPre.do_preanalysis global in
	let (inputof, _, _, _, _, _) = StepManager.stepf true "Main Sparse Analysis" do_sparse_analysis_autopfs (pre, global, selected) in
	let observation = observe (pre, global, ItvPre.get_mem pre, inputof) in
	let (_, _, _, _, _, _, proved_FI, proved_FS, _, _) = observation in
	proved_FS 

(* Return participants from the given single-query program. *)
let get_participants : Cil.file -> (string * string) BatSet.t
=fun cil_file ->
	let observe_fd = Slicer.find_observe_fundec cil_file in
	let participants = extract_locl_vnames observe_fd in
	participants

let one_tdata_from_sqprog : dir -> Flang.t list -> tdata
=fun sqprog_path features ->
	let cil_file = parse_to_cil sqprog_path in
	let (pre, global) = init_analysis cil_file in
	let extracted_prog = Feature.gen global in
	let fbvector = Feature.fbvectorize extracted_prog features in
	let participants = get_participants cil_file in	(*participated (f,v) tuples*)
	let participants = BatSet.map (make_a_loc) participants in
	let answer = check_answer cil_file participants in
	(fbvector, answer)

let tdata_from_one_benchmark : dir -> Flang.t list -> tdata list
=fun benchmark_path features ->
	imprecise_singleq_progs benchmark_path "../T2_singleq_temp";
	(* Do the following for all single-query programs. *)
	let fname_list = Array.to_list (Sys.readdir "../T2_singleq_temp") in
	let tdata_from_a_bench = List.fold_left (fun accum fname ->
			(one_tdata_from_sqprog ("../T2_singleq_temp/" ^ fname) features) :: accum
		) [] fname_list in
	clear_singleq_dir "../T2_singleq_temp";
	tdata_from_a_bench

let main () =
  let t0 = Sys.time () in
  let _ = Profiler.start_logger () in

  let usageMsg = "Usage: main.native [options] source-files" in
  Printexc.record_backtrace true;

	(* process arguments *)
	Arg.parse Options.opts args usageMsg;
	List.iter (fun f -> prerr_string (f ^ " ")) !files;
	prerr_endline "";

	(* auto-feature research *)
	if !Options.opt_auto_learn then (
		(* 1. Generate features from the reduced. *)
		prerr_endline "STEP1: Generate Features";
		let features = gen_feature_list !Options.opt_reduced in
		
		(* 2. Learn classifier. *)
		prerr_endline "\nSTEP2: Learn the Classifier";
		let fname_list = Array.to_list (Sys.readdir "../T2") in
		let all_training_data =
			List.fold_left (fun accum fname -> 
					let tdata_from_a_benchmark = tdata_from_one_benchmark ("../T2/" ^ fname) features in
					tdata_from_a_benchmark @ accum
				) [] fname_list in
		(*TODO: Give the training data to the classifier.*)

		exit 1
	)
	else if !Options.opt_auto_apply then (	
		(* 3. Select New Locations. *)
		prerr_endline "\nSTEP3: Select New Locations";
		(*TODO*)
		(*Predictor.copy_pgms "../benchmarks/bc-1.06.c" "../N_singleq";*)
		(*let candidates = Predictor.build_candidates "../N_singleq" in*)
		(*let locset = Apply.select "research/learning/classifier.sh" candidates in*)
		let locset = BatSet.empty in

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

		do_itv_analysis_autopfs pre global locset;
		prerr_endline "Finished properly.";
		Profiler.report stdout;
		prerr_endline (string_of_float (Sys.time () -. t0));
		exit 1
	)

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
    
    do_itv_analysis pre global;

    prerr_endline "Finished properly.";
    Profiler.report stdout;
    prerr_endline (string_of_float (Sys.time () -. t0));

  with exc ->
    prerr_endline (Printexc.to_string exc);
    prerr_endline (Printexc.get_backtrace())

let _ = main ()
