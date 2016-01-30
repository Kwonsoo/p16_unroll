open Vocab
open AbsDom
open Lat
open Pre
open Dug
open DMap
open AbsSem

let total_iterations = ref 0
let g_clock = ref 0.0

module Make(Pre:PRE)
           (MainSem:AbsSem with type Dom.A.t = Pre.Access.Loc.t)
           (Table:DMAP with type A.t = Node.t and type B.t = MainSem.Dom.t)
           (DUGraph:DUG with type vertex = Table.A.t and type loc = Pre.Access.Loc.t)
=
struct

  module Mem = MainSem.Dom
  module Worklist = Worklist.Make(Node)(DUGraph)
  type locset = Pre.Access.Loc.t BatSet.t

	let refined_vals : (Node.t, (Mem.A.t * Mem.B.t) BatSet.t) BatMap.t ref = ref BatMap.empty
	
  module Workorder = Worklist.Workorder
  let needwidening : DUGraph.vertex -> Workorder.t -> bool
  =fun idx order -> Workorder.is_loopheader idx order

  let def_locs_cache = Hashtbl.create 251
  let get_def_locs idx dug =
    try Hashtbl.find def_locs_cache idx with Not_found ->
    let def_locs =
      let union_locs succ = BatSet.union (DUGraph.get_abslocs idx succ dug) in
      list_fold union_locs (DUGraph.succ idx dug) BatSet.empty in
    Hashtbl.add def_locs_cache idx def_locs; def_locs

  let prdbg_endline global node input old_output new_output = 
    prerr_endline (Node.to_string node);
    prerr_endline (IntraCfg.Cmd.to_string 
        (InterCfg.cmdof global.Global.icfg node));
    prerr_endline "input";
    prerr_endline (Mem.to_string input);
    prerr_endline "old_output";
    prerr_endline (Mem.to_string old_output);
    prerr_endline "new_output";
    prerr_endline (Mem.to_string new_output)

  (* fixpoint iterator specialized to the widening phase *)
  let analyze_node ?locset silent : DUGraph.vertex
    -> (Worklist.t * Global.t * DUGraph.t * Pre.t * Table.t * Table.t
        * Workorder.t)
    -> (Worklist.t * Global.t * DUGraph.t * Pre.t * Table.t * Table.t
        * Workorder.t)
  = fun idx
    (works, global, dug, pre, inputof, outputof, order) ->

    (
      total_iterations := !total_iterations + 1;
      if !total_iterations = 1 then g_clock := Sys.time()
      else if !total_iterations mod 10000 = 0
      then 
          begin
            my_prerr_endline silent ("#iters: "^(string_of_int !total_iterations)
                          ^" took "^string_of_float (Sys.time()-.(!g_clock)));
            my_prerr_endline silent ""; flush stderr;
          g_clock := Sys.time ();
          end
    );
    let input = Table.find idx inputof in
    let old_output = Table.find idx outputof in

    let _ = Profiler.start_event "SparseAnalysis.run" in
    let (new_output, global) =
      MainSem.run ~mode:Strong ?locset ~premem:(Pre.get_mem pre)
        ~silent:silent idx (input, global) in
    let _ = Profiler.finish_event "SparseAnalysis.run" in
    
    let _ = Profiler.start_event "SparseAnalysis.widening" in

    let (new_output, b_stable, unstables) =
      let def_locs = get_def_locs idx dug in
      let is_unstb v1 v2 = not (Mem.B.le v2 v1) in
      let u = Mem.unstables old_output new_output is_unstb def_locs in
      let op = if needwidening idx order then Mem.B.widen else Mem.B.join in
      let u = List.map (fun (k, v1, v2) -> (k, op v1 v2)) u in
      let new_output = list_fold (fun (k, v) -> Mem.add k v) u old_output in
      (* update unstable locations's values by widened values *)
      let u = List.map (fun (k, _) -> (k, Mem.find k new_output)) u in
      (new_output, u = [], u) in
    let _ = Profiler.finish_event "SparseAnalysis.widening" in

    if b_stable then
      (works, global, dug, pre, inputof, outputof, order)
    else
      let _ = Profiler.start_event "SparseAnalysis.propagating" in
      let id1 = BatMap.find idx order.Workorder.order in
      let (works, inputof) =
        let update_succ succ (works, inputof) =
          let old_input = Table.find succ inputof in
          let locs_on_edge = DUGraph.get_duset idx succ dug in
          let is_on_edge (x, _) = DUGraph.mem_duset x locs_on_edge in
          let to_join = List.filter is_on_edge unstables in
          if to_join = [] then (works, inputof) else
            let new_input = Mem.join_pairs to_join old_input in
            let id2 = BatMap.find succ order.Workorder.order in
            let bInnerLoop = Worklist.compare_order (id2, succ) (id1, idx) in
            (Worklist.queue bInnerLoop order.Workorder.headorder succ id2 works,
             Table.add succ new_input inputof) in
        let succs = DUGraph.succ idx dug in
        list_fold update_succ succs (works, inputof) in
        let _ = Profiler.finish_event "SparseAnalysis.propagating" in
      (works, global, dug, pre, inputof, Table.add idx new_output outputof,
       order)

  (* fixpoint iterator that can be used in both widening and narrowing phases *)
	(* result : lfp \x. ub \meet f(x) (in widening) or   gfp \x. ub \meet f(x)  (in narrowing) *)
  let analyze_node_with_otable ?(ub=Table.empty) ?locset (widen,join,le) silent :
    DUGraph.vertex
    -> (Worklist.t * Global.t * DUGraph.t * Pre.t * Table.t * Table.t
        * Workorder.t)
    -> (Worklist.t * Global.t * DUGraph.t * Pre.t * Table.t * Table.t
        * Workorder.t)
  =fun idx
    (works, global, dug, pre, inputof, outputof, order) ->

    (
      total_iterations := !total_iterations + 1;
      if !total_iterations = 1 then g_clock := Sys.time()
      else if !total_iterations mod 10000 = 0
      then 
          begin
          prerr_endline ("#iters: "^(string_of_int !total_iterations)
                        ^" took "^string_of_float (Sys.time()-.(!g_clock)));
          prerr_endline ""; flush stderr;
          g_clock := Sys.time ();
          end
    );
    let pred = DUGraph.pred idx dug in
    let input = List.fold_left (fun m p ->
          let pmem = Table.find p outputof in
          let locs_on_edge = DUGraph.get_abslocs p idx dug in 
          BatSet.fold (fun l m -> 
              let v1 = Mem.find l pmem in
              let v2 = Mem.find l m in
              Mem.add l (Mem.B.join v1 v2) m) locs_on_edge m
          ) Mem.bot pred in
    let old_output = Table.find idx outputof in
    let (new_output, global) =
      MainSem.run ~mode:Strong ?locset ~premem:(Pre.get_mem pre) 
        ~silent:silent idx (input, global) in
		
		(* prevent from being greater than ub *)
		let new_output =
			let ubmem = Table.find idx ub in
			Mem.foldi (fun loc v m ->
				let ub_v = Mem.find loc ubmem in
				let new_v = Mem.B.meet v ub_v in
				if (Mem.B.eq new_v Mem.B.bot) then m
				else Mem.add loc new_v m
			) new_output new_output in
			
		(* prdbg_endline global idx input old_output new_output; *)
		
		let (new_output, b_stable) =
      let def_locs = get_def_locs idx dug in
      let is_unstb v1 v2 = not (le v2 v1) in
			let u = Mem.unstables old_output new_output is_unstb def_locs in
      let op = widen in 
      let u = List.map (fun (k, v1, v2) -> (k, op v1 v2)) u in
      let new_output = list_fold (fun (k, v) -> Mem.add k v) u old_output in
      (* update unstable locations's values by widened values *)
      let u = List.map (fun (k, _) -> (k, Mem.find k new_output)) u in
      (new_output, u = []) in
		(* let new_output =                                               *)
    (*   let def_locs = get_def_locs idx dug in                       *)
		(* 	Mem.filter (fun loc _ -> BatSet.mem loc def_locs) new_output *)
    (* in                                                             *)
		(* prdbg_endline global idx input old_output new_output; *)
		(* let new_output = widen old_output new_output in *)
		(* let b_stable = le new_output old_output in *)

    let inputof = Table.add idx input inputof in
    if b_stable then
      (works, global, dug, pre, inputof, outputof, order)
    else
      let id1 = BatMap.find idx order.Workorder.order in
      let works = 
        List.fold_left (fun works succ ->
              let id2 = BatMap.find succ order.Workorder.order in
              let bInnerLoop = Worklist.compare_order (id2, succ) (id1, idx) in
                Worklist.queue bInnerLoop order.Workorder.headorder succ id2 works
        ) works (DUGraph.succ idx dug) in
      (works, global, dug, pre, inputof,
       Table.add idx new_output outputof, order)


  let rec iterate ?(ub=Table.empty) ?(otable=false) ?locset (widen,join,le) silent : 
      (Worklist.t * Global.t * DUGraph.t * Pre.t * Table.t * Table.t
       * Workorder.t)
   -> (Worklist.t * Global.t * DUGraph.t * Pre.t * Table.t * Table.t
       * Workorder.t)
  =fun (works, global, dug, pre, inputof, outputof, order) ->

    match Worklist.pick works with
    | None -> (works, global, dug, pre, inputof, outputof, order)
    | Some (idx, rest) ->
      let x = 
        if otable then 
          analyze_node_with_otable ~ub:ub ?locset (widen, join, le) silent idx
            (rest, global, dug, pre, inputof, outputof, order)
        else 
          analyze_node ?locset silent idx
            (rest, global, dug, pre, inputof, outputof, order)
      in
      iterate ~ub:ub ~otable:otable ?locset (widen,join,le) silent x

  let widening ?otable ?locset silent :
      Global.t * DUGraph.t * Pre.t * Table.t * Table.t * Workorder.t
      -> Global.t * DUGraph.t * Pre.t * Table.t * Table.t * Workorder.t

  =fun (global, dug, pre, inputof, outputof, order) ->
    total_iterations := 0;
    let init_worklist = Worklist.init order.Workorder.order dug in
    let (_, global, dug, pre, inputof, outputof, order) =
      iterate ?otable ?locset (Mem.B.widen, Mem.B.join, Mem.B.le) silent
        (init_worklist, global, dug, pre, inputof, outputof, order) in
      my_prerr_endline silent
        ("#iteration in widening : " ^ string_of_int !total_iterations);
    (global, dug, pre, inputof, outputof, order)
 
(* computing the gfp upper bounded by ub *)
  let narrowing ?(initnodes=BatSet.empty) ?(ub=Table.empty) ?otable ?locset silent :
      Global.t * DUGraph.t * Pre.t * Table.t * Table.t * Workorder.t
      -> Global.t * DUGraph.t * Pre.t * Table.t * Table.t * Workorder.t

  =fun (global, dug, pre, inputof, outputof, order) ->
    total_iterations := 0;
		let initnodes = if (BatSet.is_empty initnodes) then DUGraph.nodesof dug else initnodes in  
    let init_worklist = Worklist.init_nodes order.Workorder.order initnodes (*dug*) in
    let (_, global, dug, pre, inputof, outputof, order) =
      iterate ~ub:ub ~otable:true ?locset
        (Mem.B.narrow, Mem.B.join, fun x y -> Mem.B.le y x) silent
        (init_worklist, global, dug, pre, inputof, outputof, order) in
    my_prerr_endline silent
      ("#iteration in narrowing : " ^ string_of_int !total_iterations);
    (global, dug, pre, inputof, outputof, order)

		
  let onestep_transfer : bool -> Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
  =fun silent nodes (mem,global) ->
     list_fold (fun node (mem,global) ->
       MainSem.run ~silent:silent node (mem,global)
     ) nodes (mem,global) 

  let rec fixptFI: bool -> Node.t list -> Mem.t * Global.t -> Mem.t * Global.t
  =fun silent nodes (mem,global) ->
    let (mem',global') = onestep_transfer silent nodes (mem,global) in
    let mem' = Mem.widen mem mem' in
      if Mem.le mem' mem && Dump.le global'.Global.dump global.Global.dump
      then (mem',global')
      else fixptFI silent nodes (mem',global')

  let runFI (silent,nodes,mem,global) = fixptFI silent nodes (mem,global)

  let bind_undefined_locs (inputof,memFI,global,dug,preinfo,locset) =
    DUGraph.fold_node (fun n t ->
  Profiler.start_event "Sparse.useof";
      let used = Pre.Access.useof (Pre.get_access preinfo n) in
  Profiler.finish_event "Sparse.useof";
  Profiler.start_event "Sparse.pred";
      let pred = DUGraph.pred n dug in
  Profiler.finish_event "Sparse.pred";
  Profiler.start_event "Sparse.list_on_edge";
      let locs_on_edge = list_fold (fun p -> BatSet.union (DUGraph.get_abslocs p n dug)) pred BatSet.empty in
  Profiler.finish_event "Sparse.list_on_edge";
  Profiler.start_event "Sparse.diff";
      let locs_not_on_edge = BatSet.diff used locs_on_edge in
  Profiler.finish_event "Sparse.diff";
  Profiler.start_event "Sparse.not_on_edge";
      let mem_with_locs_not_on_edge =
        BatSet.fold (fun loc mem ->
          Mem.add loc (Mem.find loc memFI)  mem
        ) locs_not_on_edge Mem.bot in
  Profiler.finish_event "Sparse.not_on_edge";
      Table.add n mem_with_locs_not_on_edge t
    ) dug inputof
  
  (* add pre-analysis memory to unanalyzed nodes *)
  let bind_unanalyzed_node (memFI,inputof,global,preinfo,dug) = 
    let nodes = InterCfg.nodesof (Global.get_icfg global) in
    let nodes_in_dug = DUGraph.nodesof dug in
      list_fold (fun node t -> 
        if BatSet.mem node nodes_in_dug then t
        else 
          let mem_with_access = 
            BatSet.fold (fun loc -> 
              Mem.add loc (Mem.find loc memFI) 
            ) (Pre.Access.useof (Pre.get_access preinfo node)) Mem.bot in
            Table.add node mem_with_access t 
      ) nodes inputof

  let perform ?(otable=false) ?(silent=false) :
      Global.t * DUGraph.t * Pre.t * Pre.Access.Loc.t BatSet.t * Table.t * Workorder.t
      -> Table.t * Table.t * Mem.t
  =fun (global, dug, preinfo, locset, inputof, order) -> 

    let icfg = Global.get_icfg global in

(*    (* worklist order *)
    let _ = Profiler.start_event "SparseAnalysis.workorder" in
    let callnodes = list2set (InterCfg.callnodesof icfg) in
    let order = StepManager.stepf_s silent false "Workorder computation"
      Workorder.perform (dug,callnodes) in
    let _ = Profiler.finish_event "SparseAnalysis.workorder" in
*)
    (* flow-insensitive analysis *)
    let (memFI,_) = StepManager.stepf_s silent false "Flow-insensitive analysis"
      runFI (true, InterCfg.nodesof icfg, Mem.bot, global) in

    (* widening *)
    let _ = Profiler.start_event "SparseAnalysis.bind_undef" in
    let inputof = bind_undefined_locs (inputof,memFI,global,dug,preinfo,locset) in
    let _ = Profiler.finish_event "SparseAnalysis.bind_undef" in

    let (global, dug, preinfo, inputof, outputof, order) =
      StepManager.stepf_s silent false "Fixpoint iteration with widening"
        (widening ~otable:otable ~locset:locset silent)
        (global, dug, preinfo, inputof, Table.empty, order) in

    let _ = Profiler.start_event "SparseAnalysis.bind_unanalyzed" in
    let inputof = bind_unanalyzed_node (memFI,inputof,global,preinfo,dug) in
    let _ = Profiler.finish_event "SparseAnalysis.bind_unanalyzed" in
    
    prerr_memory_usage ~silent:silent ();

    (* meet with memFI *)
    let _ = Profiler.start_event "SparseAnalysis.meet_FI" in
    let (inputof, outputof) =
      StepManager.stepf_opt_s silent true false
        "Meet with flow insensitive analysis memory"
        (fun (inputof, outputof) ->
          let inputof = Table.map (Mem.meet_big_small memFI) inputof in
          let outputof = Table.map (Mem.meet_big_small memFI) outputof in
          (inputof, outputof)) (inputof, outputof) in
    let _ = Profiler.finish_event "SparseAnalysis.meet_FI" in

    (* narrowing *)
    let _ = Profiler.start_event "SparseAnalysis.narrow" in
    let (global, dug, preinfo, inputof, outputof, order) =
        StepManager.stepf_opt_s silent !Options.opt_narrow false
          "Fixpoint iteration with narrowing"
          (narrowing silent)
          (global, dug, preinfo, inputof, outputof, order) in
    let _ = Profiler.finish_event "SparseAnalysis.narrow" in
    (inputof, outputof, memFI)


(* Merge m1 and m2 while taking m2(x) if x is in locs *)
let merge_over : Mem.A.t BatSet.t -> Mem.t -> Mem.t -> Mem.t
= fun locs m1 m2 ->
  let add_when_in k m1 = Mem.add k (Mem.find k m2) m1 in
  BatSet.fold add_when_in locs m1

(* Merge m1 and m2 while taking m2(x) if x is not in locs, but in dom(m2) *)
let merge_over' : Mem.A.t BatSet.t -> Mem.t -> Mem.t -> Mem.t
= fun locs m1 m2 ->
  let add_when_in k v m1 =
    if Mem.B.eq v Mem.B.bot then m1 else
    if BatSet.mem k locs then m1 else
      Mem.add k v m1 in
  Mem.foldi add_when_in m2 m1

let filter_locs : Mem.A.t BatSet.t -> Mem.t -> Mem.t
= fun locs m ->
  Mem.filter (fun l _ -> BatSet.mem l locs) m

let get_use_locs_by_du idx_here dug =
  let add_locs pred = BatSet.union (DUGraph.get_abslocs pred idx_here dug) in
  list_fold add_locs (DUGraph.pred idx_here dug) BatSet.empty

let get_def_locs_by_du idx_here dug =
  let add_locs succ = BatSet.union (DUGraph.get_abslocs idx_here succ dug) in
  list_fold add_locs (DUGraph.succ idx_here dug) BatSet.empty

(* Generate the full input table for a given procedure 
 * : exploits the SSA property that the value of a location not used at a node 
 *   is identical to the value at the immediate dominator of the node *)
let densify_cfg : Global.t * Pre.t * DUGraph.t * locset
  -> Table.t -> Table.t -> IntraCfg.t -> Table.t * Table.t
= fun (global, pre, dug, locset) inputof outputof cfg ->
  let pid = IntraCfg.get_pid cfg in
  let dom_tree = IntraCfg.get_dom_tree cfg in

  let rec propagate : IntraCfg.Node.t -> Table.t * Table.t -> Table.t * Table.t
  =fun here (inputof, outputof) ->
    let idx_here = Index.make pid here in
    let use_locs = get_use_locs_by_du idx_here dug in
    let def_locs = get_def_locs_by_du idx_here dug in
    let input_here = Table.find idx_here inputof in
    let output_here = Table.find idx_here outputof in
    let d_input_here =
      let basic_mem =
        match IntraCfg.parent_of_dom_tree here dom_tree with
        | None -> Mem.bot
        | Some idom -> Table.find (Index.make pid idom) outputof in
      let input_here' = merge_over use_locs basic_mem input_here in
      merge_over' locset input_here' input_here in
    let d_output_here =
      let output_here' =
        fst (MainSem.run ~mode:Strong ~locset ~premem:(Pre.get_mem pre)
               idx_here (d_input_here, global)) in
      merge_over def_locs output_here' output_here in
    let d_input_here = filter_locs locset d_input_here in
    let d_output_here = filter_locs locset d_output_here in
    let d_inputof = Table.add idx_here d_input_here inputof in
    let d_outputof = Table.add idx_here d_output_here outputof in
    let nodes_dom_ordered = IntraCfg.children_of_dom_tree here dom_tree in
    BatSet.fold propagate nodes_dom_ordered (d_inputof, d_outputof) in

  propagate IntraCfg.Node.ENTRY (inputof, outputof)

let densify : Global.t * Pre.t * Table.t * Table.t * DUGraph.t * locset
  -> Table.t * Table.t
=fun (global, pre, inputof, outputof, dug, locset) ->
  let icfg = Global.get_icfg global in
  let pids = InterCfg.pidsof icfg in
  let todo = List.length pids in
  let (inputof, outputof) =
    snd (
    list_fold (fun pid (k, (inputof, outputof)) ->
      prerr_progressbar false k todo;
      (k + 1,
       densify_cfg (global, pre, dug, locset) inputof outputof
         (InterCfg.cfgof icfg pid))
    ) pids (1, (inputof, outputof))) in
  (inputof, outputof)

end
