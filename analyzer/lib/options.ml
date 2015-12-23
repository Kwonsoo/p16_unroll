open Arg

let opt_il = ref false
let opt_dug = ref false
let opt_noalarm = ref false
let opt_inline = ref []
let opt_narrow = ref false
let opt_profile = ref false
let opt_nobar = ref false
let opt_optil = ref false
let opt_cfgs_dir = ref ""
let opt_weights = ref ""
let opt_widen_thresholds = ref ""
let opt_auto_thresholds = ref false
let opt_cfgs = ref false
let opt_pfs = ref 100
let opt_alarm_type = ref "bo"
let opt_deadcode = ref false
let opt_debug = ref false
let opt_pf = ref false
let opts =
  [
  ("-cil", (Arg.Set opt_il), "Show the input program in CIL");
  ("-cfgs", (Arg.String (fun s -> opt_cfgs := true; opt_cfgs_dir := s)), "Store CFGs in dot. Supply a directory to store");
  ("-dug", (Arg.Set opt_dug), "Print Def-Use graph");
  ("-pf", (Arg.Set opt_pf), "Per-file analysis");
  ("-noalarm", (Arg.Set opt_noalarm), "Do not print alarms");
  ("-alarmtype", (Arg.String (fun s -> 
      match s with
      | "bo" -> opt_alarm_type := "bo"
      | "dz" -> opt_alarm_type := "dz"
      | "nd" -> opt_alarm_type := "nd"
      | "ptsto" -> opt_alarm_type := "ptsto"
      | _ -> raise (Failure "Alarm type must be either bo or nz"))), "Alarm type: bo, dz, nd, ptsto");
  ("-inline", (Arg.String (fun s -> opt_inline := s::(!opt_inline))), "Inline *alloc* functions");
  ("-deadcode", (Arg.Set opt_deadcode), "Do not generate alarm in deadcode");
  ("-profile", (Arg.Set opt_profile), "Profiler");
  ("-narrow", (Arg.Set opt_narrow), "Do narrowing");
  ("-pfs", (Arg.Int (fun n -> opt_pfs := n)), "Partial Flow-Sensitivity (0--100)");
  ("-nobar", (Arg.Set opt_nobar), "No progress bar");
  ("-optil", (Arg.Set opt_optil), "Optimize IL");
  ("-debug", (Arg.Set opt_debug), "Debug mode");
  ("-wv", (Arg.String (fun s -> opt_weights := s)), "Weight vector for flow-sensitivity (e.g., \"0 1 -1 ... \"). Unspecified weights are zeros.");
  ("-thresholds", (Arg.String (fun s -> opt_widen_thresholds := s)), "Widening with threshold (e.g., \"0 1 2 3 ...\")");
  ("-auto_thresholds", (Arg.Set opt_auto_thresholds), "Choose thresholds automatically");
  ]
