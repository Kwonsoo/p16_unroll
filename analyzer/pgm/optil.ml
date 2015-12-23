open Vocab
open Cil
open IntraCfg
open IntraCfg.Cmd

(*
  1. collect all strings in salloc(_,s) of program: S
  2. build a map from s \in S to lv: M
  3. replace each salloc(lv,s) by set(lv,M(s))
  4. insert salloc(lv,s) in _G_ for each (s,lv) \in M
*)

let collect_strs : InterCfg.t -> string BatSet.t 
=fun icfg ->
  list_fold (fun n -> 
    match InterCfg.cmdof icfg n with
    | Csalloc (_,s,_) -> BatSet.add s
    | _ -> id
  ) (InterCfg.nodesof icfg) BatSet.empty

let str_count = ref 0
let get_cstr_name () =
  str_count := !str_count + 1;
  "_zoo_cstr_" ^ i2s !str_count

let build_strmap : string BatSet.t -> (string, lval) BatMap.t
=fun strs ->
  BatSet.fold (fun str map ->
    let name = get_cstr_name () in
    let lv = (Var (Cil.makeGlobalVar name Cil.charPtrType), NoOffset) in
      BatMap.add str lv map
  ) strs BatMap.empty

let replace_salloc : InterCfg.t -> (string, lval) BatMap.t -> InterCfg.t
=fun icfg strmap ->
  let nodes = InterCfg.nodesof icfg in
    list_fold (fun n g -> 
      match InterCfg.cmdof g n with
      | Csalloc (lhs,str,location) -> 
        let rhs = Lval (BatMap.find str strmap) in
        let cmd = Cset (lhs,rhs,location) in
          InterCfg.add_cmd g n cmd
      | _ -> g
    ) nodes icfg

let dummy_location = { line = 0; file = ""; byte = 0 }

let insert_salloc : InterCfg.t -> (string, lval) BatMap.t -> InterCfg.t 
=fun icfg strmap ->
  let _G_ = InterCfg.cfgof icfg InterCfg.global_proc in
 let _G_with_sallocs = 
    BatMap.foldi (fun str lhs g ->
      let entry = IntraCfg.entryof g in
      let _ = assert (List.length (IntraCfg.succ entry g) = 1) in
      let next = List.nth (IntraCfg.succ entry g) 0 in
      let cmd = Csalloc (lhs,str,dummy_location) in
        IntraCfg.add_new_node entry cmd next g
    ) strmap _G_ in
  { icfg with InterCfg.cfgs = BatMap.add InterCfg.global_proc _G_with_sallocs icfg.InterCfg.cfgs}

let opt_salloc : InterCfg.t -> InterCfg.t
=fun icfg -> 
  let strs = collect_strs icfg in
  let strmap = build_strmap strs in
  let icfg = replace_salloc icfg strmap in
  let icfg = insert_salloc icfg strmap in
    icfg

let perform : InterCfg.t -> InterCfg.t
=fun icfg ->
try
  icfg 
  |> (if !Options.opt_optil then opt_salloc else id)
  |> id
with _ -> prerr_endline "optil.perform"; raise (Failure "Optil")
