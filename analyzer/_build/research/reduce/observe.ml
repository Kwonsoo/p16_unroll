open Graph
open Cil
open Global
open AbsDom
open Vocab
open Frontend
open ItvDom
open ItvAnalysis
open Report

type observation = 
    | OBS_BO of Itv.t * Itv.t * Itv.t * bool (* size, offset, index, proved*)
    | OBS_PTSTO of (Loc.t BatSet.t) (* points-to set *)
    | OBS_OTHERS

let is_proved (size,offset,index) = 
  let idx = Itv.plus offset index in
    match idx, size with
    | Itv.V (Itv.Int ol, Itv.Int oh), Itv.V (Itv.Int sl, _) ->
      if oh >= sl || ol < 0 then false else true
    | _ -> false

(* get the value of "airac_observe" for the given memory *)
let get_observe_info exps mem pid = 
  match get_alarm_type () with
  | Report.BO ->
    begin
      match exps with 
      | buf::idx::_ ->
        let size = ArrayBlk.sizeof (ItvDom.array_of_val (EvalOp.eval pid buf mem)) in
        let offset = ArrayBlk.offsetof (ItvDom.array_of_val (EvalOp.eval pid buf mem)) in
        let index = itv_of_val (EvalOp.eval pid idx mem) in
        let proved = is_proved (size, offset, index) in
        let buf_name = Cil2str.s_exp buf in
        let idx_name = Cil2str.s_exp idx in
          OBS_BO (size, offset, index, proved)
      | _ -> raise (Failure "airac_observe for BO requires two arguments (buffer and index expressions)")
    end
  | Report.PTSTO ->
    begin
      match exps with
      | buf::_ -> OBS_PTSTO (ItvDom.pow_loc_of_val (EvalOp.eval pid buf mem))
      | _ -> raise (Failure "airac_observe for PTSTO requires at least one argument")
    end
  | _ -> OBS_OTHERS

let get_initial_observe () =
  match get_alarm_type () with
  | Report.BO -> OBS_BO (Itv.bot, Itv.bot, Itv.bot, false)
  | Report.PTSTO -> OBS_PTSTO BatSet.empty
  | _ -> OBS_OTHERS

let join_observation : observation -> observation -> observation
=fun obs1 obs2->
  match obs1, obs2 with
  | OBS_BO (size1,offset1,index1,proved1), OBS_BO (size2,offset2,index2,proved2) -> 
    OBS_BO (Itv.join size1 size2, Itv.join offset1 offset2, Itv.join index1 index2, proved1 && proved2)
  | OBS_PTSTO s1, OBS_PTSTO s2 -> OBS_PTSTO (BatSet.union s1 s2)
  | _ -> raise (Failure "join_observation: Not implemented")

let get_observation : InterCfg.t -> Table.t -> observation
=fun g t ->
  let nodes = InterCfg.nodesof g in
    list_fold (fun n a -> 
      let mem = Table.find n t in
      let pid = InterCfg.Node.get_pid n in
      match InterCfg.cmdof g n with 
      | IntraCfg.Cmd.Ccall (None, Cil.Lval (Cil.Var f, Cil.NoOffset), exps, _) when f.vname = "airac_observe" ->
        join_observation (get_observe_info exps mem pid) a
      | _ -> a
    ) nodes (get_initial_observe ())

let get_names_of_obs_args : InterCfg.t -> Table.t -> string * string
=fun g t ->
  let nodes = InterCfg.nodesof g in
    list_fold (fun n a -> 
      let mem = Table.find n t in
      let pid = InterCfg.Node.get_pid n in
      match InterCfg.cmdof g n with 
      | IntraCfg.Cmd.Ccall (None, Cil.Lval (Cil.Var f, Cil.NoOffset), exps, _) when f.vname = "airac_observe" ->
        begin
          match exps with
          | buf::idx::_ -> Cil2str.s_exp buf, Cil2str.s_exp idx
          | _ -> raise (Failure "get_names_of_obs_args")
        end
      | _ -> a
    ) nodes ("XXX","YYY")

let observe (global1, inputof1)  (global2, inputof2) : string = 
  let g1 = Global.get_icfg global1 in (* e.g., global for FI/CI *)
  let g2 = Global.get_icfg global2 in (* e.g., global for FS/CS *)
  let obs1 = get_observation g1 inputof1 in
  let obs2 = get_observation g2 inputof2 in
    match obs1, obs2 with
    | OBS_BO (size1, offset1, index1, proved1), OBS_BO (size2, offset2, index2, proved2) ->
      let bufname,idxname = get_names_of_obs_args g1 inputof1 in
      "(AIRAC_OBSERVE)" ^
      "FI/CI (" ^ string_of_bool proved1 ^")" ^
                " Size: " ^ Itv.to_string size1 ^ 
                " Offset: " ^ Itv.to_string offset1 ^ 
                " Index : " ^ Itv.to_string index1 ^
      " FS/CS (" ^ string_of_bool proved2 ^ ")" ^
                " Size: " ^ Itv.to_string size2 ^ 
                " Offset: " ^ Itv.to_string offset2 ^ 
                " Index : " ^ Itv.to_string index2 ^
      " " ^ bufname ^ " " ^ idxname ^ "\n"
    | OBS_PTSTO locs1, OBS_PTSTO locs2 -> 
      "(AIRAC_OBSERVE) " ^ " FI/CI:" ^ string_of_int (BatSet.cardinal locs1) ^ 
                           " FS/CS:" ^ string_of_int (BatSet.cardinal locs2)
    | _ -> "(AIRAC_OBSERVE) undefined kinds "
