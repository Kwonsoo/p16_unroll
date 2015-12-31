open Graph
open Vocab
open Cil
open Global
open Frontend
open ItvDom
open ItvAnalysis

let found (exp,loc,alarm_exp) = List.exists (AlarmExp.eq alarm_exp) (AlarmExp.c_exp exp loc)

let found_s (exps,loc,alarm_exp) = List.exists (fun e -> found (e, loc, alarm_exp)) exps 

(* for poinsto queries *)
let found_ptsto (exp,loc,str) =
  BatSet.mem str (list2set (List.map (fun alexp -> 
      Cil2str.s_location loc ^ " " ^ AlarmExp.to_string alexp
      ) (AlarmExp.c_exp exp loc)))

let found_s_ptsto (exps,loc,str) = List.exists (fun e -> found_ptsto (e,loc,str)) exps

let alarmExp2args alarm_exp =
  match alarm_exp with
  | IntraCfg.Cmd.ArrayExp (lv,exp,loc) -> [Cil.Lval lv; exp]
  | IntraCfg.Cmd.DerefExp (exp,_) -> [exp; Cil.Const (Cil.CInt64 (Int64.of_int 0, IInt, None))]
  | _ -> raise (Failure "alarmExp2args")

let inserted = ref false

let gen_airac_observe alarm_exp loc = 
  let var = Cil.makeGlobalVar "airac_observe" (TFun (TVoid [], Some [], true, [] )) in
  let airac_observe = Cil.Lval (Cil.Var var, Cil.NoOffset) in
  let args = alarmExp2args alarm_exp in
    Call (None, airac_observe, args, loc)

class insertObserveVisitor (alarm_exp) = object(self)
  inherit nopCilVisitor 
  method vinst (i : instr) = 
    match i with
    | Set (lhs, rhs, loc) when (not !inserted) && (found (rhs, loc, alarm_exp) || found (Lval lhs, loc, alarm_exp))  -> 
          inserted := true;
          ChangeTo [gen_airac_observe alarm_exp loc; Set (lhs,rhs,loc)]
    | Call (ret, fexp, args, loc) when (not !inserted) && (found (fexp, loc, alarm_exp) || found_s (args, loc, alarm_exp)) ->
          inserted := true;
          ChangeTo [gen_airac_observe alarm_exp loc; Call (ret, fexp, args, loc)]
    | _ -> DoChildren

  method vstmt (s : stmt) = 
    match s.skind with 
    | If (e, b1, b2, loc) when (not !inserted) && found (e, loc, alarm_exp) ->
        inserted:=true;
        let observe_stmt = {labels=[]; skind = Instr [gen_airac_observe alarm_exp loc]; sid=0; succs=[]; preds=[]} in
        let skind = Block { battrs = []; bstmts = [observe_stmt;s] } in
        ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    | Return (Some exp, loc) when (not !inserted) && found (exp, loc, alarm_exp) ->
        inserted := true;
        let observe_stmt = {labels=[]; skind = Instr [gen_airac_observe alarm_exp loc]; sid=0; succs=[]; preds=[]} in
        let skind = Block { battrs = []; bstmts = [observe_stmt;s] } in
        ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    | Switch (exp, _,_,loc) when (not !inserted) && found (exp, loc, alarm_exp) ->
        inserted := true;
        let observe_stmt = {labels=[]; skind = Instr [gen_airac_observe alarm_exp loc]; sid=0; succs=[]; preds=[]} in
        let skind = Block { battrs = []; bstmts = [observe_stmt;s] } in
        ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    | _ -> DoChildren 
end

class insertObserveVisitorPtsto (alarm_exp, str_of_loc_alexp) = object(self)
  inherit nopCilVisitor 
  method vinst (i : instr) = 
    match i with
    | Set (lhs, rhs, loc) when (not !inserted) && (found_ptsto (rhs, loc, str_of_loc_alexp) || found_ptsto (Lval lhs, loc, str_of_loc_alexp))  -> 
          inserted := true;
          ChangeTo [ gen_airac_observe alarm_exp loc;  Set (lhs,rhs,loc)]
    | Call (ret, fexp, args, loc) when (not !inserted) && (found_ptsto (fexp, loc, str_of_loc_alexp) || found_s_ptsto (args, loc, str_of_loc_alexp)) ->
          inserted := true;
          ChangeTo [ gen_airac_observe alarm_exp loc;  Call (ret, fexp, args, loc)]
    | _ -> DoChildren

  method vstmt (s : stmt) =
    match s.skind with 
    | If (e, b1, b2, loc) when (not !inserted) && found_ptsto (e, loc, str_of_loc_alexp) ->
        inserted:=true;
        let observe_stmt = {labels=[]; skind = Instr [gen_airac_observe alarm_exp loc]; sid=0; succs=[]; preds=[]} in
        let skind = Block { battrs = []; bstmts = [observe_stmt;s] } in
        ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    | Return (Some exp, loc) when (not !inserted) && found_ptsto (exp, loc, str_of_loc_alexp) ->
        inserted := true;
        let observe_stmt = {labels=[]; skind = Instr [gen_airac_observe alarm_exp loc]; sid=0; succs=[]; preds=[]} in
        let skind = Block { battrs = []; bstmts = [observe_stmt;s] } in
        ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    | Switch (exp, _,_,loc) when (not !inserted) && found_ptsto (exp, loc, str_of_loc_alexp) ->
        inserted := true;
        let observe_stmt = {labels=[]; skind = Instr [gen_airac_observe alarm_exp loc]; sid=0; succs=[]; preds=[]} in
        let skind = Block { battrs = []; bstmts = [observe_stmt;s] } in
        ChangeTo {labels = s.labels; skind = skind; sid = s.sid; succs = s.succs; preds = s.preds}
    | _ -> DoChildren 
end

class removeObserveVisitor () = object(self)
  inherit nopCilVisitor
  method vinst (i : instr) = 
    match i with
    | Call (None, Cil.Lval (Cil.Var f, Cil.NoOffset), _, _) when f.vname = "airac_observe" ->
        ChangeTo []
    | _ -> DoChildren 
end

