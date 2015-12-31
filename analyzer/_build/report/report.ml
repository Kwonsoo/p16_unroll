open Vocab
open AbsDom
open EvalOp
open ItvDom
open Cil
open IntraCfg
open Cmd
type alarm_type = BO | ND | DZ | PTSTO
type status = Proven | UnProven | BotAlarm

let status_to_string = function Proven -> "Proven" | UnProven -> "UnProven" | _ -> "BotAlarm"

type query = {
  node : InterCfg.node;
  exp : AlarmExp.t;
  loc : Cil.location;
  allocsite : Allocsite.t option; 
  status : status;
  desc : string
}

let is_unproven : query -> bool
=fun q -> q.status = UnProven

let get_pid : query -> string
=fun q -> InterCfg.Node.get_pid q.node

let string_of_alarminfo offset size = 
  "offset: " ^ Itv.to_string offset ^ ", size: " ^ Itv.to_string size
 
let check_bo v1 v2opt : (status * Allocsite.t option * string) list = 
  let arr = array_of_val v1 in
  if ArrayBlk.eq arr ArrayBlk.bot then [(BotAlarm, None, "Array is Bot")] else
    ArrayBlk.foldi (fun a (offset,size,_) lst ->
      let offset = 
        match v2opt with
        | None -> offset
        | Some v2 -> Itv.plus offset (itv_of_val v2) in
      let status = 
        match offset,size with
        | Itv.Bot,_
        | _,Itv.Bot -> BotAlarm
        | Itv.V (Itv.MInf,_),_
        | Itv.V (_,Itv.PInf),_ 
        | _,Itv.V (Itv.MInf,_) -> UnProven
        | Itv.V (Itv.Int ol, Itv.Int oh), Itv.V (Itv.Int sl, _) ->
          if oh >= sl || ol < 0 then UnProven
          else Proven
        | _ -> failwith "Never happen" in
        (status, Some a, string_of_alarminfo offset size)::lst
    ) arr []

let exp_minus_one e = Cil.BinOp (Cil.MinusA,e,Const (CInt64 (Int64.one,IInt, None)), Cil.TInt (Cil.IInt, []))

let inspect_aexp_bo : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  queries @ (
  match aexp with
  | IntraCfg.Cmd.ArrayExp (lv,e,loc) ->
    let v1 = Mem.lookup (eval_lv (InterCfg.Node.get_pid node) lv mem) mem in
    let v2 = EvalOp.eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_bo v1 (Some v2) in
      List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
  | IntraCfg.Cmd.DerefExp (e,loc) ->
    let v = EvalOp.eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_bo v None in 
      if Val.eq Val.bot v then 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
      else 
        List.map (fun (status,a,desc) -> 
          if status = BotAlarm
          then { node = node; exp = aexp; loc = loc; status = BotAlarm; allocsite = a; desc = "BotAlarm" }
          else { node = node; exp = aexp; loc = loc; status = status; allocsite = a; desc = desc }) lst
(*  | IntraCfg.Cmd.Strcpy (e1, e2, loc) ->
    let v1 = EvalOp.eval (InterCfg.Node.get_pid node) e1 mem in
    let v2 = EvalOp.eval (InterCfg.Node.get_pid node) e2 mem in
    let v2 = val_of_itv (Itv.minus (ArrayBlk.nullposof (array_of_val v2)) Itv.one) in
    let lst = check_bo v1 (Some v2) in
      List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
  | IntraCfg.Cmd.Strncpy (e1, _, e3, loc) 
  | IntraCfg.Cmd.Memcpy (e1, _, e3, loc) 
  | IntraCfg.Cmd.Memmove (e1, _, e3, loc) ->
    let v1 = EvalOp.eval (InterCfg.Node.get_pid node) e1 mem in
    let e3_1 = exp_minus_one e3 in
    let v3 = EvalOp.eval (InterCfg.Node.get_pid node) e3_1 mem in
    let lst = check_bo v1 (Some v3) in
      List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
*)  | _ -> []  (* TODO : strcat *)
  )

let check_nd v1 : (status * Allocsite.t option * string) list = 
  let ploc = pow_loc_of_val v1 in
  if PowLoc.eq ploc PowLoc.bot then [(BotAlarm, None, "PowLoc is Bot")] 
  else if PowLoc.mem Loc.null ploc then [(UnProven, None, "Null Dereference")]
  else [(Proven, None, "")]

let inspect_aexp_nd : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  queries @ (
  match aexp with
  | IntraCfg.Cmd.DerefExp (e,loc) ->
    let v = EvalOp.eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_nd v in 
      if Val.eq Val.bot v then 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
      else 
        List.map (fun (status,a,desc) -> 
          if status = BotAlarm
          then { node = node; exp = aexp; loc = loc; status = Proven; allocsite = a; desc = "valid pointer dereference" }
          else { node = node; exp = aexp; loc = loc; status = status; allocsite = a; desc = desc }) lst
  | _ -> [])

let check_ptsto v =
  let ploc = pow_loc_of_val v in
    PowLoc.fold (fun loc lst -> (UnProven,None,Loc.to_string loc)::lst) ploc []

let inspect_aexp_ptsto : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
=fun node aexp mem queries ->
  queries @ (
  match aexp with
  | IntraCfg.Cmd.DerefExp (e,loc) ->
    let v = EvalOp.eval (InterCfg.Node.get_pid node) e mem in
    let lst = check_ptsto v in 
      List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = a; status = status; desc = desc }) lst
  | _ -> [])

let check_dz v = 
  let v = itv_of_val v in
  if Itv.le Itv.zero v then 
    [(UnProven, None, "Divide by "^Itv.to_string v)]
  else [(Proven, None, "")]

let inspect_aexp_dz : InterCfg.node -> AlarmExp.t -> Mem.t -> query list -> query list
= fun node aexp mem queries -> 
  queries @ (
    match aexp with 
      IntraCfg.Cmd.DivExp (_, e, loc) ->
      let v = EvalOp.eval (InterCfg.Node.get_pid node) e mem in
      let lst = check_dz v in 
        List.map (fun (status,a,desc) -> { node = node; exp = aexp; loc = loc; allocsite = None; status = status; desc = desc }) lst
  | _ -> [])


let filter : query list -> status -> query list
= fun qs s -> List.filter (fun q -> q.status = s) qs

let generate ?(silent = false) : Global.t * Table.t * alarm_type -> query list
=fun (global,inputof,target) ->
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
  let total = List.length nodes in
  let (queries,_) =
    list_fold (fun node (qs,k) ->
      prerr_progressbar ~itv:1000 silent k total;
      let mem = Table.find node inputof in
      let cmd = InterCfg.cmdof icfg node in
      let aexps = AlarmExp.collect cmd in 
        if mem = Mem.bot then (qs,k+1) (* dead code *)
        else 
         begin
          match target with
          | BO -> (list_fold (fun aexp  -> inspect_aexp_bo node aexp mem) aexps qs, k+1)
          | ND -> (list_fold (fun aexp  -> inspect_aexp_nd node aexp mem) aexps qs, k+1)
          | DZ -> (list_fold (fun aexp  -> inspect_aexp_dz node aexp mem) aexps qs, k+1)
          | PTSTO -> (list_fold (fun aexp  -> inspect_aexp_ptsto node aexp mem) aexps qs, k+1)
         end
    ) nodes ([],0) in
    queries

let generate_with_mem : Global.t * Mem.t * alarm_type -> query list
=fun (global,mem,target) ->
  let icfg = Global.get_icfg global in
  let nodes = InterCfg.nodesof icfg in
    list_fold (fun node ->
      let cmd = InterCfg.cmdof icfg node in
      let aexps = AlarmExp.collect cmd in 
        if mem = Mem.bot then id (* dead code *)
        else 
          match target with 
          | BO -> list_fold (fun aexp  -> inspect_aexp_bo node aexp mem) aexps 
          | ND -> list_fold (fun aexp  -> inspect_aexp_nd node aexp mem) aexps
          | DZ -> list_fold (fun aexp  -> inspect_aexp_dz node aexp mem) aexps
          | PTSTO -> list_fold (fun aexp  -> inspect_aexp_ptsto node aexp mem) aexps
    ) nodes []

let sort_queries : query list -> query list = 
fun queries ->
  List.sort (fun a b -> 
    if compare a.loc.file b.loc.file = 0 then 
    begin
      if compare a.loc.line b.loc.line = 0 then 
        compare a.exp b.exp 
      else compare a.loc.line b.loc.line
    end
    else compare a.loc.file b.loc.file) queries 

type part_unit = Cil.location 

let sort_partition : (part_unit * query list) list -> (part_unit * query list) list = 
fun queries ->
  List.sort (fun (a,_) (b,_) -> 
    if compare a.file b.file = 0 then 
      compare a.line b.line 
    else compare a.file b.file) queries 

let partition : query list -> (part_unit, query list) BatMap.t
=fun queries ->
  list_fold (fun q m ->
    let p_als = try BatMap.find q.loc m with _ -> [] in
      BatMap.add q.loc (q::p_als) m
  ) queries BatMap.empty

let get_status : query list -> status
=fun queries -> 
  if List.exists (fun q -> q.status = BotAlarm) queries then BotAlarm
  else if List.exists (fun q -> q.status = UnProven) queries then  UnProven
  else if List.for_all (fun q -> q.status = Proven) queries then Proven
  else raise (Failure "Report.ml: get_status")

let get qs status =  
  List.filter (fun q -> q.status = status) qs

let get_proved_query_point : query list -> part_unit BatSet.t
=fun queries ->
  let all = partition queries in
  let unproved = partition (get queries UnProven) in
  let all_loc = BatMap.foldi (fun l _ -> BatSet.add l) all BatSet.empty in
  let unproved_loc = BatMap.foldi (fun l _ -> BatSet.add l) unproved BatSet.empty in
    BatSet.diff all_loc unproved_loc
 
let string_of_query q = 
  (Cil2str.s_location q.loc)^ " "^
  (AlarmExp.to_string q.exp) ^ " @" ^
  (InterCfg.Node.to_string q.node) ^ ":  " ^ 
  (match q.allocsite with 
    Some a -> Allocsite.to_string a
   | _ -> "") ^ "  " ^
  q.desc ^ " " ^ status_to_string (get_status [q])

let print_raw : bool -> query list -> unit
=fun summary_only queries ->
  let unproven = List.filter (fun x -> x.status = UnProven) queries in
  let botalarm = List.filter (fun x -> x.status = BotAlarm) queries in
  prerr_newline ();
  prerr_endline ("= "^"Alarms"^ "=");
  ignore (List.fold_left (fun k q ->
    prerr_string (string_of_int k ^ ". "); 
    prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
    prerr_string (InterCfg.Node.to_string q.node);
    prerr_string ("  ");
    prerr_string (Cil2str.s_location q.loc);
    prerr_endline ( ":  " ^ q.desc );
    k+1
    ) 1 (sort_queries unproven)); 
  prerr_endline "";
  prerr_endline ("#queries                 : " ^ i2s (List.length queries));
  prerr_endline ("#proven                  : " ^ i2s (BatSet.cardinal (get_proved_query_point queries)));
  prerr_endline ("#unproven                : " ^ i2s (List.length unproven));
  prerr_endline ("#bot-involved            : " ^ i2s (List.length botalarm))


let display_alarms title alarms_part = 
  prerr_endline "";
  prerr_endline ("= " ^ title ^ " =");
  let alarms_part = BatMap.bindings alarms_part in
  let alarms_part = sort_partition alarms_part in
  ignore (List.fold_left (fun k (part_unit, qs) ->
    prerr_string (string_of_int k ^ ". " ^ Cil2str.s_location part_unit ^ " "); 
    prerr_string (string_of_set id (list2set (List.map (fun q -> InterCfg.Node.get_pid q.node) qs)));
    prerr_string (" " ^ status_to_string (get_status qs));
    prerr_newline ();
    List.iter (fun q -> 
      prerr_string ( "  " ^ AlarmExp.to_string q.exp ^ " @");
      prerr_string (InterCfg.Node.to_string q.node);
      prerr_string ( ":  " ^ q.desc ^ " " ^ status_to_string (get_status [q]));
      (match q.allocsite with Some a ->
        prerr_endline ( ", allocsite: " ^ Allocsite.to_string a)
       | _ -> ())
    ) qs;
   k+1
  ) 1 alarms_part) 

(* queries1: FI, queries2: FS *)
let diff_alarms queries1 queries2 = 
  let locs1 = BatMap.foldi (fun p _ -> BatSet.add p) queries1 BatSet.empty in
  let locs2 = BatMap.foldi (fun p _ -> BatSet.add p) queries2 BatSet.empty in
  let diff = BatSet.diff locs1 locs2 in
  if BatSet.is_empty diff then prerr_endline "empty set";
    BatSet.fold (fun loc ->
      BatMap.add loc (BatMap.find loc queries1)
    ) diff BatMap.empty


(* get alarm diff, excluding bot alarms *)
let get_alarm_diff q1 q2 = 
  let bot_alarms = (get q1 BotAlarm) @ (get q2 BotAlarm) in
  let q1_unproven, q2_unproven = get q1 UnProven, get q2 UnProven in
  let q1_unproven =  (* exclude bot alarms *)
      List.filter (fun q -> not (List.exists (fun q' -> q.loc = q'.loc) bot_alarms)) q1_unproven in
  let q2_unproven =  (* exclude bot alarms *)
      List.filter (fun q -> not (List.exists (fun q' -> q.loc = q'.loc) bot_alarms)) q2_unproven in
  let part1 = partition q1_unproven in
  let part2 = partition q2_unproven in
    (diff_alarms part1 part2, part1, part2)

let ptstoqs2map qs = 
    list_fold (fun q (m1,m2)->
      let key = Cil2str.s_location q.loc ^ " " ^ AlarmExp.to_string q.exp in
      let old = try BatMap.find key m1 with _ -> BatSet.empty in
        (BatMap.add key (BatSet.add q.desc old) m1, 
         BatMap.add key q.exp m2)
    ) qs (BatMap.empty, BatMap.empty)
 
(* qs1: FS, qs2: FI *)
let print_pts_new show_diff qs1 qs2 = 
  let string_of_key key = key in
  let (map1,_) = ptstoqs2map qs1 in
  let (map2,_) = ptstoqs2map qs2 in
    if not show_diff then
      ignore (BatMap.foldi (fun key set k ->
        prerr_endline (string_of_int k ^ ". " ^ string_of_key key ^ " |-> " ^
        (string_of_set id set));
        k+1
      ) map1 1)
    else
      ignore (BatMap.foldi (fun key set2 k ->
        let set1 = try BatMap.find key map1 with _ -> BatSet.empty in
        let diff = BatSet.diff set2 set1 in  (* diff = FI \ FS *)
          if BatSet.is_empty (BatSet.diff set2 set1) then (* no difference *)
            k
          else
            (prerr_endline (string_of_int k ^ ". " ^
              string_of_key key ^ " |-> " ^ string_of_set id diff);
              k+1)
      ) map2 1)

let print_pts show_diff queries1 queries2 = 
  if show_diff then
    begin
      print_raw false queries1;
      print_raw false queries2;
      let diff,_,_ = get_alarm_diff queries2 queries1 in
      let alarms = BatMap.bindings diff in
        ignore (list_fold (fun (loc, alarms) k ->
            prerr_endline (string_of_int k); k+1
        ) alarms 1)
    end
  else
    begin
    let n_alarms = List.fold_left (fun n q ->
      if q.desc = "null" then n
      else 
        begin
        prerr_endline (string_of_int n ^ ". LOC:" ^ Cil2str.s_location q.loc ^ " NODE:" ^ InterCfg.Node.to_string q.node ^ " EXPRESSION:" ^ AlarmExp.to_string q.exp ^ " ABSLOC:" ^ q.desc);
        n+1
        end
    ) 1 queries1 in
    prerr_endline "";
    prerr_endline ("#points-to locations : " ^ i2s n_alarms)
    end

(* queries1 : FS alarm, queries2 : FI alarm *)
let print : bool -> bool -> query list -> query list -> alarm_type -> unit
=fun summary_only show_diff queries1 queries2 alarm_type ->
  match alarm_type with
  | PTSTO -> print_pts_new show_diff queries1 queries2
  | _ -> 
    let all1 = partition queries1 in
    let all2 = partition queries2 in
    let unproven1 = partition (get queries1 UnProven) in
    let unproven2 = partition (get queries2 UnProven) in
    let bot1 = partition (get queries1 BotAlarm) in
    let bot2 = partition (get queries2 BotAlarm) in
    if not show_diff then
      (if not summary_only then
        begin
          display_alarms "Alarms" unproven1; 
        end
      else ();
      prerr_endline "";
      prerr_endline ("#queries                 : " ^ i2s (List.length queries1));
      prerr_endline ("#queries mod alarm point : " ^ i2s (BatMap.cardinal all1));
      prerr_endline ("#proven                  : " ^ i2s (BatSet.cardinal (get_proved_query_point queries1)));
      prerr_endline ("#unproven                : " ^ i2s (BatMap.cardinal unproven1));
      prerr_endline ("#bot-involved            : " ^ i2s (BatMap.cardinal bot1)))
    else (* show diff : alarms in queries1 but not in queries 2 *)
      (display_alarms "Alarm Diff" ((fun (x,_,_) -> x) (get_alarm_diff queries2 queries1)))


let diff qs1_part qs2_part = 
  BatMap.foldi (fun punit qs1 map ->
    if BatMap.mem punit qs2_part
    then map
    else BatMap.add punit qs1 map
  ) qs1_part BatMap.empty

let print_compare : bool -> query list -> query list -> unit
=fun summary_only qs1 qs2 ->
  let qs1 = List.filter (fun q -> q.status <> BotAlarm) qs1 in
  let qs2 = List.filter (fun q -> q.status <> BotAlarm) qs2 in
  (* non-bottom alarms *)
  let qs1p = partition qs1 in
  let qs2p = partition qs2 in
  (* common, non-bottom alarms *)
  let (qs1pc,qs2pc) = 
      BatMap.foldi (fun punit qs2 (qs1pc,qs2pc) ->
        if BatMap.mem punit qs1p 
        then (BatMap.add punit (BatMap.find punit qs1p) qs1pc,
              BatMap.add punit qs2 qs2pc)
        else (qs1pc,qs2pc)
      ) qs2p (BatMap.empty, BatMap.empty) in
  (* remaining unproven alarms *)
  let qs1pcu = partition (get qs1 UnProven) in
  let qs2pcu = partition (get qs2 UnProven) in 
   prerr_endline ("#1 queries in common : " ^ i2s (BatMap.cardinal qs1pc));
   prerr_endline ("#2 queries in common : " ^ i2s (BatMap.cardinal qs2pc));
   prerr_endline ("#1 alarms : " ^ i2s (BatMap.cardinal qs1pcu));
   prerr_endline ("#2 alarms : " ^ i2s (BatMap.cardinal qs2pcu));
   if not summary_only then 
     begin
       display_alarms "1" qs1pcu;
       display_alarms "2" qs2pcu;
       display_alarms "1-2" (diff qs1pcu qs2pcu)
     end

let get_alarm_type () =
  match !Options.opt_alarm_type with
  | "bo" -> BO
  | "nd" -> ND
  | "dz" -> DZ 
  | "ptsto" -> PTSTO
  |  _   -> raise (Failure ("Unknown alarm type: " ^ !Options.opt_alarm_type)) 


