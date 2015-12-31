open Graph
open Cil
open Global
open AbsDom
open Vocab
open Frontend
open ItvDom
open ItvPre
open ItvSem
open IntraCfg
open IntraCfg.Cmd
open EvalOp

module C = Cil
module F = Frontc
module E = Errormsg

module Access = Access.Make (Loc)
module Pre = Pre.Make(ItvAccessSem)
module DUGraph = Dug.MakeSet(InterCfg.Node)(Loc)
module SSA = DugGen.Make(Pre)(DUGraph)
module ItvSparseAnalysis = SparseAnalysis.Make(Pre)(ItvSem)(Table)(DUGraph)

type locset = Loc.t BatSet.t

type feature = {
  gvars : locset; (* global variable: done. *)
  lvars : locset; (* local variable: done. *)
  lvars_in_G : locset; (* local variables of _G_ : done *)
  fields : locset; (* structure fields : done *)
  ptr_type : locset; (* TODO *)
  allocsites : locset; (* allocsites : done *)
  static_array : locset;  (* TODO *)
  ext_allocsites : locset; (* external allocsites : done *)
  single_defs : locset; (* defined at single-site: done.*)
  assign_const : locset; (* e.g. x = (c1 + c2): done. *)
  assign_sizeof : locset; (* e.g., x = sizeof(...): done *)
  prune_simple : locset; (* make_prune_simple worked: done *)
  prune_by_const : locset; (* e.g., x < c: done *)
  prune_by_var : locset; (* e.g., x < y: done *)
  prune_by_not : locset; (* e.g., !x: done *)
  pass_to_alloc : locset; (* e.g., malloc(x): done *)
  pass_to_alloc2 : locset; (* e.g., y = x; malloc(y): done *)
  pass_to_alloc_clos : locset; (* e.g., y = x; malloc(y): done *)
  pass_to_realloc : locset; (* e.g., realloc(x): done *)
  pass_to_realloc2 : locset; (* e.g., y = x; realloc(y): done *)
  pass_to_realloc_clos : locset; (* e.g., y = x; realloc(y): done *)
  pass_to_buf : locset; (* e.g., buf = x; done *)
  return_from_alloc : locset; (* x := malloc(...): done *)
  return_from_alloc2 : locset; (* y := malloc(...); x = y: done *)
  return_from_alloc_clos : locset; (* y := malloc(...); x = y: done *)
  return_from_realloc : locset; (* x := malloc(...): done *)
  return_from_realloc2 : locset; (* y := malloc(...); x = y: done *)
  return_from_realloc_clos : locset; (* y := malloc(...); x = y: done *)
  inc_itself_by_one : locset; (* e.g., x = x + 1: done *)
  inc_itself_by_var : locset; (* e.g., x = x + y *) 
  incptr_itself_by_one : locset; (* e.g., x = x + 1 (x is a pointer): done *)
  inc_itself_by_const : locset; (* e.g., x = x + c (where c > 1): done *)
  incptr_itself_by_const : locset; (* e.g., x = x + c (x is a pointer) (where c > 1): done *)
  inc : locset; (* e.g., x = y + 1 : done *)
  dec : locset; (* e.g., x = y - 1 : done *)
  dec_itself : locset; (* e.g., x = x - y : done *)
  dec_itself_by_const : locset; (* e.g., x = x - c : done *) (* TODO *)
  mul_itself_by_const : locset; (* e.g., x = x * 2 : done *)
  mul_itself_by_var : locset; (* e.g., x = x * y : done *) (* TODO *)
  used_as_array_index : locset;  (* e.g., arr[x]: done *)
  used_as_array_buf : locset; (* e.g., x[i] : done *)
  mod_in_rec_fun : locset; (* modified inside recursive functions : done *)
  read_in_rec_fun : locset; (* modified inside recursive functions *) (* TODO *)
  return_from_ext_fun : locset; (* e.g., x = ext_function() : done *) 
  mod_inside_loops : locset; (* while (1) { ... x:= ... } : done *)
  used_inside_loops : locset (* while (1) { ... :=x ... } : done *)
}

let empty_feature = {
  gvars = BatSet.empty;
  lvars = BatSet.empty;
  fields = BatSet.empty;
  ptr_type = BatSet.empty;
  allocsites = BatSet.empty;
  static_array = BatSet.empty;
  ext_allocsites = BatSet.empty;
  single_defs = BatSet.empty;
  assign_const = BatSet.empty;
  assign_sizeof = BatSet.empty;
  prune_simple = BatSet.empty;
  prune_by_const = BatSet.empty;
  prune_by_var = BatSet.empty;
  prune_by_not = BatSet.empty;
  pass_to_alloc = BatSet.empty;
  pass_to_alloc2 = BatSet.empty;
  pass_to_alloc_clos = BatSet.empty;
  pass_to_realloc = BatSet.empty;
  pass_to_realloc2 = BatSet.empty;
  pass_to_realloc_clos = BatSet.empty;
  pass_to_buf = BatSet.empty;
  return_from_alloc = BatSet.empty;
  return_from_alloc2 = BatSet.empty;
  return_from_alloc_clos = BatSet.empty;
  return_from_realloc = BatSet.empty;
  return_from_realloc2 = BatSet.empty;
  return_from_realloc_clos = BatSet.empty;
  inc_itself_by_one = BatSet.empty;
  inc_itself_by_var = BatSet.empty;
  incptr_itself_by_one = BatSet.empty;
  inc_itself_by_const = BatSet.empty;
  incptr_itself_by_const = BatSet.empty;
  mul_itself_by_const = BatSet.empty;
  mul_itself_by_var = BatSet.empty;
  used_as_array_index = BatSet.empty;
  mod_in_rec_fun = BatSet.empty;
  read_in_rec_fun = BatSet.empty;
  lvars_in_G = BatSet.empty;
  dec_itself = BatSet.empty;
  dec_itself_by_const = BatSet.empty;
  dec = BatSet.empty;
  inc= BatSet.empty;
  used_as_array_buf = BatSet.empty;
  return_from_ext_fun = BatSet.empty;
  mod_inside_loops = BatSet.empty;
  used_inside_loops = BatSet.empty
}

let prerr_feature feature = 
  let l2s locs = string_of_set Loc.to_string locs in
  prerr_endline "== features for variable ranking ==";
  prerr_endline ("gvars : " ^ l2s feature.gvars);
  prerr_endline ("lvars : " ^ l2s feature.lvars);
  prerr_endline ("fields : " ^ l2s feature.fields);
  prerr_endline ("ptr_type : " ^ l2s feature.ptr_type);
  prerr_endline ("allocsites : " ^ l2s feature.allocsites);
  prerr_endline ("static_array : " ^ l2s feature.static_array);
  prerr_endline ("ext_allocsites : " ^ l2s feature.ext_allocsites);
  prerr_endline ("single_def : " ^ l2s feature.single_defs);
  prerr_endline ("assigned_const : " ^ l2s feature.assign_const);
  prerr_endline ("assigned_sizeof : " ^ l2s feature.assign_sizeof);
  prerr_endline ("prune_simple : " ^ l2s feature.prune_simple);
  prerr_endline ("prune_const : " ^ l2s feature.prune_by_const);
  prerr_endline ("prune_var : " ^ l2s feature.prune_by_var);
  prerr_endline ("prune_not : " ^ l2s feature.prune_by_not);
  prerr_endline ("pass_to_alloc : " ^ l2s feature.pass_to_alloc);
  prerr_endline ("pass_to_alloc2 : " ^ l2s feature.pass_to_alloc2);
  prerr_endline ("pass_to_alloc_clos : " ^ l2s feature.pass_to_alloc_clos);
  prerr_endline ("pass_to_realloc : " ^ l2s feature.pass_to_realloc);
  prerr_endline ("pass_to_realloc2 : " ^ l2s feature.pass_to_realloc2);
  prerr_endline ("pass_to_realloc_clos : " ^ l2s feature.pass_to_realloc_clos);
  prerr_endline ("pass_to_buf : " ^ l2s feature.pass_to_buf);
  prerr_endline ("return_from_alloc : " ^ l2s feature.return_from_alloc);
  prerr_endline ("return_from_alloc2 : " ^ l2s feature.return_from_alloc2);
  prerr_endline ("return_from_alloc_clos : " ^ l2s feature.return_from_alloc_clos);
  prerr_endline ("return_from_realloc : " ^ l2s feature.return_from_realloc);
  prerr_endline ("return_from_realloc2 : " ^ l2s feature.return_from_realloc2);
  prerr_endline ("return_from_realloc_clos : " ^ l2s feature.return_from_realloc_clos);
  prerr_endline ("inc_itself_by_one : " ^ l2s feature.inc_itself_by_one);
  prerr_endline ("incptr_itself_by_one : " ^ l2s feature.incptr_itself_by_one);
  prerr_endline ("inc_itself_by_const : " ^ l2s feature.inc_itself_by_const);
  prerr_endline ("incptr_itself_by_const : " ^ l2s feature.incptr_itself_by_const);
  prerr_endline ("mul_itself_by_const : " ^ l2s feature.mul_itself_by_const);
  prerr_endline ("mul_itself_by_var : " ^ l2s feature.mul_itself_by_var);
  prerr_endline ("dec_itself : " ^ l2s feature.dec_itself);
  prerr_endline ("inc : " ^ l2s feature.inc);
  prerr_endline ("inc_by_var : " ^ l2s feature.inc_itself_by_var);
  prerr_endline ("dec : " ^ l2s feature.dec);
  prerr_endline ("used_as_array_index : " ^ l2s feature.used_as_array_index);
  prerr_endline ("used_as_array_buf : " ^ l2s feature.used_as_array_buf);
  prerr_endline ("mod_in_rec_fun : " ^ l2s feature.mod_in_rec_fun);
  prerr_endline ("lvars_in_G : " ^ l2s feature.lvars_in_G);
  prerr_endline ("returned_from_ext_fun : " ^ l2s feature.return_from_ext_fun);
  prerr_endline ("mod_inside_loops : " ^ l2s feature.mod_inside_loops);
  prerr_endline ("used_inside_loops : " ^ l2s feature.used_inside_loops)
  
(* simplify expressions:
   1. remove casts
   2. remove coefficients
   e.g.,  (int)(sizeof(int) * (int)x) -> x )
*)
let rec simplify_exp e = 
  e |> remove_casts 
    |> remove_coeffs

and remove_casts e = 
  match e with
  | CastE (typ,e1) -> remove_casts e1
  | BinOp (bop,e1,e2,typ) -> BinOp (bop,remove_casts e1, remove_casts e2,typ)
  | UnOp (uop,e1,typ) -> UnOp (uop,remove_casts e1,typ)
  | _ -> e

and remove_coeffs e = 
  match e with
  | BinOp (_,e1,e2,_) when is_const e1 -> remove_coeffs e2
  | BinOp (_,e1,e2,_) when is_const e2 -> remove_coeffs e1
  | UnOp (_,e1,_) -> remove_coeffs e1
  | _ -> e

and is_const e = 
  match e with
  | Const _ -> true
  | SizeOf _ -> true
  | SizeOfE _ -> true
  | SizeOfStr _ -> true
  | UnOp (_,e,_) -> is_const e
  | BinOp (_,e1,e2,_) -> is_const e1 && is_const e2
  | _ -> false

and is_sizeof e =
  match e with
  | SizeOf _ 
  | SizeOfE _ 
  | SizeOfStr _ -> true
  | _ -> false

and is_var e = 
  match e with
  | Lval (Var _, NoOffset) -> true
  | _ -> false

let inc_itself_by_one (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusA, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i = 1 -> true
  | _ -> false
 
let incptr_itself_by_one (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusPI, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i = 1 -> true
  | _ -> false

let inc_itself_by_const (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusA, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i > 1 -> true
  | _ -> false

let inc_itself_by_var (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusA, Lval (Var y,NoOffset), Lval (Var z, NoOffset) ,_)) 
     when x.vname = y.vname -> true
  | _ -> false


let incptr_itself_by_const (lv,e) =
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusPI, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i > 1 -> true
  | _ -> false

let mul_itself_by_const (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (Mult, Lval (Var y,NoOffset),Const (CInt64 (i,_,_)),_)) 
     when x.vname = y.vname && Cil.i64_to_int i > 1 -> true
  | _ -> false

let mul_itself_by_var (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (Mult, Lval (Var y,NoOffset), Lval (Var z,NoOffset) ,_)) 
     when x.vname = y.vname  -> true
  | _ -> false

let dec_itself (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (MinusA, Lval (Var y,NoOffset),_,_)) when x=y-> true
  | _ -> false

let is_inc (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (PlusA,_,_,_)) -> true
  | _ -> false

let is_dec (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (MinusA,_,_,_)) -> true
  | _ -> false
 
let is_mul (lv,e) = 
  match lv,e with
  | (Var x, NoOffset), (BinOp (Mult,_,_,_)) -> true
  | _ -> false
 
let is_proc_G loc = 
  match loc with
  | (VarAllocsite.Inl (Var.Inr (p,_)), _) -> p = InterCfg.global_proc
  | _ -> false

let add_assign_const loc feat = 
  { feat with assign_const = BatSet.add loc feat.assign_const }

let add_assign_sizeof loc feat = 
  { feat with assign_sizeof = BatSet.add loc feat.assign_sizeof }

let add_prune_by_const loc feat = 
  { feat with prune_by_const = BatSet.add loc feat.prune_by_const }

let add_prune_by_var loc feat = 
  { feat with prune_by_var = BatSet.add loc feat.prune_by_var }

let add_prune_by_not loc feat = 
  { feat with prune_by_not = BatSet.add loc feat.prune_by_not }

let add_prune_simple loc feat = 
  { feat with prune_simple = BatSet.add loc feat.prune_simple }

let add_pass_to_alloc loc feat = 
  { feat with pass_to_alloc = BatSet.add loc feat.pass_to_alloc }

let add_return_from_alloc loc feat = 
  { feat with return_from_alloc = BatSet.add loc feat.return_from_alloc }

let add_pass_to_alloc2 loc feat = 
  { feat with pass_to_alloc2 = BatSet.add loc feat.pass_to_alloc2 }

let add_return_from_alloc2 loc feat = 
  { feat with return_from_alloc2 = BatSet.add loc feat.return_from_alloc2 }

let add_pass_to_realloc loc feat = 
  { feat with pass_to_realloc = BatSet.add loc feat.pass_to_realloc }

let add_return_from_realloc loc feat = 
  { feat with return_from_realloc = BatSet.add loc feat.return_from_realloc }

let add_pass_to_realloc2 loc feat = 
  { feat with pass_to_realloc2 = BatSet.add loc feat.pass_to_realloc2 }

let add_return_from_realloc2 loc feat = 
  { feat with return_from_realloc2 = BatSet.add loc feat.return_from_realloc2 }

let add_return_from_ext_fun loc feat = 
  { feat with return_from_ext_fun = BatSet.add loc feat.return_from_ext_fun }

let add_inc_itself_by_one loc feat = 
  { feat with inc_itself_by_one = BatSet.add loc feat.inc_itself_by_one }

let add_incptr_itself_by_one loc feat = 
  { feat with incptr_itself_by_one = BatSet.add loc feat.incptr_itself_by_one }

let add_inc_itself_by_const loc feat = 
  { feat with inc_itself_by_const = BatSet.add loc feat.inc_itself_by_const }

let add_inc_itself_by_var loc feat = 
  { feat with inc_itself_by_var = BatSet.add loc feat.inc_itself_by_var }

let add_incptr_itself_by_const loc feat = 
  { feat with incptr_itself_by_const = BatSet.add loc feat.incptr_itself_by_const }

let add_mul_itself_by_const loc feat = 
  { feat with mul_itself_by_const = BatSet.add loc feat.mul_itself_by_const }

let add_mul_itself_by_var loc feat = 
  { feat with mul_itself_by_var = BatSet.add loc feat.mul_itself_by_var }

let add_inc loc feat = 
  { feat with inc = BatSet.add loc feat.inc }

let add_dec_itself loc feat = 
  { feat with dec_itself = BatSet.add loc feat.dec_itself }

let add_dec loc feat = 
  { feat with dec = BatSet.add loc feat.dec }

let add_used_as_array_index loc feat = 
  { feat with used_as_array_index = BatSet.add loc feat.used_as_array_index }

let add_used_as_array_buf loc feat = 
  { feat with used_as_array_buf = BatSet.add loc feat.used_as_array_buf }

let add_mod_in_rec_fun loc feat = 
  { feat with mod_in_rec_fun = BatSet.add loc feat.mod_in_rec_fun }

let add_mod_inside_loops loc feat = 
  { feat with mod_inside_loops = BatSet.add loc feat.mod_inside_loops }

let add_used_inside_loops loc feat = 
  { feat with used_inside_loops = BatSet.add loc feat.used_inside_loops }

let check_op op = op = Lt || op = Gt || op = Le || op = Ge || op = Eq || op = Ne

let extract_set pid (lv,e) mem global feature =
  let locs = eval_lv pid lv mem in
  try 
    feature
    |> (if is_const e then PowLoc.fold add_assign_const locs else id)
    |> (if is_sizeof e then PowLoc.fold add_assign_sizeof locs else id)
    |> (if inc_itself_by_one (lv,e) then PowLoc.fold add_inc_itself_by_one locs else id)
    |> (if incptr_itself_by_one (lv,e) then PowLoc.fold add_incptr_itself_by_one locs else id)
    |> (if incptr_itself_by_const (lv,e) then PowLoc.fold add_incptr_itself_by_const locs else id)
    |> (if inc_itself_by_const (lv,e) then PowLoc.fold add_inc_itself_by_const locs else id)
    |> (if inc_itself_by_var (lv,e) then PowLoc.fold add_inc_itself_by_var locs else id)
    |> (if mul_itself_by_const (lv,e) then PowLoc.fold add_mul_itself_by_const locs else id)
    |> (if mul_itself_by_var (lv,e) then PowLoc.fold add_mul_itself_by_var locs else id)
    |> (if dec_itself (lv,e) then PowLoc.fold add_dec_itself locs else id)
    |> (if is_inc (lv,e) then PowLoc.fold add_inc locs else id)
    |> (if is_dec (lv,e) then PowLoc.fold add_dec locs else id)
    |> (if Global.is_rec global pid then PowLoc.fold add_mod_in_rec_fun locs else id)
  with e -> prerr_endline "extract_set"; raise e

let extract_assume pid e mem global feature =
  feature |>
  (match Prune.make_cond_simple e with
  | None -> id
  | Some cond ->
    PowLoc.fold add_prune_simple 
      (Access.defof (ItvAccessSem.duof_prune_simple AbsDom.Weak global pid cond mem Access.empty))
    >>>
    begin
      (match cond with
      | BinOp (op,Lval x,e,_) when check_op op ->
       begin
        let locs = eval_lv pid x mem in
        (if is_const e then PowLoc.fold add_prune_by_const locs else id)
        >>> (if is_var e then PowLoc.fold add_prune_by_var locs else id)
       end
      | UnOp (LNot,Lval x,_) -> PowLoc.fold add_prune_by_not (eval_lv pid x mem)
      | _ -> id) 
    end)

let extract_alloc pid (lv,e) mem global feature =
  let locs_lv = eval_lv pid lv mem in 
  let locs_e = Access.useof (ItvAccessSem.duof_eval pid e mem Access.empty) in
  feature 
  |> (PowLoc.fold add_pass_to_alloc locs_e)
  |> (PowLoc.fold add_return_from_alloc locs_lv)

let extract_call_realloc pid (lvo,fe,el) mem global feature = 
  match lvo, (simplify_exp fe) with
  | Some lv, Lval (Var f, NoOffset) when f.vname = "realloc" ->
    let locs_lv = eval_lv pid lv mem in
    let locs_e = 
      Access.useof (
        list_fold (fun e -> ItvAccessSem.duof_eval pid e mem
        ) el Access.empty) in
      feature
      |> (PowLoc.fold add_pass_to_realloc locs_e)
      |> (PowLoc.fold add_return_from_realloc locs_lv) 
  | _ -> feature

let is_undef fname global = 
  Global.is_undef fname global &&
  not (fname = "realloc" || fname = "strlen")

let extract_call_ext_fun pid (lvo,fe,el) mem global feature =
  match lvo,fe with
  | Some lv, Cil.Lval (Cil.Var f, Cil.NoOffset) when is_undef f.vname global -> 
    PowLoc.fold add_return_from_ext_fun 
      (Access.useof (ItvAccessSem.duof_eval pid (Lval lv) mem Access.empty))
      feature
  | _ -> feature

let extract_call pid (lvo,fe,el) mem global feature = 
  feature
  |> extract_call_ext_fun pid (lvo,fe,el) mem global
  |> extract_call_realloc pid (lvo,fe,el) mem global

let extract_used_index pid mem cmd feature =
  let queries = AlarmExp.collect cmd in
    list_fold (fun q ->
      let locs = 
        Access.useof 
          (ItvAccessSem.duof_eval pid 
            (match q with
            | Cmd.ArrayExp (_,e,_) -> e
            | Cmd.DerefExp (BinOp(op,_,e2,_),_) -> e2
            | _ -> Const (CStr "") (* dummy exp *))
            mem Access.empty)
      in PowLoc.fold add_used_as_array_index locs
    ) queries feature

let extract_used_buf pid mem cmd feature =
  let queries = AlarmExp.collect cmd in
    list_fold (fun q ->
      let locs = 
        Access.useof 
          (ItvAccessSem.duof_eval pid 
            (match q with
            | Cmd.ArrayExp (lv,_,_) -> Lval lv
            | Cmd.DerefExp (BinOp(op,e1,_,_),_) -> e1
            | Cmd.DerefExp (e,_) -> e
            | _ -> Const (CStr "")
            (* | _ -> Const (CStr "") (\* dummy exp *\) *))
            mem Access.empty)
      in PowLoc.fold add_used_as_array_buf locs
    ) queries feature

let extract_loops pid mem cmd node icfg feature =
  if not (InterCfg.is_inside_loop node icfg) then feature
  else 
   match cmd with
   | IntraCfg.Cmd.Cset (lv,e,_) -> 
     let defs = Access.useof (ItvAccessSem.duof_eval pid (Lval lv) mem Access.empty) in
     let uses = Access.useof (ItvAccessSem.duof_eval pid e mem Access.empty) in
       PowLoc.fold add_mod_inside_loops defs 
        (PowLoc.fold add_used_inside_loops uses feature)
   | _ -> feature

let extract1 : InterCfg.t -> Mem.t -> Global.t -> InterCfg.Node.t -> feature -> feature
=fun icfg mem global node feature ->
  let pid = InterCfg.Node.get_pid node in
  let cmd = InterCfg.cmdof icfg node in
  try
    feature |>
      (match cmd with
      | Cset (lv,e,_) -> extract_set pid (lv,e) mem global
      | Cassume (e,_) -> extract_assume pid e mem global
      | Calloc (lv,IntraCfg.Cmd.Array e,_,_) -> extract_alloc pid (lv,e) mem global
      | Ccall (lvo, fe, el, _) -> extract_call pid (lvo,fe,el) mem global
      | _ -> id) 
    |> (extract_used_index pid mem cmd)
    |> (extract_used_buf pid mem cmd)
    |> (extract_loops pid mem cmd node icfg)
  with e -> prerr_endline "extract1"; raise e

let traverse1 : Pre.t -> Global.t -> feature
=fun pre global ->
  let icfg = Global.get_icfg global in
  let mem = Pre.get_mem pre in
  let nodes = InterCfg.nodesof icfg in
    list_fold (extract1 icfg mem global) nodes empty_feature

(* extract information obtainable after first iteration *)
(* : passed_to_alloc2, returned_from_alloc2 *)
let extract2 : InterCfg.t -> Mem.t -> Global.t -> InterCfg.Node.t -> feature -> feature
=fun icfg mem global node feature ->
  let pid = InterCfg.Node.get_pid node in
  match InterCfg.cmdof icfg node with
  | Cset (lv,e,_) ->
    let locs_lv = eval_lv pid lv mem in
    let locs_e  = Access.useof (ItvAccessSem.duof_eval pid e mem Access.empty) in
    let e = simplify_exp e in
    (match lv,e with
     | (Var x,NoOffset), Lval (Var y,NoOffset) -> (* x := y *)
       let l_x = PowLoc.choose locs_lv in
       let l_y = PowLoc.choose locs_e in
         feature 
         |> (if BatSet.mem l_x feature.pass_to_alloc then add_pass_to_alloc2 l_y else id)
         |> (if BatSet.mem l_y feature.return_from_alloc then add_return_from_alloc2 l_x else id)
         |> (if BatSet.mem l_x feature.pass_to_realloc then add_pass_to_realloc2 l_y else id)
         |> (if BatSet.mem l_y feature.return_from_realloc then add_return_from_realloc2 l_x else id)
     | _ -> feature
    )
  | _ -> feature

let traverse2 : Pre.t -> Global.t -> feature -> feature
=fun pre global feature ->
  let icfg = Global.get_icfg global in
  let mem = Pre.get_mem pre in 
  let nodes = InterCfg.nodesof icfg in
    list_fold (extract2 icfg mem global) nodes feature

module N = struct
  type t = Loc.t
  let compare = compare
  let equal = (=)
  let hash = Hashtbl.hash 
end
module G = Graph.Persistent.Digraph.ConcreteBidirectional(N)

let build_copy_graph icfg mem = 
  list_fold (fun n g ->
    match InterCfg.cmdof icfg n with
    | Cset (lv,e,_) ->
     (match lv,(simplify_exp e) with 
      | (Var x,NoOffset), Lval (Var y,NoOffset) ->
        let pid = InterCfg.Node.get_pid n in
        let lhs = PowLoc.choose (eval_lv pid lv mem) in
        let rhs = PowLoc.choose (Access.useof (ItvAccessSem.duof_eval pid e mem Access.empty)) in
          G.add_edge g rhs lhs 
      | _ -> g)
    | _ -> g
  ) (InterCfg.nodesof icfg) G.empty

let closure : Pre.t -> Global.t -> feature -> feature
=fun pre global feature ->
  let icfg = Global.get_icfg global in
  let mem = Pre.get_mem pre in
  let copy_graph = build_copy_graph icfg mem in
  let pta = feature.pass_to_alloc in
  let rfa = feature.return_from_alloc in
  let ptra = feature.pass_to_realloc in
  let rfra = feature.return_from_realloc in
  let buf = feature.used_as_array_buf in
  let pred g n = try list2set (G.pred g n) with _ -> BatSet.empty in
  let succ g n = try list2set (G.succ g n) with _ -> BatSet.empty in
  let pred_set g s = BatSet.fold (fun n -> BatSet.union (pred g n)) s BatSet.empty in
  let succ_set g s = BatSet.fold (fun n -> BatSet.union (succ g n)) s BatSet.empty in
  let rec clos_backward set = 
    let preds = pred_set copy_graph set in
      if BatSet.subset preds set then set
      else clos_backward (BatSet.union set preds) in
  let rec clos_forward set = 
    let succs = succ_set copy_graph set in
      if BatSet.subset succs set then set 
      else clos_forward (BatSet.union set succs) in
    { feature with 
       pass_to_alloc_clos = BatSet.diff (clos_backward pta) pta;
       pass_to_realloc_clos = BatSet.diff (clos_backward ptra) ptra;
       return_from_alloc_clos = BatSet.diff (clos_forward rfa) rfa;
       return_from_realloc_clos = BatSet.diff (clos_forward rfra) rfra;
       pass_to_buf = BatSet.diff (clos_backward buf) buf }

let extract_feature : Loc.t BatSet.t -> Pre.t -> Global.t -> feature
=fun locset pre global -> 
  let lvars = BatSet.filter Loc.is_loc_lvar locset in
  let lvars_in_G = BatSet.filter is_proc_G lvars in
  let gvars = BatSet.filter Loc.is_loc_gvar locset in
  let fields = BatSet.filter Loc.is_loc_field locset in
  let allocsites = BatSet.filter Loc.is_loc_allocsite locset in
  let ext_allocsites = BatSet.filter Loc.is_loc_ext_alloc allocsites in
  let single_defs = Pre.get_single_defs (Pre.get_defs_of pre) in
  let feature = try traverse1 pre global with e -> prerr_endline "traverse1"; raise e in (* first iteration *)
  let feature = traverse2 pre global feature in (* second iteration *)
  let feature = closure pre global feature in
    { feature with
      gvars = gvars;
      lvars = lvars;
      fields = fields;
      allocsites = allocsites;
      ext_allocsites = ext_allocsites;
      single_defs = single_defs;
      lvars_in_G = lvars_in_G;
    }

let weight_of : Loc.t -> feature -> float
=fun l f -> 
 let weights = Str.split (Str.regexp "[ \t]+") (!Options.opt_weights) in
 let getw i = try float_of_string (List.nth weights (i-1)) with _ -> 0.0 in
 let mem = BatSet.mem in
  0.0
  (* atomic rules *)
  |> (if mem l f.lvars then (+.) (getw 1) else id)
  |> (if mem l f.gvars then (+.) (getw 2) else id)
  |> (if mem l f.fields then (+.) (getw 3) else id)
  |> (if mem l f.allocsites then (+.) (getw 4) else id)
  |> (if mem l f.single_defs then (+.) (getw 5) else id)
  |> (if mem l f.ext_allocsites then (+.) (getw 6) else id)
  |> (if mem l f.assign_const then (+.) (getw 7) else id)
  |> (if mem l f.prune_by_const then (+.) (getw 8) else id)
  |> (if mem l f.prune_by_var then (+.) (getw 9) else id)
  |> (if mem l f.prune_by_not then (+.) (getw 10) else id)
  |> (if mem l f.pass_to_alloc then (+.) (getw 11) else id)
  |> (if mem l f.pass_to_alloc_clos then (+.) (getw 12) else id)
  |> (if mem l f.pass_to_realloc then (+.) (getw 13) else id)
  |> (if mem l f.pass_to_realloc_clos then (+.) (getw 14) else id)
  |> (if mem l f.return_from_alloc then (+.) (getw 15) else id)
  |> (if mem l f.return_from_alloc_clos then (+.) (getw 16) else id)
  |> (if mem l f.return_from_realloc then (+.) (getw 17) else id)
  |> (if mem l f.return_from_realloc_clos then (+.) (getw 18) else id)
  |> (if mem l f.inc_itself_by_one then (+.) (getw 19) else id)
  |> (if mem l f.inc_itself_by_const then (+.) (getw 20) else id)
  |> (if mem l f.inc_itself_by_var then (+.) (getw 21) else id) 
  |> (if mem l f.dec then (+.) (getw 22) else id)
  |> (if mem l f.dec_itself_by_const then (+.) (getw 23) else id)
  |> (if mem l f.dec_itself then (+.) (getw 24) else id)
  |> (if mem l f.mul_itself_by_const then (+.) (getw 25) else id)
  |> (if mem l f.mul_itself_by_var then (+.) (getw 26) else id) 
  |> (if mem l f.incptr_itself_by_one then (+.) (getw 27) else id)
  |> (if mem l f.used_as_array_index then (+.) (getw 28) else id)
  |> (if mem l f.used_as_array_buf then (+.) (getw 29) else id)
  |> (if mem l f.return_from_ext_fun then (+.) (getw 30) else id)
  |> (if mem l f.mod_in_rec_fun then (+.) (getw 31) else id)
  |> (if mem l f.mod_inside_loops then (+.) (getw 32) else id)
  |> (if mem l f.used_inside_loops then (+.) (getw 33) else id)
  (* combination rules *)
  |>  (if mem l f.lvars && mem l f.prune_by_const && (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos)
      then (+.) (getw 34) else id)
  |> (if mem l f.gvars && mem l f.prune_by_const && (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos)
      then (+.) (getw 35) else id)
  |> (if mem l f.lvars && (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos) 
         && (mem l f.inc_itself_by_one || mem l f.inc_itself_by_const)
      then (+.) (getw 36) else id)
  |> (if mem l f.gvars && (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos) 
         && (mem l f.inc_itself_by_one || mem l f.inc_itself_by_const)
      then (+.) (getw 37) else id)
  |> (if mem l f.lvars && (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos) 
         && (mem l f.return_from_alloc || mem l f.return_from_alloc_clos)
      then (+.) (getw 38) else id)
  |> (if mem l f.gvars && (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos) 
         && (mem l f.return_from_alloc || mem l f.return_from_alloc_clos)
      then (+.) (getw 39) else id)
  |> (if (mem l f.pass_to_alloc || mem l f.pass_to_alloc_clos) && mem l f.used_as_array_buf
      then (+.) (getw 40) else id)
  |> (if (mem l f.return_from_alloc || mem l f.return_from_alloc_clos) && mem l f.used_as_array_buf
      then (+.) (getw 41) else id)
  |> (if mem l f.lvars && (mem l f.inc_itself_by_one || mem l f.inc_itself_by_const) 
         && mem l f.mod_inside_loops
      then (+.) (getw 42) else id)
  |> (if mem l f.gvars && (mem l f.inc_itself_by_one || mem l f.inc_itself_by_const) 
         && mem l f.mod_inside_loops
      then (+.) (getw 43) else id)
  |> (if mem l f.lvars && (mem l f.inc_itself_by_one || mem l f.inc_itself_by_const) 
         && not (mem l f.mod_inside_loops)
      then (+.) (getw 44) else id)
  |> (if mem l f.gvars && (mem l f.inc_itself_by_one || mem l f.inc_itself_by_const) 
         && not (mem l f.mod_inside_loops)
      then (+.) (getw 45) else id)
 
let assign_weight locs feature = List.map (fun l -> (l, weight_of l feature)) locs
 
let rank (locset, pre, global) = 
  let weights = Str.split (Str.regexp "[ \t]+") (!Options.opt_weights) in
  let _ = prerr_endline ("Weight vector : " ^ string_of_list id weights) in
  let feature = extract_feature locset pre global in
  let loclist = BatSet.elements locset in
  let locs_weighted = assign_weight loclist feature in
  let sorted = List.sort (fun (_,w) (_,w') -> compare w' w) locs_weighted in
    BatList.map fst sorted

(* take top X-percent-ranked locations, where x : 0 ~ 100 *)
let take_top : int -> Loc.t list -> Loc.t BatSet.t 
=fun x loclist -> 
  let len = List.length loclist in
  let _end = x * len / 100 in
  list2set (BatList.take _end loclist)
