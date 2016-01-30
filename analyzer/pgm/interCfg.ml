(** Interprocedural CFG. *)

open Vocab
open Cil
open Yojson.Safe

module SS = Set.Make(String)

type pid = string
module Node = struct
  type t = pid * IntraCfg.node
  let name = "Node"
  let to_string (pid,node) = pid ^ "-" ^ IntraCfg.Node.to_string node
  let to_json x = `String (to_string x)
  let make pid node = (pid,node)
  let get_pid (pid,node) = pid
  let get_cfgnode (pid,node) = node
  let compare = compare
  let hash = Hashtbl.hash
  let equal (p1, n1) (p2, n2) = p1 = p2 && IntraCfg.Node.equal n1 n2
end

type node = Node.t

type t = {
  cfgs : (pid, IntraCfg.t) BatMap.t;
  globals : Cil.global list
}

let dummy = {
  cfgs = BatMap.empty;
  globals = []
}

let global_proc = "_G_"
let start_node = Node.make global_proc IntraCfg.Node.ENTRY

let gen_cfgs file = 
  BatMap.add global_proc (
      IntraCfg.generate_global_proc file.Cil.globals (Cil.emptyFunction global_proc)
      )
    (
		 list_fold (fun g m ->
      match g with
      | Cil.GFun (f,_) -> BatMap.add f.svar.vname (IntraCfg.fromFunDec f) m
      | _ -> m
    ) file.Cil.globals BatMap.empty)

let compute_dom_and_scc icfg = 
try 
  { icfg with cfgs = 
      BatMap.map (fun cfg ->
        IntraCfg.compute_scc (IntraCfg.compute_dom cfg)
      ) icfg.cfgs }
with _ -> prerr_endline "InterCfg.compute_dome_scc"; raise (Failure "InterCfg.compute_dom_and_scc")

let init : Cil.file -> t
=fun file -> { cfgs = gen_cfgs file; globals = file.Cil.globals } 

let remove_function : pid -> t -> t
=fun pid icfg -> 
  { icfg with cfgs = BatMap.remove pid icfg.cfgs }

let cfgof : t -> pid -> IntraCfg.t 
=fun g pid -> BatMap.find pid g.cfgs

let cmdof : t -> Node.t -> IntraCfg.cmd
=fun g (pid,node) -> IntraCfg.find_cmd node (cfgof g pid)

let add_cmd : t -> Node.t -> IntraCfg.cmd -> t
=fun g (pid,node) cmd -> 
  {g with cfgs = BatMap.add pid (IntraCfg.add_cmd node cmd (cfgof g pid)) g.cfgs}

let nodes_of_pid : t -> pid -> Node.t list
=fun g pid -> List.map (Node.make pid) (IntraCfg.nodesof (cfgof g pid))

let fold_cfgs f g a = BatMap.foldi f g.cfgs a

let nodesof : t -> Node.t list
=fun g ->
  BatMap.foldi (fun pid cfg ->
    List.append 
        (List.map (fun n -> Node.make pid n) (IntraCfg.nodesof cfg))
  ) g.cfgs []

let pidsof : t -> pid list
=fun g ->
  BatMap.foldi (fun pid _ acc -> pid :: acc) g.cfgs []

let is_undef : pid -> t -> bool
=fun pid g ->
  not (BatMap.mem pid g.cfgs)

let is_callnode : node -> t -> bool
=fun (pid,node) g -> IntraCfg.is_callnode node (cfgof g pid)

let is_returnnode : node -> t -> bool
=fun (pid,node) g -> IntraCfg.is_returnnode node (cfgof g pid)

let returnof : node -> t -> node 
=fun (pid,node) g -> (pid, IntraCfg.returnof node (cfgof g pid))

let is_inside_loop : node -> t -> bool
=fun (pid,node) g -> IntraCfg.is_inside_loop node (cfgof g pid)

let callof : node -> t -> node
=fun (pid,node) g -> (pid, IntraCfg.callof node (cfgof g pid))

let argsof : t -> pid -> string list
=fun g pid -> IntraCfg.get_formals (cfgof g pid)

let callnodesof : t -> node list
=fun g -> List.filter (fun node -> is_callnode node g) (nodesof g)

let entryof : t -> pid -> node
=fun g pid -> Node.make pid IntraCfg.Node.ENTRY

let exitof : t -> pid -> node
=fun g pid -> Node.make pid IntraCfg.Node.EXIT

let unreachable_node_pid : pid -> IntraCfg.t -> node BatSet.t
=fun pid icfg ->
  BatSet.map (fun node -> (pid, node)) (IntraCfg.unreachable_node icfg)

let unreachable_node : t -> node BatSet.t
=fun g ->
  let add_unreachable_node pid icfg =
    BatSet.union (unreachable_node_pid pid icfg) in
  fold_cfgs add_unreachable_node g BatSet.empty

let remove_node : node -> t -> t
=fun (pid, intra_node) g ->
  let intra_cfg = cfgof g pid in
  let intra_cfg = IntraCfg.remove_node intra_node intra_cfg in
  { g with cfgs = BatMap.add pid intra_cfg g.cfgs }

let print : out_channel -> t -> unit
=fun chan g -> BatMap.iter (fun pid cfg -> IntraCfg.print_dot chan cfg) g.cfgs

(* store cfgs in the given directory, one file for each cfg *)
let store_cfgs : string -> t -> unit
=fun dirname g ->
  BatMap.iter (fun pid cfg ->
    let filename = dirname ^ "/" ^ pid ^ ".dot" in
    let out = open_out filename in
    IntraCfg.print_dot out cfg;
    close_out out
  ) g.cfgs

(*NOTE*)
let dependency : t -> t
=fun inter ->
	let new_cfgs = BatMap.map (fun cfg ->
			IntraCfg.dependency cfg
		) inter.cfgs in
	{cfgs = new_cfgs;
	 globals = inter.globals}

(*NOTE*)
let print_intras : t -> unit
=fun inter ->
	BatMap.iter (fun pid cfg ->
			IntraCfg.print_intra_cmds cfg IntraCfg.Node.ENTRY
		) inter.cfgs

let to_json : t -> json
= fun g ->
  `Assoc (
    BatMap.foldi (fun pid cfg json ->
      (pid, IntraCfg.to_json cfg)::json) g.cfgs [])

let print_json : out_channel -> t -> unit
= fun chan g ->
  Yojson.Safe.pretty_to_channel chan (to_json g)
