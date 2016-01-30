open Vocab
open Yojson.Safe

module type NODE_TYPE = sig
  type t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int
  val to_string : t -> string
end

module type ABSLOC_TYPE = sig
  type t 
  val to_string : t -> string
  val dummy_loc : t
end

module type DUSET_TYPE = sig
  type t
end

module type DUG = 
sig
  type t
  module N : NODE_TYPE
  module A : ABSLOC_TYPE
  module DUSet : DUSET_TYPE 
  type vertex
  type loc
  val empty : t
  val init              : vertex list -> loc BatSet.t -> t
  val finalize          : t -> unit
  val nodesof           : t -> vertex BatSet.t
  val reachable_nodesof : vertex -> t -> vertex BatSet.t 
  val succ              : vertex -> t -> vertex list
  val pred              : vertex -> t -> vertex list
  val add_edge          : vertex -> vertex -> t -> t
  val remove_node       : vertex -> t -> t
  val remove_edge       : vertex -> vertex -> t -> t
  val get_abslocs       : vertex -> vertex -> t -> loc BatSet.t
  val get_duset         : vertex -> vertex -> t -> DUSet.t
  val mem_duset         : loc -> DUSet.t -> bool
  val add_absloc        : vertex -> loc -> vertex -> t -> t
  val add_abslocs       : vertex -> loc BatSet.t -> vertex -> t -> t
  val remove_absloc     : vertex -> loc -> vertex -> t -> t
  val remove_abslocs    : vertex -> loc BatSet.t -> vertex -> t -> t
  val in_degree         : t -> vertex -> int
  val fold_node         : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
  val fold_edges        : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a 
  val iter_edges        : (vertex -> vertex -> unit) -> t -> unit
  val sizeof            : t -> int
  val to_string_in_dot  : t -> string
  val to_json           : t -> json
end

module MakeSet (N: NODE_TYPE) (A: ABSLOC_TYPE) 
: DUG with type vertex = N.t and type loc = A.t =  
struct
  module N = N
  module A = A
  module Label = struct
    type t = A.t BatSet.t
    let compare = compare
    let default = BatSet.empty
  end
  module G = Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (N) (Label)

  type t = G.t * ((N.t * N.t), A.t BatSet.t) Hashtbl.t
  type vertex = N.t
  type loc = A.t

  module DUSet = struct
    type t = loc BatSet.t
  end

  (*TODO: Make not use this one.
  Hashtbl.create is imperitive command *)
  let empty = (G.empty, Hashtbl.create 251) 
  let empty_ () = (G.empty, Hashtbl.create 251)

  let init _ _ = empty_ ()
  let finalize _ = ()

  let nodesof (g, _) = G.fold_vertex BatSet.add g BatSet.empty
  
  module Check = Graph.Path.Check(G)
  let reachable_nodesof entry (g, _) =
    let checker = Check.create g in
    G.fold_vertex (fun v s -> 
        prerr_string (N.to_string v);
        if Check.check_path checker entry v then 
        begin
          BatSet.add v s
        end
        else 
        begin
          prerr_endline " : unreachable";
          s
        end) g BatSet.empty

  let succ n (g, _) = try G.succ g n with _ -> []
  let pred n (g, _) = try G.pred g n with _ -> []

  let remove_node : vertex -> t -> t 
  =fun n (g, cache) ->
    ((if G.mem_vertex g n then G.remove_vertex g n else g), cache)

  let add_edge : vertex -> vertex -> t -> t
  =fun src dst (g, cache) -> (G.add_edge g src dst, cache)

  let remove_edge : vertex -> vertex -> t -> t
  =fun src dst (g, cache) ->
    ((try G.remove_edge g src dst with _ -> g), cache)


  let get_abslocs : vertex -> vertex -> t -> A.t BatSet.t
  =fun src dst (g, cache) ->
    Profiler.start_event "Dug.add_absloc";
    let r =
      try Hashtbl.find cache (src, dst) with Not_found ->
        let r = try G.E.label (G.find_edge g src dst) with _ -> BatSet.empty in
        (Hashtbl.add cache (src, dst) r; r) in
    Profiler.finish_event "Dug.add_absloc";
    r

  let get_duset = get_abslocs

  let mem_duset : loc -> DUSet.t -> bool
  =fun x duset -> BatSet.mem x duset

  let add_absloc : vertex -> A.t -> vertex -> t -> t
  =fun src x dst (g, cache) ->
    let old_locs = get_abslocs src dst (g, cache) in
    let new_locs = BatSet.add x old_locs in
    let edge = G.E.create src new_locs dst in
    let (g, _) = remove_edge src dst (g, cache) in
    Hashtbl.replace cache (src, dst) new_locs;
    (G.add_edge_e g edge, cache)

  let add_abslocs : vertex -> A.t BatSet.t -> vertex -> t -> t
  =fun src xs dst (g, cache) ->
    if BatSet.is_empty xs then (g, cache) else
      let old_locs = get_abslocs src dst (g, cache) in
      let new_locs = BatSet.union xs old_locs in
      let edge = G.E.create src new_locs dst in
      let (g, _) = remove_edge src dst (g, cache) in
      Hashtbl.replace cache (src, dst) new_locs;
      (G.add_edge_e g edge, cache)

  let remove_absloc : vertex -> A.t -> vertex -> t -> t
  =fun src x dst (g, cache) ->
    let old_locs = get_abslocs src dst (g, cache) in
    let new_locs = BatSet.remove x old_locs in
    let edge = G.E.create src new_locs dst in
    let (g, _) = remove_edge src dst (g, cache) in
    Hashtbl.replace cache (src, dst) new_locs;
    (G.add_edge_e g edge, cache)

  let remove_abslocs : vertex -> A.t BatSet.t -> vertex -> t -> t
  =fun src xs dst g -> BatSet.fold (fun x -> remove_absloc src x dst) xs g

  let in_degree (g, _) = G.in_degree g
  let fold_node f (g, _) = G.fold_vertex f g
  let fold_edges f (g, _) = G.fold_edges f g
  let iter_edges f (g, _) = G.iter_edges f g
  
  let sizeof g = 
    fold_edges (fun src dst size ->
      BatSet.cardinal (get_abslocs src dst g) + size
    ) g 0

  let succ_e : vertex -> t -> (vertex * A.t BatSet.t) list 
  =fun n g -> List.map (fun s -> (s, get_abslocs n s g)) (succ n g)

  let pred_e : vertex -> t -> (vertex * A.t BatSet.t) list 
  =fun n g -> List.map (fun p -> (p, get_abslocs p n g)) (pred n g)

  let to_string_in_dot : t -> string
  =fun g -> 
    "digraph dugraph {\n" ^
    fold_edges (fun src dst str ->
      let addrset = get_abslocs src dst g in
        str ^ "\"" ^ (N.to_string src) ^ "\"" ^ " -> " ^
              "\"" ^ (N.to_string dst) ^ "\"" ^
              "[label=\"{" ^
                BatSet.fold (fun addr s -> (A.to_string addr)^","^s) addrset "" ^
              "}\"]" ^ ";\n"
    ) g ""
    ^ "}"

  let to_json : t -> json
  = fun g ->
    let nodes = `List (fold_node (fun v nodes -> 
                  (`String (N.to_string v))::nodes) g []) 
    in
    let edges = `List (fold_edges (fun src dst edges ->
                  let addrset = get_abslocs src dst g in
                  (`List [`String (N.to_string src); `String (N.to_string dst); 
                          `String (BatSet.fold (fun addr s -> (A.to_string addr)^","^s) addrset "")])
                  ::edges) g [])
    in
    `Assoc [("nodes", nodes); ("edges", edges)]
end
