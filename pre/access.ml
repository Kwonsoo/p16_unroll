open Vocab
open Lat
open Pow
open AbsDom

module type ACCESS = 
sig
  type t
  module Loc : SET
  module PowLoc : SET
  type mode
  val empty : t
  val def : mode
  val use : mode
  val all : mode
  val add : mode -> Loc.t -> t -> t
  val singleton : mode -> Loc.t -> t
  val mem : Loc.t -> t -> bool
  val remove : Loc.t -> t -> t
  val remove_set : Loc.t BatSet.t -> t -> t
  val add_set : mode -> Loc.t BatSet.t -> t -> t
  val from_set : mode -> Loc.t BatSet.t -> t
  val add_list : mode -> Loc.t list -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> Loc.t BatSet.t
  val restrict : Loc.t BatSet.t -> t -> t
  val filter_out : Loc.t BatSet.t -> t -> t
  val accessof : t -> Loc.t BatSet.t
  val useof : t -> Loc.t BatSet.t
  val defof : t -> Loc.t BatSet.t
  val cardinal : t -> int
  val to_string_use : t -> string
  val to_string_def : t -> string
  val to_string : t -> string
  val print : t -> unit
  val print_use : t -> unit
  val print_def : t -> unit
end

module Make(Loc: SET) = 
struct
  module PowLoc = Pow(Loc)
  module Loc = Loc
  type mode = DEF | USE | ALL

  type t = {
    def    : Loc.t BatSet.t;
    use    : Loc.t BatSet.t
  }
  let def = DEF
  let use = USE
  let all = ALL
  let empty = {
    def    = BatSet.empty;
    use    = BatSet.empty
  }

  let add : mode -> Loc.t -> t -> t
  = fun m a t ->
    match m with
    | USE -> { t with use = BatSet.add a t.use }
    | DEF -> { t with def = BatSet.add a t.def }
    | ALL -> { use = BatSet.add a t.use;
               def = BatSet.add a t.def }

  let singleton : mode -> Loc.t -> t
  = fun m a -> add m a empty
   
  let mem : Loc.t -> t -> bool
  = fun l a ->
    (BatSet.mem l a.def) || (BatSet.mem l a.use)

  let remove : Loc.t -> t -> t
  = fun a t ->
    {
      use = BatSet.remove a t.use;
      def = BatSet.remove a t.def;
    } 

  let remove_set : Loc.t BatSet.t -> t -> t
  = fun addrs t ->
    { 
      use = BatSet.diff t.use addrs;
      def = BatSet.diff t.def addrs;
    }

  let add_set : mode -> Loc.t BatSet.t -> t -> t
  = fun m aset t ->
    match m with
    | USE -> { t with use = BatSet.union t.use aset }
    | DEF -> { t with def = BatSet.union t.def aset }
    | ALL -> { use = BatSet.union t.use aset ;
               def = BatSet.union t.def aset }

  let from_set : mode -> Loc.t BatSet.t -> t
  = fun m aset -> add_set m aset empty
  
  let add_list : mode -> Loc.t list -> t -> t
  = fun m alist t -> list_fold (add m) alist t

  let accessof : t -> Loc.t BatSet.t
  = fun l -> BatSet.union l.def l.use

  let useof : t -> Loc.t BatSet.t
  = fun l -> l.use

  let defof : t -> Loc.t BatSet.t
  = fun l -> l.def

  let union : t -> t -> t
  = fun l1 l2 -> 
    {
      use = BatSet.union l1.use l2.use;
      def = BatSet.union l1.def l2.def
    }

  let diff : t -> t -> Loc.t BatSet.t
  = fun l1 l2 -> 
    BatSet.diff (accessof l1) (accessof l2)

  let restrict : Loc.t BatSet.t -> t -> t
  = fun addrs l -> 
    {
      use = BatSet.intersect l.use addrs;
      def = BatSet.intersect l.def addrs
    }

  let filter_out : Loc.t BatSet.t -> t -> t
  = fun addrs l -> 
    {
      use = BatSet.diff l.use addrs;
      def = BatSet.diff l.def addrs
    }

  let cardinal : t -> int
  = fun l -> BatSet.cardinal (accessof l)

  let to_string_use : t -> string
  = fun l -> "Use = " ^ string_of_set Loc.to_string l.use

  let to_string_def : t -> string
  = fun l -> "Def = " ^ string_of_set Loc.to_string l.def

  let to_string : t -> string = fun l -> to_string_use l ^ "\n" ^ to_string_def l

  let print : t -> unit
  = fun l -> prerr_string (to_string l)

  let print_use : t -> unit
  = fun l -> prerr_endline (to_string_use l)

  let print_def : t -> unit
  = fun l -> prerr_endline (to_string_def l)
end
