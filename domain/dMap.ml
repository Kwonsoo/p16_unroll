(** Lattice constructor: map.  *)

open Lat
open Vocab

module type DMAP =
sig
  include LAT
  module A : SET
  module B : LAT

  val empty : t
  val is_empty : t -> bool
  val find : A.t -> t -> B.t
  val add : A.t -> B.t -> t -> t
  val weak_add : A.t -> B.t -> t -> t
  val remove : A.t -> t -> t
  val map : (B.t -> B.t) -> t -> t
  val mapi : (A.t -> B.t -> B.t) -> t -> t
  val fold : (B.t -> 'a -> 'a) -> t -> 'a -> 'a
  val foldi : (A.t -> B.t -> 'a -> 'a) -> t -> 'a -> 'a
  val mem : A.t -> t -> bool
  val filter : (A.t -> B.t -> bool) -> t -> t
  (** {6 Functions for an efficient fixpoint iteration} *)

  (** Calculate unstable memory entries and values. *)
  val unstables : t -> t -> (B.t -> B.t -> bool) -> A.t BatSet.t
    -> (A.t * B.t * B.t) list
  val join_pairs : (A.t * B.t) list -> t -> t
  val widen_pairs : (A.t * B.t) list -> t -> t
  val meet_big_small : t -> t -> t
end

module DMap (A:SET) (B:LAT) =
struct
  module A = A
  module B = B
  type t = (A.t, B.t) BatMap.t

  let name : string = "(" ^ A.name ^ "->" ^ B.name ^ ")"

  let to_string : t -> string = fun x ->
    if BatMap.is_empty x then "bot" else
      string_of_map A.to_string B.to_string x


  let b_table = Hashtbl.create 251
  let b_hashcons x =
    try Hashtbl.find b_table x with
      Not_found -> Hashtbl.add b_table x x; x

  let le : t -> t -> bool = fun x y ->
	if x == y then true else
      let enum1 = BatMap.enum x in
      let enum2 = BatMap.enum y in
      let rec loop = fun e1 e2 ->
	    match e1, e2 with
	    | Some (k1, v1), Some (k2, v2) ->
	      let c = compare k1 k2 in
	      if c = 0 then
		    if B.le v1 v2 then
              loop (BatEnum.get enum1) (BatEnum.get enum2)
            else false
	      else if c < 0 then
            if B.le v1 B.bot then loop (BatEnum.get enum1) e2 else false
	      else
            loop e1 (BatEnum.get enum2)
	    | Some (k1, v1), None ->
          if B.le v1 B.bot then loop (BatEnum.get enum1) e2 else false
	    | None, Some (k2, v2) -> true
	    | None, None -> true
      in
      loop (BatEnum.get enum1) (BatEnum.get enum2)


  let eq : t -> t -> bool = fun x y ->
	if x == y then true	else
      let enum1 = BatMap.enum x in
      let enum2 = BatMap.enum y in
      let rec loop = fun e1 e2 ->
	    match e1, e2 with
	    | Some (k1, v1), Some (k2, v2) ->
	      let c = compare k1 k2 in
	      if c = 0 then
		    if B.eq v1 v2 then
              loop (BatEnum.get enum1) (BatEnum.get enum2)
            else false
	      else if c < 0 then
            if B.eq v1 B.bot then loop (BatEnum.get enum1) e2 else false
	      else
          if B.eq v2 B.bot then loop e1 (BatEnum.get enum2) else false
	    | Some (k1, v1), None ->
          if B.eq v1 B.bot then loop (BatEnum.get enum1) e2 else false
	    | None, Some (k2, v2) ->
          if B.eq v2 B.bot then loop e1 (BatEnum.get enum2) else false
	    | None, None -> true
      in
      loop (BatEnum.get enum1) (BatEnum.get enum2)

  let bot : t = BatMap.empty

  let filter = BatMap.filter
  let join : t -> t -> t = fun x y ->
    if le x y then y else
    if le y x then x else
      let join' k opt_v1 opt_v2 =
        match opt_v1, opt_v2 with
        | None, None -> None
        | None, Some v
        | Some v, None -> if B.eq v B.bot then None else Some v
        | Some v1, Some v2 ->
          let joined_v = b_hashcons (B.join v1 v2) in
          if B.eq joined_v B.bot then None else Some joined_v in
      BatMap.merge join' x y

  let meet : t -> t -> t = fun x y ->
    if le x y then x else
    if le y x then y else
      let meet' k opt_v1 opt_v2 =
        match opt_v1, opt_v2 with
        | None, _
        | _, None -> None
        | Some v1, Some v2 ->
          let meeted_v = b_hashcons (B.meet v1 v2) in
          if B.eq meeted_v B.bot then None else Some meeted_v in
      BatMap.merge meet' x y

  let widen : t -> t -> t = fun x y ->
    if x = y then x else
      let widen' k opt_v1 opt_v2 =
        match opt_v1, opt_v2 with
        | None, None -> None
        | None, Some v
        | Some v, None -> if B.eq v B.bot then None else Some v
        | Some v1, Some v2 ->
          let widened_v = b_hashcons (B.widen v1 v2) in
          if B.eq widened_v B.bot then None else Some widened_v in
      BatMap.merge widen' x y

  let narrow : t -> t -> t = fun x y ->
    if x = y then x else
      let narrow' k opt_v1 opt_v2 =
        match opt_v1, opt_v2 with
        | _, None -> None
        | None, Some v ->
          if B.eq v B.bot then None else invalid_arg "dMap.ml:narrow"
        | Some v1, Some v2 ->
          let narrowed_v = b_hashcons (B.narrow v1 v2) in
          if B.eq narrowed_v B.bot then None else Some narrowed_v in
      BatMap.merge narrow' x y


  (** {6 Functions for map} *)


  let empty : t = BatMap.empty

  let is_empty : t -> bool = BatMap.is_empty

  let find : A.t -> t -> B.t = fun k a -> find_def k a B.bot

  let add : A.t -> B.t -> t -> t = fun k v x ->
    if B.eq v B.bot then BatMap.remove k x else BatMap.add k (b_hashcons v) x

  let weak_add : A.t -> B.t -> t -> t = fun k v x ->
    if B.eq v B.bot then x else
      let orig_v = find k x in
      if B.le v orig_v then x else
      if B.le orig_v v then BatMap.add k (b_hashcons v) x else
        BatMap.add k (b_hashcons (B.join orig_v v)) x

  let widen_add : A.t -> B.t -> t -> t = fun k v x ->
    if B.eq v B.bot then x else
      let orig_v = find k x in
      if B.le v orig_v then x else
        BatMap.add k (b_hashcons (B.widen orig_v v)) x

  let remove : A.t -> t -> t = BatMap.remove


  (* Type of map is restricted to return the same type.  *)
  (** The first argument of map,  function f,  should preserve the
      bottom value for consistency,  e.g. [f B.bot = B.bot].  Since
      [\[x |-> B.bot\]] and [\[ \]] are the same map,  it is natural
      that [map f \[x |-> B.bot\]] and [map f \[ \]] return the same
      value.  *)
  let map : (B.t -> B.t) -> t -> t = fun f m ->
    BatMap.map (fun v -> b_hashcons (f v)) m

  let mapi : (A.t -> B.t -> B.t) -> t -> t = fun f m ->
    BatMap.mapi (fun k v -> b_hashcons (f k v)) m


  (** The first argument of map,  function f,  should preserve the
      bottom value for consistency,  e.g. [f B.bot acc = acc].  Since
      [\[x |-> B.bot\]] and [\[ \]] are the same map,  it is natural
      that [fold f \[x |-> B.bot\] acc] and [fold f \[ \] acc] return
      the same value.  *)
  let fold : (B.t -> 'a -> 'a) -> t -> 'a -> 'a = BatMap.fold

  let foldi : (A.t -> B.t -> 'a -> 'a) -> t -> 'a -> 'a = BatMap.foldi

(*  let to_json : t -> Yojson.Safe.json = fun dmap ->
    Yojson.Safe.(
      `Assoc (
        foldi
        (fun a b json ->
          (A.to_string a, B.to_json b)::json
        ) dmap []
      )
    )
*)
  (** {6 Functions for an efficient fixpoint iteration} *)

  (** Calculate unstable memory entries and values.

      Returns a list of triples, (key, old value, new value).
  *)
  let unstables : t -> t -> (B.t -> B.t -> bool) -> A.t BatSet.t
    -> (A.t * B.t * B.t) list
  = fun old_m new_m is_unstb candidate ->
    let e_old = BatMap.enum old_m in
    let e_new = BatMap.enum new_m in
    let e_candi = BatSet.enum candidate in

    let rec add_unstb k old_v new_v candi_opt unstb =
      match candi_opt with
      | None -> (candi_opt, unstb)
      | Some candi ->
        let cmp = compare k candi in
        if cmp = 0 then
          if is_unstb old_v new_v then
            (BatEnum.get e_candi, (k, old_v, new_v) :: unstb)
          else (BatEnum.get e_candi, unstb)
        else if cmp < 0 then (candi_opt, unstb) else
          add_unstb k old_v new_v (BatEnum.get e_candi) unstb in

    (* loop (old memory) (new memory) candiate (unstable keys) *)
    let rec loop kv1 kv2 candi unstb =
      match kv1, kv2, candi with
      | _, _, None
      | None, None, _ -> unstb
      | Some (k1, v1), None, _ ->
        let (candi, unstb) = add_unstb k1 v1 B.bot candi unstb in
        loop (BatEnum.get e_old) kv2 candi unstb
      | None, Some (k2, v2), _ ->
        let (candi, unstb) = add_unstb k2 B.bot v2 candi unstb in
        loop kv1 (BatEnum.get e_new) candi unstb
      | Some (k1, v1), Some (k2, v2), _ ->
        let cmp = compare k1 k2 in
        if cmp < 0 then
          let (candi, unstb) = add_unstb k1 v1 B.bot candi unstb in
          loop (BatEnum.get e_old) kv2 candi unstb
        else if cmp > 0 then
          let (candi, unstb) = add_unstb k2 B.bot v2 candi unstb in
          loop kv1 (BatEnum.get e_new) candi unstb
        else
          let (candi, unstb) = add_unstb k2 v1 v2 candi unstb in
          loop (BatEnum.get e_old) (BatEnum.get e_new) candi unstb in

    let init_old = BatEnum.get e_old in
    let init_new = BatEnum.get e_new in
    let init_candi = BatEnum.get e_candi in
    loop init_old init_new init_candi []

  let add_pairs : (A.t -> B.t -> t -> t) -> (A.t * B.t) list -> t -> t
  = fun add_f pairs m ->
    let add_pair (k, v) m = if B.eq v B.bot then m else add_f k v m in
    list_fold add_pair pairs m

  let join_pairs : (A.t * B.t) list -> t -> t = add_pairs weak_add

  let widen_pairs : (A.t * B.t) list -> t -> t = add_pairs widen_add

  let keys m = foldi (fun k _ -> BatSet.add k) m BatSet.empty

  let mem = BatMap.mem

  let le_small_big : t -> t -> bool = fun small_x big_y ->
    let le_a_map k v acc = acc && B.le v (find k big_y) in
    foldi le_a_map small_x true

  let meet_big_small : t -> t -> t = fun big_x small_y ->
    let meet_a_map k v = b_hashcons (B.meet v (find k big_x)) in
    if le_small_big small_y big_x then small_y else
      mapi meet_a_map small_y

end
