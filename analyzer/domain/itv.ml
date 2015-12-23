(** Concrete lattice: interval. *)


(** Widening threshold. *)

let rec gen_lst s e l = if s = e then l else s::(gen_lst (s+1) e l)
let threshold = ref (BatSet.empty)

let set_threshold t = threshold := t

(** {6 Auxiliary type [t'] (integer + [+oo] + [-oo])} *)

type t' = Int of int | PInf | MInf


let to_string' : t' -> string = function
  | Int i -> string_of_int i
  | PInf -> "+oo"
  | MInf -> "-oo"

let le' : t' -> t' -> bool = fun x y ->
  match x, y with
  | MInf, _ -> true
  | _, PInf -> true
  | Int i, Int j -> i <= j
  | _, _ -> false


let eq' : t' -> t' -> bool = fun x y ->
  match x, y with
  | MInf, MInf
  | PInf, PInf -> true
  | Int i, Int j -> i = j
  | _, _ -> false

let min' : t' -> t' -> t' = fun x y -> if le' x y then x else y
let max' : t' -> t' -> t' = fun x y -> if le' x y then y else x


let lower_widen' : t' -> t' -> t' = fun x y ->
  if le' y x then
    if eq' y x then y else
      let filtered = BatSet.filter (fun k -> le' (Int k) y) !threshold in
      if BatSet.is_empty filtered 
      then MInf else Int (BatSet.max_elt filtered)
  else x
    


let upper_widen' : t' -> t' -> t' = fun x y ->
  if le' x y then
    if eq' x y then y else
      let filtered = BatSet.filter (fun k -> le' y (Int k)) !threshold in
      if BatSet.is_empty filtered
      then PInf else Int (BatSet.min_elt filtered)
  else x



let lower_narrow' : t' -> t' -> t' = fun x y ->
  if le' x y then y 
  else invalid_arg 
  ("itv.ml: lower_narrow' (x, y).  y < x : "^(to_string' y)^" < "^(to_string' x))


let upper_narrow' : t' -> t' -> t' = fun x y ->
  if le' y x then y else invalid_arg "itv.ml: upper_narrow' (x, y).  x < y"


let plus' (x:t') (y:t') : t' =
  match x, y with
  | Int n1, Int n2 -> Int (n1 + n2)
  | PInf, MInf -> invalid_arg "itv.ml: plus'"
  | MInf, PInf -> invalid_arg "itv.ml: plus'"
  | PInf, _ -> PInf
  | MInf, _ -> MInf
  | _, PInf -> PInf
  | _, MInf -> MInf


let minus' (x:t') (y:t') : t' =
  match x, y with
  | Int n1, Int n2 -> Int (n1 - n2)
  | PInf, PInf -> invalid_arg "itv.ml: minus'"
  | MInf, MInf -> invalid_arg "itv.ml: minus'"
  | PInf, _ -> PInf
  | MInf, _ -> MInf
  | _, PInf -> MInf
  | _, MInf -> PInf


let times' (x:t') (y:t') : t' =
  match x, y with
  | Int n1, Int n2 -> Int (n1 * n2)
  | PInf, PInf
  | MInf, MInf -> PInf
  | PInf, MInf
  | MInf, PInf -> MInf
  | PInf, Int n
  | Int n, PInf ->
    if n < 0 then MInf else
    if n > 0 then PInf else
      Int 0
  | (MInf, Int n)
  | (Int n, MInf) ->
    if n < 0 then PInf else
    if n > 0 then MInf else
      Int 0


let divide' (x:t') (y:t') : t' =
  match x, y with
  | Int n1, Int n2 ->
    if n2 = 0 then invalid_arg "itv.ml: divide' (_, 0)" else Int (n1 / n2)
  | PInf, PInf
  | MInf, MInf -> PInf
  | MInf, PInf
  | PInf, MInf -> MInf
  | MInf, Int n ->
    if n < 0 then PInf else
    if n > 0 then MInf else
      invalid_arg "itv.ml: divide' (-oo, 0)"
  | PInf, Int n ->
    if n < 0 then MInf else
    if n > 0 then PInf else
      invalid_arg "itv.ml: divide' (+oo, 0)"
  | Int _,  PInf
  | Int _,  MInf -> Int 0


let min4' : t' -> t' -> t' -> t' -> t' = fun x y z w ->
  min' (min' x y) (min' z w)


let max4' : t' -> t' -> t' -> t' -> t' = fun x y z w ->
  max' (max' x y) (max' z w)


(** {6 Main definitions of interval} *)

type t = V of t' * t' | Bot


let name : string = "interval"
let zero = V (Int 0, Int 0)
let one = V (Int 1, Int 1)
let zero_pos = V (Int 0, PInf)

let to_string : t -> string = function
  | Bot -> "bot"
  | V (l, u) -> "["^(to_string' l)^", "^(to_string' u)^"]"
let to_json : t -> Yojson.Safe.json = fun itv ->
  let this_to_string = to_string in
  Yojson.Safe.(
    `String (this_to_string itv)
  )


let is_bot : t -> bool = function
  | Bot -> true
  | V (l, u) -> l = PInf || u = MInf || not (le' l u)


(** Normalizes invalid intervals such as [\[u, l\]] with [u > l] to
    [Bot].*)
let normalize (x:t) : t = if is_bot x then Bot else x


let table = Hashtbl.create 251
let hashcons x =
  try Hashtbl.find table x with
    Not_found -> Hashtbl.add table x x; x


let le : t -> t -> bool = fun x y ->
  if is_bot x then true else
  if is_bot y then false else
    match x, y with
    | V (l1, u1), V (l2, u2) -> le' l2 l1 && le' u1 u2
    | _, _ -> assert false


let eq : t -> t -> bool = fun x y ->
  if is_bot x && is_bot y then true else
  if is_bot x || is_bot y then false else
    match x, y with
    | V (l1, u1), V (l2, u2) -> eq' l2 l1 && eq' u1 u2
    | _, _ -> assert false


let top : t = hashcons (V (MInf, PInf))
let bot : t = Bot


let join : t -> t -> t = fun x y ->
  if le x y then y else
  if le y x then x else
  if is_bot x then normalize y else
  if is_bot y then normalize x else
    match x, y with
    | V (l1, u1), V (l2, u2) -> hashcons (V (min' l1 l2, max' u1 u2))
    | _, _ -> assert false


let meet : t -> t -> t = fun x y ->
  if le x y then x else
  if le y x then y else
  if is_bot x then Bot else
  if is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      hashcons (normalize (V (max' l1 l2, min' u1 u2)))
    | _, _ -> assert false


let widen : t -> t -> t = fun x y ->
  if x = y then x else
  if is_bot x then normalize y else
  if is_bot y then normalize x else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      hashcons (V (lower_widen' l1 l2, upper_widen' u1 u2))
    | _, _ -> assert false


let narrow : t -> t -> t = fun x y ->
  if x = y then x else
  if is_bot y then Bot else
  if is_bot x then invalid_arg "itv.ml: narrow(bot, _)" else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      hashcons (V (lower_narrow' l1 l2, upper_narrow' u1 u2))
    | _, _ -> assert false


(** {6 Auxiliary functions for interval} *)

let open_right (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (_, PInf) -> true
    | _ -> false

let close_left (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (Int _, _) -> true
    | _ -> false


let open_left (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (MInf, _) -> true
    | _ -> false


let is_range (x:t) : bool = not (is_bot x)

let is_const (x:t) : bool =
  match x with
  | V (Int x, Int y) -> x = y
  | _ -> false

let is_finite (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (Int _, Int _) -> true
    | _ -> false

let is_negative (x:t) : bool =
  if is_bot x then false else
    match x with
    | V (MInf,  _) -> true
    | V (Int x, Int _) -> x < 0
    | _ -> false


let height (x:t) : int =
  let h_bound = 1000 in
  if is_bot x then 0 else
    match x with
    | V (Int l, Int u) -> if u - l + 1 > h_bound then h_bound else u - l + 1
    | _ -> h_bound


let diff (x:t) : int =
  if is_bot x then 0 else
    match x with
    | V (Int l, Int u) -> u - l
    | _ -> 0


(** {6 Binary/Unary operations for interval} *)

let plus (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) -> hashcons (V (plus' l1 l2, plus' u1 u2))
    | _, _ -> assert false

let minus (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) -> hashcons (V (minus' l1 u2, minus' u1 l2))
    | _, _ -> assert false

let times (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      let x1 = times' l1 l2 in
      let x2 = times' l1 u2 in
      let x3 = times' u1 l2 in
      let x4 = times' u1 u2 in
      hashcons (V (min4' x1 x2 x3 x4,  max4' x1 x2 x3 x4))
    | _, _ -> assert false

let divide (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
  if le (V (Int 0, Int 0)) y then top else
    match x, y with
    | V (l1, u1), V (l2, u2) ->
      let x1 = divide' l1 l2 in
      let x2 = divide' l1 u2 in
      let x3 = divide' u1 l2 in
      let x4 = divide' u1 u2 in
      hashcons (V (min4' x1 x2 x3 x4,  max4' x1 x2 x3 x4))
    | _, _ -> assert false

let false_itv : t = hashcons (V (Int 0, Int 0))
let true_itv : t = hashcons (V (Int 1, Int 1))
let unknown_bool_itv : t = hashcons (V (Int 0, Int 1))

let and_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then
    Bot
  else if eq false_itv x || eq false_itv y then
    false_itv
  else if not (le false_itv x) && not (le false_itv y) then
    true_itv
  else
    unknown_bool_itv


let or_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then
    Bot
  else if eq false_itv x && eq false_itv y then
    false_itv
  else if not (le false_itv x) || not (le false_itv y) then
    true_itv
  else
    unknown_bool_itv


let eq_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (Int l1, Int u1), V (Int l2, Int u2)
      when l1 = u1 && u1 = l2 && l2 = u2 -> true_itv
    | V (_, Int u1), V (Int l2, _) when u1 < l2 -> false_itv
    | V (Int l1, _), V (_, Int u2) when u2 < l1 -> false_itv
    | _, _ -> unknown_bool_itv


let ne_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (Int l1, Int u1), V (Int l2, Int u2)
      when l1 = u1 && u1 = l2 && l2 = u2 -> false_itv
    | V (_, Int u1), V (Int l2, _) when u1 < l2 -> true_itv
    | V (Int l1, _), V (_, Int u2) when u2 < l1 -> true_itv
    | _, _ -> unknown_bool_itv


let lt_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (_, Int u1), V (Int l2, _) when u1 < l2 -> true_itv
    | V (Int l1, _), V (_, Int u2) when u2 <= l1 -> false_itv
    | _, _ -> unknown_bool_itv


let le_itv (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else
    match x, y with
    | V (_, Int u1), V (Int l2, _) when u1 <= l2 -> true_itv
    | V (Int l1, _), V (_, Int u2) when u2 < l1 -> false_itv
    | _, _ -> unknown_bool_itv

let gt_itv (x:t) (y:t) : t = lt_itv y x
let ge_itv (x:t) (y:t) : t = le_itv y x

let not_itv (x:t) : t =
  if is_bot x then Bot else
  if eq false_itv x then true_itv else
  if le false_itv x then unknown_bool_itv else
    false_itv

let unknown_binary (x:t) (y:t) : t =
  if is_bot x || is_bot y then Bot else top

let unknown_unary (x:t) : t = if is_bot x then Bot else top

let l_shift_itv (x:t) (y:t) : t = unknown_binary x y
let r_shift_itv (x:t) (y:t) : t = unknown_binary x y
let b_xor_itv (x:t) (y:t) : t = unknown_binary x y
let b_or_itv (x:t) (y:t) : t = unknown_binary x y
let b_and_itv (x:t) (y:t) : t = unknown_binary x y
let mod_itv (x:t) (y:t) : t = unknown_binary x y
let b_not_itv (x:t) : t = unknown_unary x

let prune : Cil.binop -> t -> t -> t = fun op x y ->
  if is_bot x || is_bot y then Bot else
    let pruned =
      match op, x, y with
      | Cil.Lt, V (a, b), V (c, d) -> V (a, min' b (minus' d (Int 1)))
      | Cil.Gt, V (a, b), V (c, d) -> V (max' a (plus' c (Int 1)), b)
      | Cil.Le, V (a, b), V (c, d) -> V (a, min' b d)
      | Cil.Ge, V (a, b), V (c, d) -> V (max' a c, b)
      | Cil.Eq, V (a, b), V (c, d) -> meet x y
      | Cil.Ne, V (a, b), V (c, d) when eq' b c && eq' c d ->
        V (a, minus' b (Int 1))
      | Cil.Ne, V (a, b), V (c, d) when eq' a c && eq' c d ->
        V (plus' a (Int 1), b)
      | Cil.Ne, V _, V _ -> x
      | _ -> invalid_arg "itv.ml:prune" in
    hashcons (normalize pruned)
