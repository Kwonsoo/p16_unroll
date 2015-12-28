type t = int list (* placeholder *)

let gen : Global.t -> t BatSet.t
=fun global -> BatSet.empty (* TODO *)

let pred : Global.t -> t -> bool
=fun global feature -> true (* TODO *)
