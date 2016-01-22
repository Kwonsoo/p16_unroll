open Types
open Feature

(* Produce single-query programs from the given new program. *)
let newprog_to_singleq_progs : dir -> unit
= fun file ->
	Sys.command ("mkdir ../new_singleq_temp");
	Sys.command ("./main.native " ^ file ^ " -insert_observe_imprecise -imprecise_type fs -dir ../new_singleq_temp"); ()

(* flang path set 하나와 실제 feature list를 받아서 하나의 fbvector를 만들어낸다. *)
let build_fbvector : Flang.t BatSet.t -> Flang.t list -> fbvector
= fun paths feature_list -> 
	let fbvector = List.fold_right (fun f accum ->
			(Feature.pred paths f) :: accum
		) feature_list [] in
	fbvector

(*TODO*)
(* Select and return the locset's that are highly likely to increase precision. *)
let select : dir -> (fbvector * locset) BatSet.t -> locset 
= fun classifier_path candidates -> 
	">> Location sets are selected."; BatSet.empty

