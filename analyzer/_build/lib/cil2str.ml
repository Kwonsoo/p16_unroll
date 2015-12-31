open Cil
open Vocab

let tostring s = Escape.escape_string (Pretty.sprint 0 s)

let rec s_exps : exp list -> string = fun es ->
  string_of_list ~first:"(" ~last:")" ~sep:", " s_exp es

and s_exp : exp -> string = function
  | Const c -> s_const c
  | Lval l -> s_lv l
  | SizeOf t -> "SizeOf(" ^ s_type t ^ ")"
  | SizeOfE e -> "SizeOfE(" ^ s_exp e ^ ")"
  | SizeOfStr s -> "SizeOfStr(" ^ s ^ ")"
  | AlignOf t -> "AlignOf(" ^ s_type t ^ ")"
  | AlignOfE e -> "AlignOfE(" ^ s_exp e ^ ")"
  | UnOp (u, e, _) -> s_uop u ^ s_exp_paren e
  | BinOp (b, e1, e2, _) -> s_exp_paren e1 ^ s_bop b ^ s_exp_paren e2
  | Question (c, e1, e2, _) ->
    s_exp_paren c ^ " ? " ^ s_exp_paren e1 ^ " : " ^ s_exp_paren e2
  | CastE (t, e) -> "(" ^ s_type t ^ ")" ^ s_exp_paren e
  | AddrOf l -> "&" ^ s_lv l
  | AddrOfLabel _ -> invalid_arg "AddrOfLabel is not supported."
  | StartOf l -> "StartOf(" ^ s_lv l ^ ")"

and s_exp_paren : exp -> string
= fun e ->
  match e with
  | UnOp _ | BinOp _ | Question _ | CastE _ -> "(" ^ s_exp e ^ ")"
  | _ -> s_exp e

and s_const : constant -> string
=fun c -> tostring (d_const () c)

and s_type : typ -> string
=fun typ -> tostring (d_type () typ)

and s_stmt : stmt -> string
= fun s -> tostring (d_stmt () s)

and s_lv : lval -> string = fun (lh, offset) ->
  s_lhost lh ^ s_offset offset

and s_lhost : lhost -> string = function
  | Var vi -> (if vi.vglob then "@" else "") ^ vi.vname
  | Mem e -> "*" ^ s_exp_paren2 e

and s_exp_paren2 : exp -> string
= fun e ->
  match e with
  | Lval (_, NoOffset) -> s_exp e
  | Lval _ | UnOp _ | BinOp _ | Question _ | CastE _ -> "(" ^ s_exp e ^ ")"
  | _ -> s_exp e

and s_offset : offset -> string = function
  | NoOffset -> ""
  | Field (fi, offset) -> "." ^ fi.fname ^ s_offset offset
  | Index (e, offset) -> "[" ^ s_exp e ^ "]" ^ s_offset offset

and s_uop u = tostring (d_unop () u)

and s_bop b = tostring (d_binop () b)

and s_instr : instr -> string
=fun i -> 
  match i with
  | Set (lv,exp,_) -> "Set(" ^ s_lv lv ^ "," ^ s_exp exp ^ ")"
  | Call (Some lv,fexp,params,_) -> 
      s_lv lv ^ ":= Call(" ^ s_exp fexp ^ s_exps params ^ ")"
  | Call (None,fexp,params,_) -> 
      "Call(" ^ s_exp fexp ^ s_exps params ^ ")"
  | Asm _ -> "Asm"

and s_instrs : instr list -> string
=fun instrs ->
  List.fold_left (fun s i -> s ^ s_instr i) "" instrs

let s_location : location -> string
=fun loc ->
  let file = try 
    let idx = String.rindex loc.file '/' in
    let len = String.length loc.file in
      String.sub loc.file (idx+1) (len-idx-1) 
    with _ -> loc.file
  in file ^ ":" ^ string_of_int loc.line
