let make_istr x = 
        "\n\n\n"^
        "--------------------------------------------------------------------------------\n"^
        x^
        "\n--------------------------------------------------------------------------------\n"

(** 하나의 단계를 구분하기 위한 함수.
		함수가 하는 일의 이름과 함수 몸체, 함수의 인자를 주면 함수에 그 인자를 넣고 실행하는 하나의 단계를 구분한다.
		구분된 하나의 단계에 대해서 시작과 종료를 알리는 메시지를 화면에 출력하고, 수행하는동안 걸린 시간을 출력한다.
		단계는 '큰'단계와 '작은'단계로 구분되는데, '큰'단계의 경우 메시지를 앞 단계과 구분해서 보기 좋게 출력한다.  
		@param s 큰 단계인 경우 true
		@param stg 단계의 이름
		@param i 함수의 인자
		@param fn 한 단계를 이루는 함수
*)
let step : bool -> string -> 'a -> ('a -> 'b) -> 'b
 = fun s stg i fn ->
    let t0 = Sys.time() in
    let _ = if s then prerr_endline (make_istr (stg^" begins...")) in
    let _ = if not s then prerr_endline (stg^" begins...") in
    let v = fn i in
    let _ = if s then prerr_endline "" in
    let _ = prerr_endline (stg^" completes: "^(string_of_float (Sys.time()-.t0))) in
        v

(** 하나의 단계를 구분하기 위한 함수. step과 동일하고 인자의 순서만 다르다.
		함수가 하는 일의 이름과 함수 몸체, 함수의 인자를 주면 함수에 그 인자를 넣고 실행하는 하나의 단계를 구분한다.
		구분된 하나의 단계에 대해서 시작과 종료를 알리는 메시지를 화면에 출력하고, 수행하는동안 걸린 시간을 출력한다.
		단계는 '큰'단계와 '작은'단계로 구분되는데, '큰'단계의 경우 메시지를 앞 단계과 구분해서 보기 좋게 출력한다.  
		@param s 큰 단계인 경우 true
		@param stg 단계의 이름
		@param fn 한 단계를 이루는 함수
		@param i 함수의 인자
*)
let stepf : bool -> string -> ('a -> 'b) -> 'a -> 'b
 = fun s stg fn i ->
     step s stg i fn

(** 첫 번째 주어진 인자가 true인 경우에만 함수를 실행한다.
		true인 경우, stepf와 동일하다.
		@param b 실행 여부
		@param s 큰 단계인 경우 true
		@param stg 단계의 이름
		@param fn 한 단계를 이루는 함수
		@param i 함수의 인자
*)
let stepf_opt : bool -> bool -> string -> ('a -> 'a) -> 'a -> 'a
 = fun b s stg fn i ->
	if b then step s stg i fn
	else i

let stepf_opt_unit : bool -> bool -> string -> ('a -> unit) -> 'a -> unit
= fun b s stg fn i -> if b then step s stg i fn else ()

let stepf_s silent s stg fn i = 
  if silent then fn i
  else stepf s stg fn i

let stepf_opt_s silent : bool -> bool -> string -> ('a -> 'a) -> 'a -> 'a
 = fun b s stg fn i ->
  if b && silent then fn i
  else if b then step s stg i fn 
  else i
