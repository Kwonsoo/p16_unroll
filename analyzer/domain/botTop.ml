(* bot-top lattice *)
type t = Bot | Top

let name = "BotTop"
let bot = Bot
let top = Top

let le x y = 
  match x, y with
  | Top, Bot -> false
  | _, _ -> true

let eq = (=) 

let join x y = 
  match x, y with
  | Bot, _ -> y
  | _, Bot -> x
  | _, _ -> Top

let meet x y =
  match x, y with
  | Bot, _ 
  | _, Bot -> Bot
  | _ -> Top

let widen  = join
let narrow = meet

let to_string x = match x with Bot -> "Bot" | Top -> "Top"
