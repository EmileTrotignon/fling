(***********************************/**_****|*****************************************)
(* <position.ml                    /\ /  /\ | /\ ####################################*)
(*                                 \_ \_ \/ | \_ ####################################*)
(* <created by Emile Trotignon>                                                      *)
(*                                        _     _  |                                 *)
(* <date : 10/10/2109>             |\ /\ |  |\\ _\ | /\ #############################*)
(*                                 || \/ |  ||| \| | \_ #############################*)
(*                                                                                   *)
(*                                  _        /  _ o        _                         *)
(*                                 /_ || |\ /\ |  | /\ || |  /\ #####################*)
(*                                 _/ \| |/ \_ |  | \_ \| |  \_ #####################*)
(*                                       |                                           *)
(*                                    _   _ o  _       _ _   _ | _                   *)
(*                                 |\ _\ |  | /_ #### /_ _\ /  | _\\  / #############*)
(*                                 |/ \| |  | _/ #### _/ \| \_ | \| \/  #############*)
(**********************************|********************************/*****************)

type t = int * int

let of_int x y = (x, y)

let proj_x = fst

let proj_y = snd

let eq x y = x = y

let move (x, y) (x', y') = (x + x', y + y')

let string_of_position (x, y) =
  Printf.sprintf "(%s,%s)" (string_of_int x) (string_of_int y)

let is_inside_boundaries max_x max_y (x, y) =
  x >= 0 && y >= 0 && x < max_x && y < max_y
