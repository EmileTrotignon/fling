(***********************************/**_****|*****************************************)
(* <rules.ml>                      /\ /  /\ | /\ ####################################*)
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

type direction = Up | Right | Down | Left

module Ball = struct
  type t = Position.t * int

  let compare (p, _) (p', _) = compare p p'
end

let max_x = 15

let max_y = 15

let is_inside_bounds pos = Position.is_inside_boundaries max_x max_y pos

let ball_inside_bounds (pos, _) = is_inside_bounds pos

module GameS = Set.Make (Ball)

type game = GameS.t

type ball = Ball.t

type move = Position.t * direction

module Move = struct
  type t = move

  let compare = compare
end

module MoveSet = Set.Make (Move)

let is_ball g p = GameS.mem (p, 0) g

let ball_of_position game p = GameS.find (p, 0) game

let force_add ele s = GameS.add ele (GameS.remove ele s)

(* Two functions sharing the same private reference
   Nice ! *)
let make_ball, new_id =
  let next_id = ref (Random.bits ()) in
  let make_ball next_id p =
    next_id := !next_id + 1 ;
    (p, !next_id - 1)
  in
  let new_id next_id (pos, _) =
    next_id := !next_id + 1 ;
    (pos, !next_id - 1)
  in
  make_ball (next_id), new_id (next_id)

let new_game = GameS.of_list

let eq_ball (_, id) (_, id') = id = id'

let directional_incr pos dir =
  match dir with
  | Up -> Position.move pos (Position.of_int 0 1)
  | Right -> Position.move pos (Position.of_int 1 0)
  | Down -> Position.move pos (Position.of_int 0 (-1))
  | Left -> Position.move pos (Position.of_int (-1) 0)

let directional_deincr pos dir =
  match dir with
  | Up -> directional_incr pos Down
  | Right -> directional_incr pos Left
  | Down -> directional_incr pos Up
  | Left -> directional_incr pos Right

let ball_dir_incr dir (pos, id) = (directional_incr pos dir, id)

let ball_dir_deincr dir (pos, id) = (directional_deincr pos dir, id)

let make_move (p, _) d = (p, d)

let rec find_ball pos g dir =
  let pos' = directional_incr pos dir in
  if not (Position.is_inside_boundaries max_x max_y pos') then None
  else if is_ball g pos' then Some pos'
  else find_ball pos' g dir

let is_move_valid g (pos, dir) =
  if GameS.mem (directional_incr pos dir, 0) g then false
  else match find_ball pos g dir with None -> false | Some _ -> true

let apply_move game (position, dir) =
  if not (is_move_valid game (position, dir)) then failwith "Impossible move"
  else
    let rec aux g pos =
      let id = match ball_of_position g pos with _, id' -> id' in
      let g' = GameS.remove (pos, id) g in
      match find_ball pos g dir with
      | None -> g'
      | Some pos' ->
          let new_pos = directional_deincr pos' dir in
          aux (force_add (new_pos, id) g') pos'
    in
    aux game position

let rec existing_moves g =
  if GameS.equal g GameS.empty then MoveSet.empty
  else
    let pos, id = GameS.choose g in
    List.fold_right MoveSet.union
      [ MoveSet.singleton (pos, Down)
      ; MoveSet.singleton (pos, Left)
      ; MoveSet.singleton (pos, Up)
      ; MoveSet.singleton (pos, Right) ]
      (existing_moves (GameS.remove (pos, id) g))

let moveset g = MoveSet.filter (is_move_valid g) (existing_moves g)

let moves g = MoveSet.elements (moveset g)

let get_balls = GameS.elements

let position_of_ball (p, _) = p

let is_win game = GameS.cardinal game = 1

let get_id (_, id) = id


