(***********************************/*******|*****************************************)
(* <valid.ml>                      /\ /  /\ | /\ ####################################*)
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

open Rules
open Utils
(* Set of games *)
module GameSS = Set.Make (GameS)


(* I hadnt checked that Set.S provides compare, and it does, 
   so I did this even if it's not really needed or anything, 
   because it's still a fun piece of code
module GameS' = struct
  type t = GameS.t
  let rec compare g g' =
    if (GameS.equal g g') then 0
    else
      (if  (GameS.cardinal g) = (GameS.cardinal g') then
         let (p, _) = GameS.choose g in
         let (p', _) = GameS.choose g' in
         if p = p' then
           compare (GameS.remove (p, 0) g) (GameS.remove (p', 0) g')
         else
           Pervasives.compare p p'
       else
         Pervasives.compare (GameS.cardinal g) (GameS.cardinal g'))
end
*)

(* [moved_balls game b dir] returns the set of ball 
   that would be moved by playing ([b], [dir]) (except [b] itself *)
let moved_balls game b dir =
  let pos = position_of_ball b in
  let cond b' =
    let pos' = position_of_ball b' in
    match dir with
    | Up ->
        Position.proj_x pos = Position.proj_x pos'
        && Position.proj_y pos < Position.proj_y pos'
    | Right ->
        Position.proj_y pos = Position.proj_y pos'
        && Position.proj_x pos' < Position.proj_x pos
    | Down ->
        Position.proj_x pos = Position.proj_x pos'
        && Position.proj_y pos' < Position.proj_y pos
    | Left ->
        Position.proj_y pos = Position.proj_y pos'
        && Position.proj_x pos < Position.proj_x pos'
  in
  GameS.filter cond game

(* [possible_previous_positions game dir ball] returns all the position of ball [ball] 
   such that you can you can get [game] 
   by applying move (position, [dir]), where GameS.mem (position, 0) return_value *)
let possible_previous_positions game dir ball =
  let rec aux ball =
    let pred_ball = ball_dir_deincr dir ball in
    if (not (ball_inside_bounds pred_ball)) || GameS.mem pred_ball game then
      GameS.empty
    else
      GameS.add pred_ball (aux pred_ball)
  in
  aux (ball_dir_deincr dir ball)

(* [make_game_from_possible_previous_position game dir prev_pos]
returns the game game' such that apply_move game' ([prev_pos], [dir]) = [game] *)
let make_game_from_possible_previous_position game dir prev_pos =
  let mb = moved_balls game prev_pos dir in
  GameS.add prev_pos
    (GameS.union
       (GameS.map (ball_dir_incr dir) mb)
       (GameS.diff game mb))

(* pick a random game in [games] 
   throw exception "Not_found" if GameS.equal games GameS.empty*)
let random_choose games =
  let rec aux n games =
    let candidate = GameSS.choose games in
    if Random.int n = 0 then candidate
    else aux (n - 1) (GameSS.remove candidate games)
  in
  aux (GameSS.cardinal games) games

(* contains all the games of size 1 *)
let games_of_size_1 =
  let list_of_all_int_x_int =
    let rec aux u =
      match u with
      | [] -> failwith "this is NOT happening"
      | (x, y) :: us ->
          if x = max_x - 1 then
            if y = max_y - 1 then (x, y) :: us
            else aux ((0, y + 1) :: (x, y) :: us)
          else aux ((x + 1, y) :: (x, y) :: us)
    in
    aux [(0, 0)]
  in
  (* [game_of_size_1_from_int_x_int (x, y)] return a game with a single ball at position [(x, y)] *)
  let game_of_size_1_from_int_x_int (x, y) =
    GameS.singleton (make_ball (Position.of_int x y))
  in
  List.fold_left (flip GameSS.add) GameSS.empty
    (List.map game_of_size_1_from_int_x_int (list_of_all_int_x_int))

(* see .mli *)
let rec get_valid_game equiprobable size =
  let aux () =
    (* [predecessors game] returns all the games such that it is possible to get [game] by playing once *)
    let predecessors game =
      (* [predecessors_were_that_ball_was_played game ball] 
         return all the predecessors such that you can get [game] by playing once, 
         and the ball you played lands in [ball]'s position *)
      let predecessors_were_that_ball_was_played game ball =
        let mgfppp' dir ball =
          let r = make_game_from_possible_previous_position game dir ball in
          if GameS.for_all ball_inside_bounds r then GameSS.singleton r
          else GameSS.empty
        in
        (* yeah this kind of stuff is anoying *)
        let cancerous_map f game =
          List.fold_left GameSS.union GameSS.empty
            (List.map f (GameS.elements game))
        in
        let ppps_up = possible_previous_positions game Up ball in
        let ppps_right = possible_previous_positions game Right ball in
        let ppps_down = possible_previous_positions game Down ball in
        let ppps_left = possible_previous_positions game Left ball in
        GameSS.union
          (cancerous_map (mgfppp' Up) ppps_up)
          (GameSS.union
             (cancerous_map (mgfppp' Right) ppps_right)
             (GameSS.union
                (cancerous_map (mgfppp' Down) ppps_down)
                (cancerous_map (mgfppp' Left) ppps_left)))
      in
      let r =
        List.fold_left GameSS.union GameSS.empty
          (List.map
             (predecessors_were_that_ball_was_played game)
             (GameS.elements game))
      in
      (* if equiprobable, we return every predecessors, one is chosen at random *)
      if equiprobable then r else GameSS.singleton (random_choose r)
    in
    let rec iterator i games_of_size_i =
      (*let size_good game = GameS.cardinal game = i in*)
      if i = size then games_of_size_i
      else
        iterator (i + 1)
          (List.fold_left GameSS.union GameSS.empty
             (List.map predecessors (GameSS.elements games_of_size_i)))
    in
    let games_of_size_size = iterator 1 games_of_size_1 in
    random_choose games_of_size_size
  in
  (* if equiprobable, game_of_size_size is only empty 
     if you called the function on a size so big 
     that there is no solvable game of this size *)
  if equiprobable then GameS.map new_id (aux ())
    (* if not equiprobable, if you are not lucky, 
     you may choose a branch that does not have any solvable predecessors 
     in that case, you just retry
     this is why this function does not terminate on a size so big 
     that there is no solvable game of this size and equiprobable = true *)
  else
    try GameS.map new_id (aux ()) with Not_found ->
      get_valid_game equiprobable size
