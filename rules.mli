(***********************************/**_****|*****************************************)
(* <rules.mli>                     /\ /  /\ | /\ ####################################*)
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

type ball

type move

module MoveSet : (Set.S with type elt = move)

module GameS : (Set.S with type elt = ball)

type game = GameS.t

(** [max_x] the width of the game board *)
val max_x : int
  
(** [max_x] the height of the game board *)
val max_y : int

(** [make_ball pos] returns a new ball at position [pos] *)
val make_ball : Position.t -> ball

(** [new_id b] returns a new ball at the same position as [b]  but with a new id*)
val new_id : ball -> ball

(** [new_game ball_list] returns a new game form a list of balls [ball_list] *)
val new_game : ball list -> game

(** [eq_ball ball ball'] returns true if and only if ball and ball' are equals
    indenpendetly from their position since balls can move *)
val eq_ball : ball -> ball -> bool

(** [make_move b d] returns a new move from a ball [b] and a direction [d] *)
val make_move : ball -> direction -> move

(** [apply_move game move] returns a new game where [move] has been applied to [game] *)
val apply_move : game -> move -> game

(** [moves game] returns all the valid moves possible for [game] *)

(** [existing_moves game] returns the set of all the moves applicable to the current game, valid of not *)
val existing_moves : game -> MoveSet.t

(** [moveset game] returns the set of all valid moves for the given game*)
val moveset : game -> MoveSet.t

(** [moves game] returns the list of all valid moves for the given game*)
val moves : game -> move list

(** [get_balls game] returns the current list of ball on the [game] *)
val get_balls : game -> ball list

(** [is_ball pos] returns true if and only if their is a ball on the position [pos] *)
val is_ball : game -> Position.t -> bool

(** [ball_of_position game pos] returns the ball that is on the position [pos]. Fail if their is none *)
val ball_of_position : game -> Position.t -> ball

(** [position_of_ball ball] returns the position of the ball [ball] *)
val position_of_ball : ball -> Position.t

(** [is_win game] returns wether the game is won or not *)
val is_win : game -> bool

(** [get_id b] returns the id of ball [b] *)
val get_id : ball -> int

(** [ball_inside_bounds b] return true if [b] is inside the bounds of the game board *) 
val ball_inside_bounds : ball -> bool

(** [ball_dir_incr d b] returns a ball 1 meter apart from [b] in the direction [d]
    the id is the same as [b]'s *)
val ball_dir_incr : direction -> ball -> ball

(** [ball_dir_incr d b] returns a ball 1 meter apart from [b] in the direction opposite of [d]
    the id is the same as [b]'s *)
val ball_dir_deincr : direction -> ball -> ball
