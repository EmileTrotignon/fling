(*                                  /  _    |                                        *)
(* <game.ml>                       /\ /  /\ | /\ ####################################*)
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

open Utils
module G = Graphics
module D = Draw

(* max width of the grid printed *)
let max_x = Rules.max_x

(* max height of the grid printed *)
let max_y = Rules.max_y

let number_of_balls = 10

let equiprobable = false

(* game is a reference to the initial game. *)
let game = ref (Rules.new_game [])

(* return the ball that the player wants to move *)
let rec get_ball game =
  let status = G.wait_next_event [G.Button_down] in
  let x, y = (status.G.mouse_x, status.G.mouse_y) in
  let p = D.position_of_coord x y in
  if Rules.is_ball game p then (
    let ball = Rules.ball_of_position game p in
    D.draw_ball ~select:true ball ;
    (* to show which ball has been selected *)
    ball )
  else get_ball game

(* the player has selected an empty cell *)

(* convert the key pressed into a char and call the continuation k on it *)
let get_key_pressed k =
  let status = G.wait_next_event [G.Key_pressed] in
  let key = Char.code status.G.key in
  k (Char.chr key)

(* return the direction choosen by the player *)
let rec get_ball_direction () =
  let dir_of_char c =
    Rules.(
      match c with
      | 'z' -> Some Up
      | 's' -> Some Down
      | 'd' -> Some Right
      | 'q' -> Some Left
      | _ -> None)
  in
  get_key_pressed (fun c ->
      match dir_of_char c with Some x -> x | None -> get_ball_direction ()
      (* wrong key pressed by the player *) )

(* get the next move of the player *)
let get_next_move game =
  let p = get_ball game in
  let d = get_ball_direction () in
  Rules.make_move p d

(* create_game does not allows the player to create its own game by putting balls over the grid 
   because this is way to easy *)
let create_game () = Valid.get_valid_game equiprobable number_of_balls

(* A menu is a pair of string * f where f is a function of type unit -> unit.
   If the player choose on the menu which function should be called *)
let rec menu = [("solve", solve); ("play", play); ("exit", leave)]

(* play allows the player to create a new game, and then try to solve it *)
and play () =
  game := create_game () ;
  loop !game

(* solve allows the player to create a new game and then see if the game can be solved *)
and solve () =
  game := create_game () ;
  solver !game

(* loop game loops on the game while their is still moves possible for the player *)
and loop game =
  D.draw_game max_x max_y game ;
  Rules.(
    let m = moveset game in
    if MoveSet.equal MoveSet.empty m then finish_game (is_win game)
    else
      let next_move = get_next_move game in
      if MoveSet.mem next_move m then
        let game' = Rules.apply_move game next_move in
        loop game'
      else loop game)

(* solver game solve the game if it is possible *)
and solver game =
  D.draw_game max_x max_y game ;
  let moves = Solver.solve game in
  match moves with
  | None ->
      D.draw_string "No solution!" ;
      get_key_pressed (fun _ -> main menu)
  | Some moves ->
      let g =
        List.fold_left
          (fun g m ->
            D.draw_game max_x max_y g ;
            D.draw_string "Solved!" ;
            get_key_pressed (fun _ -> ()) ;
            Rules.apply_move g m )
          game moves
      in
      D.draw_game max_x max_y g ;
      get_key_pressed (fun _ -> main (("resolve", resolve) :: menu))

(* replay the previous game *)
(*and replay () =
  loop !game*)
(* resolve the preivous game *)
and resolve () = solver !game

(* leave the application *)
and leave () = D.close_window ()

(* finish a game properly *)
and finish_game win =
  let victory_strings =
    [| "JE DIS OUI !!!"
     ; "Etttt le but du joueur francais c'est incroyable !!!"
     ; "Une victoire est une victoire. C'est le resultat qui compte."
     ; "Veni, vedi, vici"
     ; "Qu'est ce que ca fait d'etre fauche ? Je n'm'en rappelle plus"
     ; "D.U.C, je confirme on a touche la cible"
     ; "La ville est sous controle"
     ; "Touche coule"
     ; "A vaincre sans peril, on s'amuse bien quand meme"
     ; "C'est bien ca. Allez, va le faire au tableau" |]
  in
  let defeat_strings =
    [| "Non Zinedine ! Pas apres tout ca Zinedine !"
     ; "Se battre pour une cause juste est déjà une victoire. - proverbre \
        de perdant"
     ; "La defaite est novatrice, la victoire est conservatrice."
     ; "La victoire n’est rien, mon garçon, la victoire ne laisse pas de \
        trace, c’est un assouvissement passager. La vie, c’est la \
        defaite. - proverbre de perdant"
     ; "On apprend peu par la victoire, mais beaucoup par la defaite. - \
        proverbre de perdant"
     ; "L'important c'est de participer - ta maitresse en CE2"
     ; "Je suis desole, mais je dois dire non :("
     ; "T'as perdu"
     ; "C'est consternant"
     ; "Vous n'etes pas inversible"
     ; "Il va falloir changer d'attitude, ou de carriere"
     ; "Ma petite soeur de cinq ans a deja reussi avec cette combinaison de \
        depart"
     ; "Comme d'habitude..." |]
  in
  D.set_color (D.get_color () / 2) ;
  (*G.clear_graph () ;*)
  G.moveto (D.width / 4) (D.height / 2) ;
  G.set_color G.red ;
  G.draw_string
    ( rand_ele (if win then victory_strings else defeat_strings)
    ^ "   [ PRESS ANY KEY TO PROCEED]" ) ;
  let _ = G.read_key () in
  () ; main menu

(* get the choice of the player *)
and main l =
  let choice c =
    let i = int_of_char c - int_of_char '0' in
    if 0 <= i && i < List.length l then snd (List.nth l i) () else main l
  in
  D.init_window () ;
  Random.self_init () ;
  D.draw_menu l ;
  get_key_pressed choice

let _ = main menu
