(***********************************/*******|*****************************************)
(* <draw.ml>                       /\ /  /\ | /\ ####################################*)
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

module G = Graphics

let width = 1024

let height = 1024

let line_height = 33

let padding_left = 10

let padding_right = 10

let padding_up = 10

let padding_down = 10

(*let margin = 5*)

let cell_size = ref 0

let colors_generated = ref false

let colors = ref []

let get_color, set_color =
  let get_color' rcolor () = !rcolor in
  let set_color' rcolor color =
    G.set_color color ;
    rcolor := color
  in
  let rcolor = ref G.black in
  (get_color' rcolor, set_color' rcolor)

let generate_new_color color =
  let from_rgb c =
    let r = c / (256 * 256) in
    let g = c / 256 mod 256 in
    let b = c mod 256 in
    (r, g, b)
  in
  let mix i i' = (i + i') / 2 in
  let red = Random.int 256 in
  let green = Random.int 256 in
  let blue = Random.int 256 in
  let old_red, old_green, old_blue = from_rgb color in
  G.rgb (mix red old_red) (mix green old_green) (mix blue old_blue)

let init_window () =
  G.set_window_title "Fling" ;
  G.resize_window width height ;
  G.clear_graph ()

let close_window () = G.close_graph ()

let draw_grid cols rows =
  G.set_color G.black ;
  let cell_width = (width - padding_left - padding_right) / cols in
  let cell_height = (height - padding_up - padding_down) / rows in
  cell_size := min cell_width cell_height ;
  let start_x, start_y = (padding_left, padding_down) in
  let end_x, end_y =
    (start_x + (cols * !cell_size), start_y + (rows * !cell_size))
  in
  G.moveto start_x start_y ;
  for _ = 0 to cols do
    G.lineto (G.current_x ()) end_y ;
    G.moveto (G.current_x () + !cell_size) start_y
  done ;
  G.moveto padding_left padding_down ;
  for _ = 0 to rows do
    G.lineto end_x (G.current_y ()) ;
    G.moveto start_x (G.current_y () + !cell_size)
  done

let images =
  G.open_graph "" ;
  Array.map Image.image_of_path
    (Array.map ((^) "img/")
       [|"booba.ppm"
        ; "chuck_norris.ppm"
        ; "comon.ppm"
        ; "gims.ppm"
        ; "goubault.ppm"
        ; "haddad.ppm"
        ; "igor.ppm"
        ; "jul.ppm"
        ; "kaaris.ppm"
        ; "schwoon.ppm"
        ; "sofiane.ppm"|])
    
let draw_ball ?(select = false) ball =
  let p = Rules.position_of_ball ball in
  let size = !cell_size in
  let x = padding_left + (Position.proj_x p * size) + (size / 2) in
  let y = padding_left + (Position.proj_y p * size) + (size / 2) in
  (*let radius = (size -margin) / 2 in*)
  ( if select then set_color G.red
  else if !colors_generated then
    let color =
      fst (List.find (fun cb -> Rules.eq_ball (snd cb) ball) !colors)
    in
    set_color color
  else
    let color = generate_new_color G.white in
    colors := (color, ball) :: !colors ;
    set_color color ) ;
  if select then (
    G.draw_circle x y 32 ;
    G.draw_circle x y (32 + 1) ;
    G.draw_circle x y (32 + 2) )
  else
    let id = Rules.get_id ball in
    G.draw_image images.(id mod Array.length images) (x - 32) (y - 32)

let draw_balls balls = List.iter draw_ball balls

let draw_string s =
  G.moveto (width / 2) (height - padding_up) ;
  set_color G.red ;
  G.draw_string s

let draw_game cols rows game =
  G.clear_graph () ;
  draw_grid cols rows ;
  draw_balls (Rules.get_balls game)

let position_of_coord x y =
  let size = !cell_size in
  let x', y' = (x - padding_left, y - padding_down) in
  Position.of_int (x' / size) (y' / size)

let draw_menu l =
  G.clear_graph () ;
  set_color G.black ;
  let x, y = (width / 2, height / 2) in
  G.moveto x y ;
  ignore
  @@ List.fold_left
       (fun (i, y) (name, _) ->
         G.draw_string (Printf.sprintf "%d : %s" i name) ;
         let y' = y - line_height in
         G.moveto x y' ;
         (i + 1, y') )
       (0, y) l

let ready b = colors_generated := b
