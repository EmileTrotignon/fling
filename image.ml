(*                                  /  _    |                                        *)
(* <image.ml>                      /\ /  /\ | /\ ####################################*)
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

type rgb = {r: int; g: int; b: int}

(*
let rgb_of_int i =
  {r= i / (256 * 256); g = (i / 256) mod 256; b = i mod 256}
*)
let int_of_rgb c =
  (c.r * 256 * 256) + (c.g * 256) + c.b 

(*
let output_ppm oc img =
  let dump = G.dump_image img in
  let width = Array.length dump.(0) in
  let height = Array.length dump in
  Printf.fprintf oc "P6\n%d %d\n255\n" width height;
  
  for y = 0 to pred height do
    for x = 0 to pred width do
      let c = rgb_of_int dump.(y).(x) in
      output_char oc (char_of_int c.r);
      output_char oc (char_of_int c.g);
      output_char oc (char_of_int c.b);
    done;
  done;
  output_char oc '\n';
  flush oc
 *)
let load_image ic =
  let aux width height =
    let read_int ic' =
      int_of_string (input_line ic')
    in
    let mat = Array.make_matrix height width 0 in
    for y = 0 to pred height do
      for x = 0 to pred width do
        let b = read_int ic in
        let r = read_int ic in
        let g = read_int ic in
        let c = {r = r; g = g; b = b} in
        mat.(y).(x) <- int_of_rgb c
      done;
    done;
    G.make_image mat
  in
  let _ = input_line ic in
  let line = input_line ic in
  Scanf.sscanf (line) "%d %d" aux 

let image_of_path path =
  load_image (open_in path)
