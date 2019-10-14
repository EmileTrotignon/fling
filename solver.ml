(***********************************/**_****|*****************************************)
(* <solver.ml>                     /\ /  /\ | /\ ####################################*)
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

let solve game' =
  (* [aux game possible_moves] returns None if game is not solvable
     else return Some solution where solution is the list of moves required to solve this instance
     Here, if [possible_moves] = None, every possible move is permitted (we compute it with moveset), 
     and if [possible_moves] = Some moveset, only the moves in moveset are permitted *)
  let rec aux game possible_moves =
    Rules.(
      (* if you won then no more moves are required *)
      if is_win game then Some []
      else
        
        let possible_moves' =
          match possible_moves with None -> moveset game | Some ms -> ms
        in
        if MoveSet.equal possible_moves' MoveSet.empty then None
        else
          let ele = MoveSet.choose possible_moves' in
          match aux (apply_move game ele) None with
          (* if a applying the current move gives a solution, returns that solution *)
          | Some eles -> Some (ele :: eles)
          (* if it doesnt, remove that move from the set of possible moves and do it all over again *)
          | None -> aux game (Some (MoveSet.remove ele possible_moves')))
  in
  aux game' None


             
