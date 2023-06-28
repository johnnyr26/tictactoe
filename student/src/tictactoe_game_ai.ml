open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let positions =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  List.random_element_exn positions
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if List.length winning_moves > 0
  then List.random_element_exn winning_moves
  else random_move_strategy ~game_kind ~pieces
;;

let _ = pick_winning_move_if_possible_strategy

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  if List.length winning_moves > 0
  then List.random_element_exn winning_moves
  else (
    let losing_moves =
      Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
    in
    if List.length losing_moves > 0
    then List.random_element_exn losing_moves
    else random_move_strategy ~game_kind ~pieces)
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let evaluate = Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces in
  match evaluate with
  | Tic_tac_toe_exercises_lib.Evaluation.Game_over { winner = Some winner }
    ->
    if Piece.equal winner me then Float.infinity else Float.neg_infinity
  | Game_over { winner = None } | Game_continues | Illegal_state -> 0.0
;;

let _ = score

(* function minimax(node, depth, maximizingPlayer) is if depth = 0 or node is
   a terminal node then return the heuristic value of node if
   maximizingPlayer then value := −∞ for each child of node do value :=
   max(value, minimax(child, depth − 1, FALSE)) return value else (*
   minimizing player *) value := +∞ for each child of node do value :=
   min(value, minimax(child, depth − 1, TRUE)) return value *)

(* Initial call *)
(* minimax(origin, depth, TRUE) *)

let terminal_nodes ~(me : Piece.t) ~(game_state : Game_state.t) =
  let pieces = game_state.pieces in
  let game_kind = Game_kind.Tic_tac_toe in
  let winning_moves =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  let losing_moves =
    Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
  in
  let available_moves =
    Tic_tac_toe_exercises_lib.available_moves
      ~game_kind:Game_kind.Tic_tac_toe
      ~pieces
  in
  List.filter available_moves ~f:(fun position ->
    List.mem winning_moves position ~equal:Position.equal
    || List.mem losing_moves position ~equal:Position.equal
    || List.length available_moves = 1)
;;


(* TODO: Discuss implementation of this code with TA *)
let rec minimax
  ~(node : Position.t)
  ~(depth : int)
  ~(maximizing_player : bool)
  ~(me : Piece.t)
  ~(game_state : Game_state.t)
  : float
  =
  let pieces = game_state.pieces in
  let game_kind = Game_kind.Tic_tac_toe in
  let available_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  if depth = 0
     || List.mem (terminal_nodes ~me ~game_state) node ~equal:Position.equal
  then score ~me ~game_kind:Game_kind.Tic_tac_toe ~pieces:game_state.pieces
  else if maximizing_player
  then (
    let value = Float.neg_infinity in
    List.fold available_moves ~init:value ~f:(fun value child ->
      match
        List.max_elt
          [ value
          ; minimax
              ~node:child
              ~depth:(depth - 1)
              ~maximizing_player:false
              ~me
              ~game_state
          ]
          ~compare:Float.compare
      with
      | Some elt -> elt
      | None -> value))
  else (
    let value = Float.infinity in
    List.fold available_moves ~init:value ~f:(fun value child ->
      match
        List.max_elt
          [ value
          ; minimax
              ~node:child
              ~depth:(depth - 1)
              ~maximizing_player:true
              ~me
              ~game_state
          ]
          ~compare:Float.compare
      with
      | Some elt -> elt
      | None -> value))
;;

let _ = minimax

(* TODO: if depth = 0 or node is a terminal node then...*)

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  pick_winning_move_or_block_if_possible_strategy
    ~me
    ~game_kind:Tic_tac_toe
    ~pieces:game_state.pieces
;;
