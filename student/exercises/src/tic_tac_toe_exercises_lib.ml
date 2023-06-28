open Core
open Tic_tac_toe_2023_common
open Protocol

module Evaluation = struct
  type t =
    | Illegal_state
    | Game_over of { winner : Piece.t option }
    | Game_continues
  [@@deriving sexp_of]

  let to_string (t : t) = t |> sexp_of_t |> Sexp.to_string
end

(* Here are some functions which know how to create a couple different kinds
   of games *)
let empty_game =
  let game_id = Game_id.of_int 0 in
  let game_kind = Game_kind.Tic_tac_toe in
  let player_x = Player.Player (Username.of_string "Player_X") in
  let player_o = Player.Player (Username.of_string "Player_O") in
  let game_status = Game_status.Turn_of Piece.X in
  { Game_state.game_id
  ; game_kind
  ; player_x
  ; player_o
  ; pieces = Position.Map.empty
  ; game_status
  }
;;

let place_piece (game : Game_state.t) ~piece ~position : Game_state.t =
  let pieces = Map.set game.pieces ~key:position ~data:piece in
  { game with pieces }
;;

let win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let win_for_o =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 2 }
;;

let%expect_test "print_win_for_o" =
  print_endline (Game_state.to_string_hum win_for_o);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    OXO
    XXO
    XOO |}]
;;

let first_row_win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 2 }
;;

let%expect_test "print_first_row_win_for_x" =
  print_endline (Game_state.to_string_hum first_row_win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XXX
    O
    O |}]
;;

let first_row_win_for_o =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
;;

let%expect_test "print_first_row_win_for_o" =
  print_endline (Game_state.to_string_hum first_row_win_for_o);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    OOO
    X
    X |}]
;;

let third_row_win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
;;

let%expect_test "print_third_row_win_for_x" =
  print_endline (Game_state.to_string_hum third_row_win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))

    OO
    XXX |}]
;;

let third_row_win_for_o =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
;;

let%expect_test "print_third_row_win_for_o" =
  print_endline (Game_state.to_string_hum third_row_win_for_o);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))

    XX
    OOO |}]
;;

let first_col_win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
;;

let%expect_test "print_first_col_win_for_x" =
  print_endline (Game_state.to_string_hum first_col_win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XO
    X
    X O |}]
;;

let third_col_win_for_o =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 2 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 2 }
;;

let%expect_test "print_third_col_win_for_o" =
  print_endline (Game_state.to_string_hum third_col_win_for_o);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
      O
    XXO
      O |}]
;;

let left_diag_win_for_x =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
;;

let%expect_test "print_left_diag_win_for_x" =
  print_endline (Game_state.to_string_hum left_diag_win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XO
     X
    O X |}]
;;

let right_diag_win_for_o =
  empty_game
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 0; column = 2 }
;;

let%expect_test "right_diag_win_for_o" =
  print_endline (Game_state.to_string_hum right_diag_win_for_o);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XXO
     O
    O |}]
;;

let non_win =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 0; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 2 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 2; column = 0 }
;;

let board_size ~(game_kind : Game_kind.t) : int =
  match game_kind with Tic_tac_toe -> 3 | Omok -> 15
;;

let win_count ~(game_kind : Game_kind.t) : int =
  match game_kind with Tic_tac_toe -> 3 | Omok -> 5
;;

let create_board ~(game_kind : Game_kind.t) : Position.t list =
  List.concat
    (List.init (board_size ~game_kind) ~f:(fun row ->
       List.init (board_size ~game_kind) ~f:(fun column ->
         { Position.row; column })))
;;

(* Exercise 1.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)
let available_moves
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let positions = create_board ~game_kind in
  List.filter positions ~f:(fun position ->
    match Map.find pieces position with Some _ -> false | None -> true)
;;

(* Exercise 2.

   For instructions on implemeting this refer to the README.

   After you are done with this implementation, you can uncomment out
   "evaluate" test cases found below in this file. *)

let rec evaluate_row
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(count : int)
  ~(position : Position.t)
  ~(me : Piece.t)
  : bool
  =
  if count = win_count ~game_kind
  then true
  else if not (Position.in_bounds ~game_kind position)
  then false
  else (
    match Map.find pieces position with
    | Some piece ->
      if Piece.equal piece me
      then
        evaluate_row
          ~game_kind
          ~pieces
          ~count:(count + 1)
          ~position:(Position.right position)
          ~me
      else
        evaluate_row
          ~game_kind
          ~pieces
          ~count:0
          ~position:(Position.right position)
          ~me
    | None ->
      evaluate_row
        ~game_kind
        ~pieces
        ~count:0
        ~position:(Position.right position)
        ~me)
;;

let third_row_no_win_for_x_1 =
  empty_game
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 0 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 0 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 2; column = 1 }
  |> place_piece ~piece:Piece.O ~position:{ Position.row = 1; column = 1 }
  |> place_piece ~piece:Piece.X ~position:{ Position.row = 1; column = 2 }
;;

let%expect_test "print_third_row_win_for_o" =
  printf
    "%b"
    (evaluate_row
       ~game_kind:Game_kind.Tic_tac_toe
       ~pieces:third_row_no_win_for_x_1.pieces
       ~count:0
       ~position:{ Position.row = 1; column = 0 }
       ~me:Piece.X);
  [%expect {| false |}]
;;

let rec evaluate_col
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(count : int)
  ~(position : Position.t)
  ~(me : Piece.t)
  : bool
  =
  if count = win_count ~game_kind
  then true
  else if not (Position.in_bounds ~game_kind position)
  then false
  else (
    match Map.find pieces position with
    | Some piece ->
      if Piece.equal piece me
      then
        evaluate_col
          ~game_kind
          ~pieces
          ~count:(count + 1)
          ~position:(Position.down position)
          ~me
      else
        evaluate_col
          ~game_kind
          ~pieces
          ~count:0
          ~position:(Position.down position)
          ~me
    | None ->
      evaluate_col
        ~game_kind
        ~pieces
        ~count:0
        ~position:(Position.down position)
        ~me)
;;

let rec evaluate_left_diag
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(count : int)
  ~(position : Position.t)
  ~(me : Piece.t)
  : bool
  =
  if count = win_count ~game_kind
  then true
  else if not (Position.in_bounds ~game_kind position)
  then false
  else (
    match Map.find pieces position with
    | Some piece ->
      if Piece.equal piece me
      then
        evaluate_left_diag
          ~game_kind
          ~pieces
          ~count:(count + 1)
          ~position:(Position.down_left position)
          ~me
      else
        evaluate_left_diag
          ~game_kind
          ~pieces
          ~count:0
          ~position:(Position.down_left position)
          ~me
    | None ->
      evaluate_left_diag
        ~game_kind
        ~pieces
        ~count:0
        ~position:(Position.down_left position)
        ~me)
;;

let rec evaluate_right_diag
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  ~(count : int)
  ~(position : Position.t)
  ~(me : Piece.t)
  : bool
  =
  if count = win_count ~game_kind
  then true
  else if not (Position.in_bounds ~game_kind position)
  then false
  else (
    match Map.find pieces position with
    | Some piece ->
      if Piece.equal piece me
      then
        evaluate_right_diag
          ~game_kind
          ~pieces
          ~count:(count + 1)
          ~position:(Position.down_right position)
          ~me
      else
        evaluate_right_diag
          ~game_kind
          ~pieces
          ~count:0
          ~position:(Position.down_right position)
          ~me
    | None ->
      evaluate_right_diag
        ~game_kind
        ~pieces
        ~count:0
        ~position:(Position.down_right position)
        ~me)
;;

let evaluate ~(game_kind : Game_kind.t) ~(pieces : Piece.t Position.Map.t)
  : Evaluation.t
  =
  let x_win =
    List.exists (create_board ~game_kind) ~f:(fun position ->
      evaluate_row ~game_kind ~pieces ~count:0 ~position ~me:Piece.X
      || evaluate_col ~game_kind ~pieces ~count:0 ~position ~me:Piece.X
      || evaluate_left_diag ~game_kind ~pieces ~count:0 ~position ~me:Piece.X
      || evaluate_right_diag
           ~game_kind
           ~pieces
           ~count:0
           ~position
           ~me:Piece.X)
  in
  let o_win =
    List.exists (create_board ~game_kind) ~f:(fun position ->
      evaluate_row ~game_kind ~pieces ~count:0 ~position ~me:Piece.O
      || evaluate_col ~game_kind ~pieces ~count:0 ~position ~me:Piece.O
      || evaluate_left_diag ~game_kind ~pieces ~count:0 ~position ~me:Piece.O
      || evaluate_right_diag
           ~game_kind
           ~pieces
           ~count:0
           ~position
           ~me:Piece.O)
  in
  if x_win
  then Evaluation.Game_over { winner = Some Piece.X }
  else if o_win
  then Evaluation.Game_over { winner = Some Piece.O }
  else Evaluation.Game_continues
;;

(* Exercise 3. *)
let winning_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let available_moves = available_moves ~game_kind ~pieces in
  List.filter available_moves ~f:(fun move ->
    let new_pieces = Map.set pieces ~key:move ~data:me in
    match evaluate ~game_kind ~pieces:new_pieces with
    | Evaluation.Game_over { winner = Some winner } -> Piece.equal me winner
    | Evaluation.Game_over { winner = None }
    | Evaluation.Game_continues | Evaluation.Illegal_state ->
      false)
;;

(* Exercise 4. *)
let losing_moves
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t list
  =
  let opponent = Piece.flip me in
  let available_moves = available_moves ~game_kind ~pieces in
  List.filter available_moves ~f:(fun move ->
    let new_pieces = Map.set pieces ~key:move ~data:opponent in
    match evaluate ~game_kind ~pieces:new_pieces with
    | Evaluation.Game_over { winner = Some winner } ->
      Piece.equal opponent winner
    | Evaluation.Game_over { winner = None }
    | Evaluation.Game_continues | Evaluation.Illegal_state ->
      false)
;;

let exercise_one =
  Command.basic
    ~summary:"Exercise 1: Where can I move?"
    (let%map_open.Command () = return () in
     fun () ->
       let moves =
         available_moves
           ~game_kind:win_for_x.game_kind
           ~pieces:win_for_x.pieces
       in
       print_s [%sexp (moves : Position.t list)];
       let moves =
         available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
       in
       print_s [%sexp (moves : Position.t list)])
;;

let exercise_two =
  Command.basic
    ~summary:"Exercise 2: Did is the game over?"
    (let%map_open.Command () = return () in
     fun () ->
       let evaluation =
         evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
       in
       print_s [%sexp (evaluation : Evaluation.t)])
;;

let exercise_three =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 3: Is there a winning move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let winning_moves =
         winning_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (winning_moves : Position.t list)];
       ())
;;

let exercise_four =
  let piece_options =
    Piece.all |> List.map ~f:Piece.to_string |> String.concat ~sep:", "
  in
  Command.basic
    ~summary:"Exercise 4: Is there a losing move?"
    (let%map_open.Command () = return ()
     and piece =
       flag
         "piece"
         (required (Arg_type.create Piece.of_string))
         ~doc:("PIECE " ^ piece_options)
     in
     fun () ->
       let losing_moves =
         losing_moves
           ~me:piece
           ~game_kind:non_win.game_kind
           ~pieces:non_win.pieces
       in
       print_s [%sexp (losing_moves : Position.t list)];
       ())
;;

let%expect_test "print_win_for_x" =
  print_endline (Game_state.to_string_hum win_for_x);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    XOX
    OOX
    OXX |}]
;;

let%expect_test "print_non_win" =
  print_endline (Game_state.to_string_hum non_win);
  [%expect
    {|
    ((game_id 0)(game_kind Tic_tac_toe)(player_x(Player Player_X))(player_o(Player Player_O))(game_status(Turn_of X)))
    X
    O
    O X |}]
;;

let%expect_test "yes available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect
    {|
  (((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 1)) 
   ((row 1) (column 2)) ((row 2) (column 1))) |}]
;;

let%expect_test "no available_moves" =
  let (moves : Position.t list) =
    available_moves ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
    |> List.sort ~compare:Position.compare
  in
  print_s [%sexp (moves : Position.t list)];
  [%expect {| () |}]
;;

(* When you've implemented the [evaluate] function, uncomment the next two
   tests! *)
let%expect_test "evalulate_win_for_x" =
  print_endline
    (evaluate ~game_kind:win_for_x.game_kind ~pieces:win_for_x.pieces
     |> Evaluation.to_string);
  [%expect {| (Game_over(winner(X))) |}]
;;

let%expect_test "evalulate_non_win" =
  print_endline
    (evaluate ~game_kind:non_win.game_kind ~pieces:non_win.pieces
     |> Evaluation.to_string);
  [%expect {| Game_continues |}]
;;

(* When you've implemented the [winning_moves] function, uncomment this
   test! *)
let%expect_test "winning_move" =
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.X
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {|
    X 5 ((row 1) (column 1))
    (((row 1) (column 1)))
  |}];
  let positions =
    winning_moves
      ~game_kind:non_win.game_kind
      ~pieces:non_win.pieces
      ~me:Piece.O
  in
  print_s [%sexp (positions : Position.t list)];
  [%expect {|
    () |}]
;;

(* When you've implemented the [losing_moves] function, uncomment this
   test! *)
(*let%expect_test "print_losing" = let positions = losing_moves
  ~game_kind:non_win.game_kind ~pieces:non_win.pieces ~me:Piece.X in print_s
  [%sexp (positions : Position.t list)]; [%expect {| () |}]; let positions =
  losing_moves ~game_kind:non_win.game_kind ~pieces:non_win.pieces
  ~me:Piece.O in print_s [%sexp (positions : Position.t list)]; [%expect {|
  ((((row 0) (column 1)) ((row 0) (column 2)) ((row 1) (column 2)) ((row 2)
  (column 1)))) |}] ;;*)
