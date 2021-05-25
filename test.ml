(** Test plan:

    - Before we were able to successfully implement the GUI, we used
      OUnit to test our state file by testing the following basic
      functionalities and corner cases:

    1. pushing a block into a legal position

    2. pushing a block into an illegal position

    3. pushing two blocks next to each other

    4. pushing a block onto a hole

    5. pushing a block off of a hole

    6. legal player movement

    7. illegal player movement

    8. winning the game

    9. losing the game

    10. registering a command

    etc.

    We further tested registering a text command and making sure that
    our genmap function was creating solvable maps.

    Overall we tested the state, genmap, text, and command modules using
    OUnit tests. We used blackbox testing to make sure each function
    being tested was returning the correct value. e.g. we tested to make
    sure [move] returned the correct new state. Because our functions
    use many other helper functions, we decided not to prioritize
    glassbox testing.

    In general, most of the testing was further done manually once the
    GUI was working. For example, we tested block pushing, checking win
    condition, buttons, select modes, menus and other in-game
    interactions manually. Functions that were responsible for drawing
    images to the screen (i.e. gui) were also tested manually.

    - Since we are developing a game, almost all the bugs can be caught
      from visible and unprecedented moves, so while we focused on
      automated testing before the GUI was working and using it to make
      sure the basic functionality of the game was working (pushing,
      moving, winning), we later transitioned to much more manual
      testing. Thus, we believe our approach demonstrates the
      correctness of the program because we have solidified the
      correctness of our state transitions using OUnit tests and have
      ensured the user experience is accurate using manual testing which
      is what games are all about. *)

open Camlimages
open OUnit2
open State
open Graphics
open Png
open Images
open Jpeg
open Types
open Genmap
open Text
open Map
open State
open Command

let pp_string s = "\"" ^ s ^ "\""

let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

let parse_test name dialogue width expected_value =
  name >:: fun ctxt ->
  assert_equal expected_value
    (parse_dialogue dialogue [] width)
    ~printer:(pp_list pp_string)

let width_test name room_id st tile_size expected_value =
  name >:: fun ctxt ->
  assert_equal expected_value
    (snd (get_hw room_id st tile_size))
    ~printer:string_of_int

let height_test name room_id st tile_size expected_value =
  name >:: fun ctxt ->
  assert_equal expected_value
    (fst (get_hw room_id st tile_size))
    ~printer:string_of_int

let player_one =
  {
    position = (1, 1);
    on_exit = false;
    player_num = Fst;
    player_img = "images/link60x60.png";
  }

let player_two =
  {
    position = (3, 3);
    on_exit = false;
    player_num = Snd;
    player_img = "images/player60x60.png";
  }

let room_list_1 = [ map2; map_maze; win; lose ]

let player_list_1 = [ player_one ]

let player_list_2 = [ player_one; player_two ]

let room_list_2 = [ testing_map; win; lose ]

let state_gen id mode rooms first_room (player_list : player list) =
  {
    mode;
    active = true;
    current_room_id = id;
    all_rooms = rooms;
    players = player_list;
    filled_holes = 0;
    blocks = first_room.init_blocks;
    breaks = first_room.init_breaks;
    steps_left = first_room.step_limit;
  }

let some_state =
  state_gen "random" Normal room_list_1 map2 player_list_1

let some_state_2_player =
  state_gen "random" Normal room_list_1 map2 player_list_2

let state_mov_norm =
  state_gen "test" Normal room_list_2 testing_map player_list_1

let state_mov_norm_limit =
  state_gen "test" Limit room_list_2 testing_map player_list_1

let state_mov_norm_two_player =
  state_gen "test" Normal room_list_2 testing_map player_list_2

let init_state_test name sel_list expected_value =
  name >:: fun ctxt -> assert_equal expected_value (init_state sel_list)

let get_state_tests name state func expected_value =
  name >:: fun ctxt -> assert_equal expected_value (func state)

let some_room = get_room_by_id "random" some_state

let p2_3_2 () =
  match move state_mov_norm_two_player Down testing_map Snd with
  | Legal st -> st
  | Illegal -> state_mov_norm_two_player

let p1_1_2 () =
  match move state_mov_norm_two_player Up testing_map Fst with
  | Legal st -> st
  | Illegal -> state_mov_norm_two_player

let p1_3_1 () =
  match move state_mov_norm_two_player Right testing_map Fst with
  | Legal st -> (
      match move st Right testing_map Fst with
      | Legal st2 -> st2
      | Illegal -> st)
  | Illegal -> state_mov_norm_two_player

let compare_command
    (name : string)
    (result : command)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output result

let compare_exception (name : string) result (expected_except : exn) :
    test =
  name >:: fun _ -> assert_raises expected_except result

let malformed1 () = parse "Start"

let malformed2 () = parse "quiT"

let malformed3 () = parse "hello world"

let malformed4 () = parse "quit 1"

let malformed5 () = parse "start 1234"

let empty1 () = parse ""

let empty2 () = parse " "

let move_test_player (name : string) result player_num expected_result :
    test =
  name >:: fun _ ->
  match result with
  | Illegal -> failwith "Illegal"
  | Legal state ->
      let player_select =
        List.find
          (fun (player : player) -> player.player_num = player_num)
          state.players
      in
      assert_equal expected_result player_select.position

let move_test_illegal (name : string) result : test =
  name >:: fun _ -> assert_equal Illegal result

let rec rec_move result dir_list room player_num =
  List.fold_left
    (fun init dir ->
      match init with
      | Illegal -> Illegal
      | Legal st -> move st dir room player_num)
    result dir_list

let move_test_room_id (name : string) result id : test =
  name >:: fun _ ->
  match result with
  | Illegal -> failwith "Illegal"
  | Legal state ->
      let current_rm_id = state.current_room_id in
      assert_equal current_rm_id id

let move_parse result =
  match result with Legal st -> st | Illegal -> failwith "Illegal"

let move_test_block (name : string) result expected_result : test =
  name >:: fun _ ->
  match result with
  | Illegal -> failwith "Illegal"
  | Legal state ->
      assert_equal (List.mem expected_result state.blocks) true

let normal_tile_00 : tile = { position = (0, 0); ttype = Normal }

let set_pos (tile : tile) (pos : int * int) =
  { tile with position = pos }

let set_type (tile : tile) tile_type = { tile with ttype = tile_type }

let normal_tile_01 = set_pos normal_tile_00 (0, 1)

let normal_tile_02 = set_pos normal_tile_00 (0, 2)

let normal_tile_10 = set_pos normal_tile_00 (1, 0)

let normal_tile_11 = set_pos normal_tile_00 (1, 1)

let normal_tile_12 = set_pos normal_tile_00 (1, 2)

let normal_tile_20 = set_pos normal_tile_00 (2, 0)

let normal_tile_21 = set_pos normal_tile_00 (2, 1)

let normal_tile_22 = set_pos normal_tile_00 (2, 2)

let bound_tile_00 = set_type normal_tile_00 Obstacle

let bound_tile_01 = set_type normal_tile_01 Obstacle

let bound_tile_02 = set_type normal_tile_02 Obstacle

let bound_tile_10 = set_type normal_tile_10 Obstacle

let bound_tile_11 = set_type normal_tile_11 Obstacle

let bound_tile_12 = set_type normal_tile_12 Obstacle

let bound_tile_20 = set_type normal_tile_20 Obstacle

let bound_tile_21 = set_type normal_tile_21 Obstacle

let bound_tile_22 = set_type normal_tile_22 Obstacle

let init_map2x1_exp : tile array array =
  [| [| normal_tile_00; normal_tile_10 |] (* row 0*) |]

let init_map2x2_exp : tile array array =
  [|
    [| normal_tile_01; normal_tile_11 |] (* row 1*);
    [| normal_tile_00; normal_tile_10 |] (* row 0*);
  |]

let init_map3x3_exp : tile array array =
  [|
    [| normal_tile_02; normal_tile_12; normal_tile_22 |] (* row 2*);
    [| normal_tile_01; normal_tile_11; normal_tile_21 |] (* row 1*);
    [| normal_tile_00; normal_tile_10; normal_tile_20 |] (* row 0*);
  |]

let init_map2x1 : tile array array = map_init 2 1 normal_tile_00

let init_map2x2 : tile array array = map_init 2 2 normal_tile_00

let init_map3x3 : tile array array = map_init 3 3 normal_tile_00

let bound_map3x3_exp : tile array array =
  [|
    [| bound_tile_02; bound_tile_12; bound_tile_22 |] (* row 2*);
    [| bound_tile_01; normal_tile_11; bound_tile_21 |] (* row 1*);
    [| bound_tile_00; bound_tile_10; bound_tile_20 |] (* row 0*);
  |]

let bound_map3x3 =
  init_boundary (map_init 3 3 normal_tile_00) bound_tile_00

let genmap_test name generated_map expected_map =
  name >:: fun _ -> assert_equal expected_map generated_map

let tests =
  [
    (*Text module tests *)
    parse_test "parse test, game intro"
      "Press any key to start the game. Use \"asdw\" for player 1 and \
       \"jkli\" for player 2. Fill all holes on the screen by pushing \
       blocks into them, then go to the exit to arrive at the next \
       level."
      600
      [
        "Press any key to start the game. Use \"asdw\" for player 1 \
         and \"jkli\" for player 2. Fill all";
        "holes on the screen by pushing blocks into them, then go to \
         the exit to arrive at the next";
        "level.";
      ];
    parse_test "parse test random writing"
      "I somehow have to write something that is more than a hundred \
       characters and I certainly hope this is enough cause if it's \
       not I'm gonna be super annoyed."
      600
      [
        "I somehow have to write something that is more than a hundred \
         characters and I certainly";
        "hope this is enough cause if it's not I'm gonna be super \
         annoyed.";
      ];
    width_test "width test" "random"
      (init_state ("one", "normal"))
      60 1200;
    height_test "height test" "random"
      (init_state ("one", "normal"))
      60 600;
    (*State module tests *)
    init_state_test "init_state test" ("one", "normal") some_state;
    get_state_tests "get_player" some_state get_player [ player_one ];
    get_state_tests "get_player two" some_state_2_player get_player
      [ player_one; player_two ];
    get_state_tests "get_tile_list" some_state get_tile_list
      (get_room_by_id "random" some_state).map_tile_list;
    get_state_tests "get_hole_list" some_room get_hole_list
      some_room.holes;
    get_state_tests "get_breaks" some_state get_breaks map2.init_breaks;
    get_state_tests "duplicate_state" some_state duplicate_state
      some_state;
    (*Command module tests*)
    compare_command "Go Left command: a" (parse "a") (Fst Left);
    compare_command "Go Left command: a (with spaces)" (parse " a ")
      (Fst Left);
    compare_command "Go Right command: d" (parse "d") (Fst Right);
    compare_command "Go Up command: w" (parse "w") (Fst Up);
    compare_command "Go Down command: s" (parse "s") (Fst Down);
    compare_command "Go Left command: j" (parse "j") (Snd Left);
    compare_command "Go Right command: l" (parse "l") (Snd Right);
    compare_command "Go Up command: i" (parse "i") (Snd Up);
    compare_command "Go Down command: k" (parse "k") (Snd Down);
    compare_command "Quit command: quit" (parse "quit") Quit;
    compare_command "Start command: start" (parse "start") Start;
    compare_command "Start command: start (with spaces)"
      (parse "   start   ") Start;
    compare_exception "Malformed Command 1 (check case sensitivity)"
      malformed1 Malformed;
    compare_exception "Malformed Command 2" malformed2 Malformed;
    compare_exception "Malformed Command 3" malformed3 Malformed;
    compare_exception "Malformed Command 4" malformed4 Malformed;
    compare_exception "Malformed Command 5" malformed5 Malformed;
    compare_exception "Empty Command 1" empty1 Empty;
    compare_exception "Empty Command 2" empty2 Empty;
    (* Move player tests *)
    move_test_player "Move up Fst player"
      (move state_mov_norm Up testing_map Fst)
      Fst (1, 2);
    move_test_player "Move right Fst player"
      (move state_mov_norm Right testing_map Fst)
      Fst (2, 1);
    move_test_illegal "Move down Fst player illegal"
      (move state_mov_norm Down testing_map Fst);
    move_test_illegal "Move left Fst player illegal"
      (move state_mov_norm Left testing_map Fst);
    move_test_player "Move up p2 Snd player"
      (move state_mov_norm_two_player Up testing_map Snd)
      Snd (3, 4);
    move_test_player "Move right p2 Snd player"
      (move state_mov_norm_two_player Right testing_map Snd)
      Snd (4, 3);
    move_test_player "Move down p2 Snd player"
      (move state_mov_norm_two_player Down testing_map Snd)
      Snd (3, 2);
    move_test_player "Move left p2 Snd player"
      (move state_mov_norm_two_player Left testing_map Snd)
      Snd (2, 3);
    (* Test win/lose/illegal conditions *)
    move_test_room_id "Move win rm_id = win"
      (rec_move (Legal state_mov_norm)
         [ Right; Right; Up; Down; Left; Left ]
         testing_map Fst)
      "win";
    move_test_illegal "Move Illegal"
      (rec_move (Legal state_mov_norm)
         [ Right; Right; Up; Left; Left; Down ]
         testing_map Fst);
    move_test_room_id "Move lose rm_id = lose"
      (rec_move (Legal state_mov_norm_limit) [ Up; Up; Down; Up ]
         testing_map Fst)
      "lose";
    move_test_room_id
      "Pushing a block on and then off a hole, the win \n\
      \      condition should fail to satisfy"
      (rec_move (Legal state_mov_norm)
         [ Right; Right; Up; Right; Up; Left; Down; Down; Left; Left ]
         testing_map Fst)
      "test";
    (* Move block tests *)
    move_test_block "Move down block at (3,2)"
      (move state_mov_norm_two_player Down testing_map Snd)
      { position = (3, 1); in_hole = false };
    move_test_block "Move left block at (2,2)"
      (move (p2_3_2 ()) Left testing_map Snd)
      { position = (1, 2); in_hole = false };
    move_test_illegal "Move block onto border"
      (move (p2_3_2 ()) Down testing_map Snd);
    move_test_block "Move double blocks, one at (2,2)"
      (move (p1_1_2 ()) Right testing_map Fst)
      { position = (3, 2); in_hole = false };
    move_test_block "Move double blocks, other at (3,2)"
      (move (p1_1_2 ()) Right testing_map Fst)
      { position = (4, 2); in_hole = false };
    move_test_block "Move block at (3,2) into hole"
      (move (p1_3_1 ()) Up testing_map Fst)
      { position = (3, 3); in_hole = true };
    (* Genmap tests *)
    genmap_test "map_init of size 2x1" init_map2x1 init_map2x1_exp;
    genmap_test "map_init of size 2x2" init_map2x2 init_map2x2_exp;
    genmap_test "map_init of size 3x3" init_map3x3 init_map3x3_exp;
    genmap_test "init_boundary of size 3x3" bound_map3x3
      bound_map3x3_exp;
  ]

let suite = "test suite for Final project" >::: List.flatten [ tests ]

let _ = run_test_tt_main suite
