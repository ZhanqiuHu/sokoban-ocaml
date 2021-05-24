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

(* let init_state_test name input expected_value = name >:: fun ctxt ->
   assert_equal expected_value (parse_dialogue dialogue [] width)
   ~printer:(pp_list pp_string) *)
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

let room_list_1 = [ map2; win; lose ]

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

let state_mov_norm_two_player =
  state_gen "test" Normal room_list_2 testing_map player_list_2

let init_state_test name sel_list expected_value =
  name >:: fun ctxt -> assert_equal expected_value (init_state sel_list)

let get_state_tests name state func expected_value =
  name >:: fun ctxt -> assert_equal expected_value (func state)

let some_room = get_room_by_id "random" some_state

let p2_3_3 = move state_mov_norm_two_player Down testing_map Snd

let p2_3_2 = move state_mov_norm_two_player Down testing_map Snd

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

(* let rec rec_move *)

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
      assert_equal (List.mem state.blocks expected_result) true

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
    move_test_player "Move up"
      (move state_mov_norm Up testing_map Fst)
      Fst (1, 2);
    move_test_player "Move right"
      (move state_mov_norm Right testing_map Fst)
      Fst (2, 1);
    move_test_illegal "Move down"
      (move state_mov_norm Down testing_map Fst);
    move_test_illegal "Move left"
      (move state_mov_norm Left testing_map Fst);
    move_test_player "Move up p2"
      (move state_mov_norm_two_player Up testing_map Snd)
      Snd (3, 4);
    move_test_player "Move right p2"
      (move state_mov_norm_two_player Right testing_map Snd)
      Snd (4, 3);
    move_test_player "Move down p2"
      (move state_mov_norm_two_player Down testing_map Snd)
      Snd (3, 2);
    move_test_player "Move left p2"
      (move state_mov_norm_two_player Left testing_map Snd)
      Snd (2, 3);
    move_test_room_id "Move win rm_id = win";
    (* Move block tests *)
    move_test_block "Move down block at (3,2)" ()
      { pos = (3, 1); in_hole = false };
  ]

let suite = "test suite for Final project" >::: List.flatten [ tests ]

let _ = run_test_tt_main suite

(* Normal mode,one player, success, room_id = "win"; Normal mode,one
   player, pushed on and off, room_id != "win"; Normal mode,one player,
   cascading blocks, player pos does not change; Normal mode,two
   players, success, room_id = "win"; Normal mode,two player: player
   pushing each other; Sliding mode,one players; 4 Sliding mode,two
   players; 4 Sliding mode,two player: player pushing each other; Limit
   mode, one player, fail and room_id = "lose"; Limit mode, two players,
   fail and room_id = "lose"; *)
