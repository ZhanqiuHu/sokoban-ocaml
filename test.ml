open OUnit2
open Map
open Map_test
open Types
open State
open Command

let get_tile_list_test name st expected_tile_lst =
  name >:: fun ctxt -> assert_equal expected_tile_lst (get_tile_list st)

let move_test name st dir expected_result =
  name >:: fun ctxt ->
  let result = move st dir in
  match result with
  | Legal t -> assert_equal expected_result t
  | Illegal -> raise (Failure "created result is Illegal")

let move_test_fail name st dir =
  name >:: fun ctxt -> assert_equal Illegal (move st dir)

let get_tile_list_tests =
  [
    get_tile_list_test "getting the tile list of the starting room"
      init_state map1.map_tile_list;
  ]

let state_step_right =
  {
    current_room_id = "beginning";
    all_rooms = [ map1_step_right ];
    player_pos = (2, 1);
  }

let state_step_down =
  {
    current_room_id = "beginning";
    all_rooms = [ map1_step_down ];
    player_pos = (1, 2);
  }

let move_tests =
  [
    move_test "init (1, 1) move right legal" init_state Right
      state_step_right;
    move_test "init (1, 1) move down legal" init_state Down
      state_step_down;
    move_test_fail "init (1, 1) move left reach boundary" init_state
      Left;
    move_test_fail "init (1, 1) move up reach boundary" init_state Up;
  ]

let compare_command
    (name : string)
    (result : command)
    (expected_output : command) : test =
  name >:: fun _ -> assert_equal expected_output result

(** [compare_exception name result expected_except] is a test of name
    [name] that compares two exceptions [result] and [expected_except]. *)
let compare_exception (name : string) result (expected_except : exn) :
    test =
  name >:: fun _ -> assert_raises expected_except result

let malformed1 () = parse "Start"

let malformed2 () = parse "quiT"

let malformed3 () = parse "hello world"

let malformed4 () = parse "quit 1"

let malformed5 () = parse "start 1234"

let empty1 () = parse ""

let empty2 () = parse "     "

let command_tests =
  [
    compare_command "Go Left command: a" (parse "a") (Go Left);
    compare_command "Go Left command: a (with spaces)" (parse "  a   ")
      (Go Left);
    compare_command "Go Right command: d" (parse "d") (Go Right);
    compare_command "Go Up command: w" (parse "w") (Go Up);
    compare_command "Go Down command: s" (parse "s") (Go Down);
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
  ]

let suite =
  "test suite for project"
  >::: List.flatten [ get_tile_list_tests; move_tests; command_tests ]

let _ = run_test_tt_main suite
