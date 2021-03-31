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

let suite =
  "test suite for project"
  >::: List.flatten [ get_tile_list_tests; move_tests ]

let _ = run_test_tt_main suite
