(* open OUnit2 open Map open Map_test open Types open State open Command

   let get_tile_list_test name st expected_tile_lst = name >:: fun ctxt
   -> assert_equal expected_tile_lst (get_tile_list st)

   let move_test name st dir expected_result = name >:: fun ctxt -> let
   result = move st dir in match result with | Legal t -> assert_equal
   expected_result t | Illegal -> raise (Failure "created result is
   Illegal")

   let move_test_fail name st dir = name >:: fun ctxt -> assert_equal
   Illegal (move st dir)

   let get_tile_list_tests = [ get_tile_list_test "getting the tile list
   of the starting room" init_state map1.map_tile_list; ]

   let state_step_right = { current_room_id = "beginning"; all_rooms = [
   map1_step_right ]; player_pos = (2, 1); }

   let state_step_down = { current_room_id = "beginning"; all_rooms = [
   map1_step_down ]; player_pos = (1, 2); }

   let move_tests = [ move_test "init (1, 1) move right legal"
   init_state Right state_step_right; move_test "init (1, 1) move down
   legal" init_state Down state_step_down; move_test_fail "init (1, 1)
   move left reach boundary" init_state Left; move_test_fail "init (1,
   1) move up reach boundary" init_state Up; ]

   let compare_command (name : string) (result : command)
   (expected_output : command) : test = name >:: fun _ -> assert_equal
   expected_output result

   (** [compare_exception name result expected_except] is a test of name
   [name] that compares two exceptions [result] and [expected_except].
   *) let compare_exception (name : string) result (expected_except :
   exn) : test = name >:: fun _ -> assert_raises expected_except result

   let malformed1 () = parse "Start"

   let malformed2 () = parse "quiT"

   let malformed3 () = parse "hello world"

   let malformed4 () = parse "quit 1"

   let malformed5 () = parse "start 1234"

   let empty1 () = parse ""

   let empty2 () = parse " "

   let command_tests = [ compare_command "Go Left command: a" (parse
   "a") (Go Left); compare_command "Go Left command: a (with spaces)"
   (parse " a ") (Go Left); compare_command "Go Right command: d" (parse
   "d") (Go Right); compare_command "Go Up command: w" (parse "w") (Go
   Up); compare_command "Go Down command: s" (parse "s") (Go Down);
   compare_command "Quit command: quit" (parse "quit") Quit;
   compare_command "Start command: start" (parse "start") Start;
   compare_command "Start command: start (with spaces)" (parse " start
   ") Start; compare_exception "Malformed Command 1 (check case
   sensitivity)" malformed1 Malformed; compare_exception "Malformed
   Command 2" malformed2 Malformed; compare_exception "Malformed Command
   3" malformed3 Malformed; compare_exception "Malformed Command 4"
   malformed4 Malformed; compare_exception "Malformed Command 5"
   malformed5 Malformed; compare_exception "Empty Command 1" empty1
   Empty; compare_exception "Empty Command 2" empty2 Empty; ]

   let suite = "test suite for project" >::: List.flatten [
   get_tile_list_tests; move_tests; command_tests ]

   let _ = run_test_tt_main suite *)
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

let tests =
  [
    parse_test "parse test"
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
    parse_test "parse test"
      "I somehow have to write something that is more than a hundred  \
       characters and I certainly hope this is enough cause if it's \
       not I'm gonna be super annoyed."
      600
      [
        "I somehow have to write something that is more than a \
         hundred  characters and I certainly";
        "hope this is enough cause if it's not I'm gonna be super \
         annoyed.";
      ];
    width_test "width test" "random"
      (init_state ("one", "normal"))
      60 600;
    height_test "height test" "random"
      (init_state ("one", "normal"))
      60 600;
  ]

let suite = "test suite for Final project" >::: List.flatten [ tests ]

let _ = run_test_tt_main suite
