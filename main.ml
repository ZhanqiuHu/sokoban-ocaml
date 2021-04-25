open Types
open State
open Command
open Gui

let read_key_new () =
  let s = Graphics.wait_next_event [ Graphics.Key_pressed ] in
  s.key

(** [change_state st direction] returns a new state given the current
    state [st] and the direction of movement [direction].

    If the movement is [Illegal] then this function returns the old
    state. *)
let change_state st direction room player_num =
  let state_of_result st = function Legal t -> t | Illegal -> st in
  state_of_result st (move st direction room player_num)

(** [prompt_command st] prompts the user for a command while in state
    [st] and returns the new state given the command. If the player
    enters an [Empty] or [Malformed] command then a message is printed
    to the screen and the old state is returned. *)
let prompt_command st : state =
  print_endline "Where do you go next?";
  try
    match parse (Char.escaped (read_key_new ())) with
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^\n   \n\n";
        exit 0
    | Fst direction ->
        change_state st direction
          (get_room_by_id st.current_room_id st)
          Fst
    | Snd direction ->
        change_state st direction
          (get_room_by_id st.current_room_id st)
          Snd
    | Start -> st
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n\
        \ You must type something. Type quit to stop the game or 'w', \
         's', 'a', 'd' to move the player. \n\n";
      st
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n\
        \ Type quit to\n\
        \   stop the game or 'w', 's', 'a', 'd' to move the  player. \n\n";
      st

(** [print_game st] prints a state [st] to the screen using its [map]
    attribute. *)
let print_game (st : state) =
  Graphics.auto_synchronize false;
  Gui.draw_rect_images st 60 60;
  Gui.draw_hole_list st 60 60;
  Gui.draw_block_list st 60 60;
  Gui.draw_break_list st 60 60;
  Graphics.auto_synchronize true

(** [play_game st boo] executes the game at state [st]. It prints the
    map to the screen and prompts the user for a command. [boo] checks
    if new state is the same as previous state. *)
let rec play_game st boo =
  if not boo then print_game st else Gui.draw_break_list st 60 60;
  Gui.draw_player st 60 60;

  let new_state = prompt_command st in
  play_game new_state (new_state = st)

(** [start_game s] starts the adventure is [s] is 'start'. If [s] is
    'quit' then the game stops. Otherwise, it prompts for a valid
    start/quit command. *)
let rec start_game s =
  try
    match parse s with
    | Start -> play_game init_state true
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^ \n\n";
        exit 0
    | _ -> (
        print_endline "Please type 'start' to begin the game.\n";
        print_string "> ";
        match Char.escaped (read_key_new ()) with
        | exception End_of_file -> ()
        | s -> start_game s)
  with
  | Empty -> (
      print_endline "Please type 'start' to begin the game.\n";
      print_string "> ";
      match Char.escaped (read_key_new ()) with
      | exception End_of_file -> ()
      | s -> start_game s)
  | Malformed -> (
      print_endline "Please type 'start' to begin the game.\n";
      print_string "> ";
      match Char.escaped (read_key_new ()) with
      | exception End_of_file -> ()
      | s -> start_game s)

(** [main ()] prompts the user to start the game, then starts it.

    This method will continually prompt the user until they enter
    "start".*)
exception End

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the\n     3110 Puzzle Game engine.\n";
  print_endline "Please type 'start' to\n     begin the game.\n";
  print_string "> ";
  Graphics.open_graph " 600x600";
  Gui.draw_rect_images init_state 60 60;
  Gui.draw_hole_list init_state 60 60;
  Gui.draw_block_list init_state 60 60;
  Gui.draw_break_list init_state 60 60;
  (* Uncomment the following code to test faltten_list and
     list_to_nested_list

     Gui.draw_rect_images (Gui.list_to_nested_list (Gui.flatten_list
     map) 10 10) 60 60; *)
  (* 20 x 27 *)
  match Char.escaped (read_key_new ()) with
  | exception End_of_file -> ()
  | s -> start_game "start"

(* Execute the game engine. *)
let () = main ()
