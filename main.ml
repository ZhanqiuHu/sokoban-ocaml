open Types
open State
open Command
open Gui

let quit_button =
  {
    position = (1 * 60, 10 * 60);
    width = 144;
    height = 60;
    image = "images/quit144x60.png";
    name = "quit";
  }

let reset_button =
  {
    position = (4 * 60, 10 * 60);
    width = 131;
    height = 60;
    image = "images/reset131x60.png";
    name = "reset";
  }

let back_button =
  {
    position = (7 * 60, 10 * 60);
    width = 63;
    height = 60;
    image = "images/left63x60.png";
    name = "back";
  }

let button_press
    (pos_condition : int * int -> bool)
    (s : Graphics.status) =
  let pos = Graphics.mouse_pos () in
  pos_condition pos && s.button

let pos_condition x_low x_high y_low y_high (pos : int * int) =
  fst pos >= x_low
  && fst pos <= x_high
  && snd pos >= y_low
  && snd pos <= y_high

let quit_cond =
  let pos = quit_button.position in
  let x_low = fst pos in
  let y_low = snd pos in
  let x_high = x_low + quit_button.width in
  let y_high = y_low + quit_button.height in
  pos_condition x_low x_high y_low y_high

let reset_cond =
  let pos = reset_button.position in
  let x_low = fst pos in
  let y_low = snd pos in
  let x_high = x_low + reset_button.width in
  let y_high = y_low + reset_button.height in
  pos_condition x_low x_high y_low y_high

let back_cond =
  let pos = back_button.position in
  let x_low = fst pos in
  let y_low = snd pos in
  let x_high = x_low + back_button.width in
  let y_high = y_low + back_button.height in
  pos_condition x_low x_high y_low y_high

let read_key_button () =
  let s = Graphics.wait_next_event [ Key_pressed ] in
  let key_char = s.key in
  Char.escaped key_char

let read_key_button () =
  let s = Graphics.wait_next_event [ Key_pressed; Button_down ] in
  if button_press quit_cond s then "quit"
  else if button_press reset_cond s then "start"
  else if button_press back_cond s then "back"
  else
    let key_char = s.key in
    Char.escaped key_char

let update_back history =
  if history.num_steps >= 1 then (
    history.num_steps <- history.num_steps - 1;
    history.state_list <- List.tl history.state_list)
  else ();
  List.hd history.state_list

let update_history st history =
  if
    history.num_steps = 0
    || List.hd history.state_list <> st
    || history.state_list = []
  then (
    history.num_steps <- history.num_steps + 1;
    history.state_list <- st :: history.state_list)
  else ()

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
let prompt_command (st : state) history =
  print_endline "valid...";
  try
    match parse (read_key_button ()) with
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
    | Start -> State.initialize_state init_state 1
    | Back -> update_back history
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n\
        \ You must type something. Close the window to stop the game \
         or 'w', 's', 'a', 'd' to move player 1 or 'i', 'k', 'j', 'l' \
         to move player 2. \n\n";
      st
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n\
        \ Close the window to stop the game or 'w', 's', 'a', 'd' to \
         move player 1 or 'i', 'k', 'j', 'l' to move player 2. \n\n";
      st

(** [print_win st] prints the win message to the screen. *)
let print_win st =
  Graphics.set_color Graphics.black;
  let room = get_room_by_id "win" st in
  Graphics.moveto (room.height / 2 * 60) (room.width / 2 * 60);
  Graphics.draw_string "YOU WIN!"

(** [print_game st] prints a state [st] to the screen using its [map]
    attribute. *)
let print_game (st : state) =
  Graphics.auto_synchronize false;
  Gui.draw_rect_images st 60 60;
  Gui.draw_hole_list st 60 60;
  Gui.draw_block_list st 60 60;
  Gui.draw_button quit_button;
  Gui.draw_button reset_button;
  Gui.draw_button back_button;
  Gui.draw_break_list st 60 60;
  if st.current_room_id = "win" then print_win st;
  Graphics.auto_synchronize true

(** [play_game st is_updated history] executes the game at state [st].
    It prints the map to the screen and prompts the user for a command.
    [boo] checks if new state is the same as previous state. *)
let rec play_game st is_updated history =
  if not is_updated then print_game st else Gui.draw_break_list st 60 60;
  Gui.draw_player st 60 60;
  update_history (duplicate_state st) history;

  let new_state = prompt_command st history in
  play_game new_state (new_state = st) history

(** [start_game s] starts the adventure is [s] is 'start'. If [s] is
    'quit' then the game stops. Otherwise, it prompts for a valid
    start/quit command. *)
let rec start_game s history =
  try
    match parse s with
    | Start -> play_game init_state true history
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^ \n\n";
        exit 0
    | _ -> (
        print_endline "Please press any key to begin the game.\n";
        print_string "> ";
        match read_key_button () with
        | exception End_of_file -> ()
        | s -> start_game s history)
  with
  | Empty -> (
      print_endline "Please press any key to begin the game.\n";
      print_string "> ";
      match read_key_button () with
      | exception End_of_file -> ()
      | s -> start_game s history)
  | Malformed -> (
      print_endline "Please press any key to begin the game.\n";
      print_string "> ";
      match read_key_button () with
      | exception End_of_file -> ()
      | s -> start_game s history)

(** Prints the start game instructions/message to the screen. *)
let print_start st =
  Graphics.set_color Graphics.black;
  let room = get_room_by_id st.current_room_id st in
  Graphics.moveto (room.height / 2 * 60) (room.width / 2 * 60);
  Graphics.draw_string "Press any key to start the game."

(* let open_graph w h = Stdlib.print_string "open"; match Sys.os_type
   with | "Win32" -> Stdlib.print_string "window"; Graphics.open_graph
   ("localhost:0.0 " ^ string_of_int w ^ "x" ^ string_of_int h) | "Unix"
   | "Cygwin" | "MacOS" -> Stdlib.print_string ("other" ^ Sys.os_type);
   Graphics.open_graph (" " ^ string_of_int w ^ "x" ^ string_of_int h) |
   _ -> invalid_arg ("Graphics: unknown OS type: " ^ Sys.os_type) *)

let open_graph map_w map_h =
  Graphics.open_graph
    (" " ^ string_of_int map_w ^ "x" ^ string_of_int map_h)

(* For Windows users: Graphics.open_graph ("localhost:0.0 " ^
   (string_of_int map_w) ^ "x" ^ (string_of_int map_h)) *)

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the\n 3110 Puzzle Game engine.\n";
  print_endline "Please press any key to\n begin the game.\n";
  print_string "> ";
  let init_room =
    State.get_room_by_id init_state.current_room_id init_state
  in
  open_graph (init_room.width * 60) ((init_room.height + 1) * 60);
  quit_button.position <-
    (fst quit_button.position, init_room.height * 60);
  reset_button.position <-
    (fst reset_button.position, init_room.height * 60);
  back_button.position <-
    (fst back_button.position, init_room.height * 60);

  Gui.draw_rect_images init_state 60 60;
  Gui.draw_hole_list init_state 60 60;
  Gui.draw_block_list init_state 60 60;
  Gui.draw_break_list init_state 60 60;
  Gui.draw_button quit_button;
  Gui.draw_button reset_button;
  Gui.draw_button back_button;
  print_start init_state;
  let state_history =
    { state_list = [ duplicate_state init_state ]; num_steps = 0 }
  in

  (* Uncomment the following code to test flatten_list and
     list_to_nested_list

     Gui.draw_rect_images (Gui.list_to_nested_list (Gui.flatten_list
     map) 10 10) 60 60; *)
  (* 20 x 27 *)
  match read_key_button () with
  | exception End_of_file -> ()
  | "quit" -> start_game "quit" state_history
  | _ -> start_game "start" state_history

(* Execute the game engine. *)

let () = main ()
