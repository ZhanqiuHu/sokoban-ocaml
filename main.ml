open Types
open State
open Command
open Gui

(** The width of a map. *)
let map_w = 10

(** The height of a map. *)
let map_h = 10

(** The pixel size of a tile. *)
let tile_size = 60

let pause_page =
  Genmap.(
    map_to_list
      (map_init map_w map_h { position = (0, 0); ttype = Normal }))

let start_page =
  Genmap.(
    map_to_list
      (map_init map_w map_h { position = (0, 0); ttype = Normal }))

let quit_button =
  {
    position = (1 * tile_size, map_h * tile_size);
    width = 144;
    height = 60;
    image = "images/quit144x60.png";
    name = "quit";
    enable = false;
  }

let reset_button =
  {
    position = (4 * tile_size, map_h * tile_size);
    width = 131;
    height = 60;
    image = "images/reset131x60.png";
    name = "reset";
    enable = false;
  }

let back_button =
  {
    position = (7 * tile_size, 10 * tile_size);
    width = 63;
    height = 60;
    image = "images/left63x60.png";
    name = "back";
    enable = false;
  }

let pause_button =
  {
    position = (9 * tile_size, 10 * tile_size);
    width = 63;
    height = 60;
    image = "images/pause60x60.png";
    name = "pause";
    enable = false;
  }

let start_button =
  {
    position = (7 * tile_size, 1 * tile_size);
    width = 166;
    height = 60;
    image = "images/start166x60.png";
    name = "start";
    enable = false;
  }

let return_button =
  {
    position = (3 * tile_size, 5 * tile_size);
    width = 191;
    height = 60;
    image = "images/return191x60.png";
    name = "return";
    enable = false;
  }

let one_player_select =
  {
    position = (1 * tile_size, 8 * tile_size);
    width = 133;
    height = 60;
    image_select = "images/one_select133x60.png";
    image_deselect = "images/one_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

let two_player_select =
  {
    position = (1 * tile_size, 6 * tile_size);
    width = 133;
    height = 60;
    image_select = "images/two_select133x60.png";
    image_deselect = "images/two_unselect133x60.png";
    name = "start";
    enable = true;
    select = true;
    exclusives = [];
  }

let slide_select =
  {
    position = (5 * tile_size, 8 * tile_size);
    width = 133;
    height = 60;
    image_select = "images/sliding_select133x60.png";
    image_deselect = "images/sliding_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

let time_select =
  {
    position = (5 * tile_size, 6 * tile_size);
    width = 133;
    height = 60;
    image_select = "images/time_select133x60.png";
    image_deselect = "images/time_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

let normal_select =
  {
    position = (5 * tile_size, 4 * tile_size);
    width = 133;
    height = 60;
    image_select = "images/normal_select133x60.png";
    image_deselect = "images/normal_unselect133x60.png";
    name = "start";
    enable = true;
    select = true;
    exclusives = [];
  }

let select_list =
  [
    one_player_select;
    two_player_select;
    slide_select;
    time_select;
    normal_select;
  ]

let assign_exclusives =
  one_player_select.exclusives <- [ two_player_select ];
  two_player_select.exclusives <- [ one_player_select ];
  slide_select.exclusives <- [ time_select; normal_select ];
  time_select.exclusives <- [ slide_select; normal_select ];
  normal_select.exclusives <- [ time_select; slide_select ]

let enable_button (button : button) = button.enable <- true

let disable_button (button : button) = button.enable <- false

(** returns true if pos is inside x and y range *)
let pos_condition x_low x_high y_low y_high (pos : int * int) =
  fst pos >= x_low
  && fst pos <= x_high
  && snd pos >= y_low
  && snd pos <= y_high

(** returns true if button is pressed *)
let button_cond
    (butt_pos : int * int)
    (butt_dim : int * int)
    (enable : bool)
    (mouse_pos : int * int) =
  let x_low = fst butt_pos in
  let y_low = snd butt_pos in
  let x_high = x_low + fst butt_dim in
  let y_high = y_low + snd butt_dim in
  pos_condition x_low x_high y_low y_high mouse_pos && enable

let button_press (button : button) (s : Graphics.status) : bool =
  let mouse_pos = Graphics.mouse_pos () in
  button_cond button.position
    (button.width, button.height)
    button.enable mouse_pos

let select_press (sel : select) (s : Graphics.status) : bool =
  let mouse_pos = Graphics.mouse_pos () in
  button_cond sel.position (sel.width, sel.height) sel.enable mouse_pos

(** Updates the select field of [sel] to true and unselects all other
    other select buttons that are exclusive to it. *)
let update_select (sel : select) =
  sel.select <- not sel.select;
  if sel.select then
    let rec update lst =
      match lst with
      | [] -> ()
      | h :: t ->
          h.select <- false;
          update t
    in
    update sel.exclusives

(** Returns true if a select button has been pushed. If so, then it
    updates the select field of that button to true and unselects all
    other other select buttons that are exclusive to it. False
    otherwise. *)
let select_button (s : Graphics.status) =
  if select_press one_player_select s then (
    update_select one_player_select;
    true)
  else if select_press two_player_select s then (
    update_select two_player_select;
    true)
  else if select_press slide_select s then (
    update_select slide_select;
    true)
  else if select_press time_select s then (
    update_select time_select;
    true)
  else if select_press normal_select s then (
    update_select normal_select;
    true)
  else false

(** returns the command in string when a key or a button is pressed *)
let read_key_button () =
  let s = Graphics.wait_next_event [ Key_pressed; Button_down ] in
  if button_press pause_button s && pause_button.name = "pause" then
    "pause"
  else if button_press pause_button s && pause_button.name = "resume"
  then "resume"
  else if button_press quit_button s then "quit"
  else if button_press start_button s then "start"
  else if button_press reset_button s then "start"
  else if button_press back_button s then "back"
  else if button_press return_button s then "return"
  else if select_button s then "select"
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

(** [print_win st] prints the win message to the screen. *)
let print_win st =
  Graphics.set_color Graphics.black;
  let room = get_room_by_id "win" st in
  Graphics.moveto
    (room.height / 2 * tile_size)
    (room.width / 2 * tile_size);
  Graphics.draw_string "YOU WIN!"

(** [prompt_command st] prompts the user for a command while in state
    [st] and returns the new state given the command. If the player
    enters an [Empty] or [Malformed] command then a message is printed
    to the screen and the old state is returned. *)
let prompt_command_active (st : state) history (command : command) =
  print_endline "valid...";
  match command with
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
  | Pause ->
      st.active <- false;
      st
  | Resume ->
      st.active <- true;
      st

let prompt_command (st : state) (history : history) =
  try
    let s = read_key_button () in
    if st.active && s <> "pause" then (
      pause_button.name <- "pause";
      let command = parse s in
      prompt_command_active st history command
      (* else if s = "return" && not st.active then main () *))
    else if s = "resume" && not st.active then (
      pause_button.name <- "pause";
      pause_button.image <- "images/pause60x60.png";
      prompt_command_active st history Resume)
    else (
      pause_button.name <- "resume";
      pause_button.image <- "images/resume60x60.png";
      prompt_command_active st history Pause)
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

(** [print_game st] prints a state [st] to the screen using its [map]
    attribute. *)
let print_game (st : state) =
  Graphics.auto_synchronize false;
  Graphics.clear_graph ();
  Gui.draw_rect_images st tile_size tile_size;
  Gui.draw_hole_list st tile_size tile_size;
  Gui.draw_block_list st tile_size tile_size;
  Gui.draw_button quit_button;
  Gui.draw_button reset_button;
  Gui.draw_button back_button;
  Gui.draw_button pause_button;
  Gui.draw_break_list st tile_size tile_size;
  if st.current_room_id = "win" then print_win st;
  Graphics.auto_synchronize true

let draw_new_active_state st is_updated history =
  print_game st;
  Gui.draw_break_list st tile_size tile_size;
  Gui.draw_player st tile_size tile_size

(** [play_game st is_updated history] executes the game at state [st].
    It prints the map to the screen and prompts the user for a command.
    [boo] checks if new state is the same as previous state. *)
let rec play_game st is_updated history =
  if st.active then (
    draw_new_active_state st is_updated history;
    update_history (duplicate_state st) history)
  else (
    Graphics.auto_synchronize false;
    Graphics.clear_graph ();
    draw_tiles tile_size tile_size pause_page;
    draw_button return_button;
    enable_button return_button;
    draw_button pause_button;
    Graphics.auto_synchronize true);
  let new_state = prompt_command st history in
  play_game new_state (new_state = st) history

(** [start_game s] starts the adventure is [s] is 'start'. If [s] is
    'quit' then the game stops. Otherwise, it prompts for a valid
    start/quit command. *)
let rec start_game s history =
  try
    match parse s with
    | Start ->
        disable_button start_button;
        enable_button pause_button;
        enable_button quit_button;
        enable_button reset_button;
        enable_button back_button;
        play_game init_state true history
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
  Graphics.moveto
    (room.height / 2 * tile_size)
    (room.width / 2 * tile_size);
  Graphics.draw_string "Press any key to start the game."

let open_graph map_w map_h =
  Graphics.open_graph
    ("localhost:0.0 " ^ string_of_int map_w ^ "x" ^ string_of_int map_h)

(* For Windows users: Graphics.open_graph ("localhost:0.0 " ^
   (string_of_int map_w) ^ "x" ^ (string_of_int map_h)) *)

let initialize_window init_room =
  open_graph
    (init_room.width * tile_size)
    ((init_room.height + 1) * tile_size);
  quit_button.position <-
    (fst quit_button.position, init_room.height * tile_size);
  reset_button.position <-
    (fst reset_button.position, init_room.height * tile_size);
  back_button.position <-
    (fst back_button.position, init_room.height * tile_size);
  Graphics.auto_synchronize false;
  Gui.draw_tiles tile_size tile_size start_page;
  Gui.draw_button start_button;
  Gui.draw_select select_list;
  Graphics.auto_synchronize true;
  enable_button start_button;
  assign_exclusives

let wait_button state_history =
  let rec wait_start () =
    match read_key_button () with
    | exception End_of_file -> ()
    | "quit" -> start_game "quit" state_history
    | "start" ->
        let check_select : bool =
          (one_player_select.select || two_player_select.select)
          && (normal_select.select || slide_select.select
            || time_select.select)
        in
        if check_select then start_game "start" state_history
        else wait_start ()
    | "select" ->
        Gui.draw_select select_list;
        wait_start ()
    | _ -> wait_start ()
  in
  wait_start ()

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the\n 3110 Puzzle Game engine.\n";
  print_endline "Please press any key to\n begin the game.\n";
  print_string "> ";
  let init_room =
    State.get_room_by_id init_state.current_room_id init_state
  in
  initialize_window init_room;
  (* print_start init_state; *)
  let state_history =
    { state_list = [ duplicate_state init_state ]; num_steps = 0 }
  in
  wait_button state_history

(* Execute the game engine. *)

let () = main ()
