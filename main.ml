open Types
open State
open Command
open Gui
open Text

(** [map_w] is the width of a map. *)
let map_w = 20

(**[map_h] is the height of a map. *)
let map_h = 10

(** [tile_size ] is the pixel size of a tile. *)
let tile_size = 60

(** [empty_page] is an empty map used for the background of the start
    and pause pages. *)
let empty_page =
  Genmap.(
    map_to_list
      (map_init map_w map_h { position = (0, 0); ttype = Normal }))

(** [quit_button] is the button for quitting the game. *)
let quit_button =
  {
    position = (1 * tile_size, map_h * tile_size);
    width = 144;
    height = 60;
    image = "images/quit144x60.png";
    name = "quit";
    enable = false;
  }

(** [reset_button] is the button for resetting the game. *)
let reset_button =
  {
    position = (4 * tile_size, map_h * tile_size);
    width = 131;
    height = 60;
    image = "images/reset131x60.png";
    name = "reset";
    enable = false;
  }

(** [back_button] is the button for undoing the previous step. *)
let back_button =
  {
    position = (7 * tile_size, map_h * tile_size);
    width = 63;
    height = 60;
    image = "images/left63x60.png";
    name = "back";
    enable = false;
  }

(** [pause_button] is the button for opening the pause menu. *)
let pause_button =
  {
    position = ((map_w - 1) * tile_size, map_h * tile_size);
    width = 63;
    height = 60;
    image = "images/pause60x60.png";
    name = "pause";
    enable = false;
  }

(** [start_button] is the button for starting the game. *)
let start_button =
  {
    position = (((map_w / 2) + 3) * tile_size, 2 * tile_size);
    width = 166;
    height = 60;
    image = "images/start166x60.png";
    name = "start";
    enable = true;
  }

(** [return_button] is the button for returning to the start menu to
    select the mode. It is accessible through the pause menu. *)
let return_button =
  {
    position = (((map_w / 2) - 1) * tile_size, map_h / 2 * tile_size);
    width = 191;
    height = 60;
    image = "images/return191x60.png";
    name = "return";
    enable = false;
  }

(** [one_player_select] is the select button for playing in one player
    mode. It is exclusive to [two_player_select]. *)
let one_player_select =
  {
    position =
      (((map_w / 2) - 3) * tile_size, ((map_h / 2) + 3) * tile_size);
    width = 133;
    height = 60;
    image_select = "images/one_select133x60.png";
    image_deselect = "images/one_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

(** [two_player_select] is the select button for playing in two player
    mode. It is exclusive to [one_player_select]. *)
let two_player_select =
  {
    position =
      (((map_w / 2) - 3) * tile_size, ((map_h / 2) + 1) * tile_size);
    width = 133;
    height = 60;
    image_select = "images/two_select133x60.png";
    image_deselect = "images/two_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

(** [slide_select] is the select button for playing in sliding blocks
    mode. It is exclusive to [step_select] and [normal_select]. *)
let slide_select =
  {
    position =
      (((map_w / 2) + 1) * tile_size, ((map_h / 2) + 3) * tile_size);
    width = 133;
    height = 60;
    image_select = "images/sliding_select133x60.png";
    image_deselect = "images/sliding_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

(** [step_select] is the select button for playing in step limitting
    mode. It is exclusive to [slide_select] and [normal_select]. *)
let step_select =
  {
    position =
      (((map_w / 2) + 1) * tile_size, ((map_h / 2) + 1) * tile_size);
    width = 133;
    height = 60;
    image_select = "images/steplim_select133x60.png";
    image_deselect = "images/steplim_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

(** [normal_select] is the select button for playing in normal mode. It
    is exclusive to [step_select] and [slide_select]. *)
let normal_select =
  {
    position =
      (((map_w / 2) + 1) * tile_size, ((map_h / 2) - 1) * tile_size);
    width = 133;
    height = 60;
    image_select = "images/normal_select133x60.png";
    image_deselect = "images/normal_unselect133x60.png";
    name = "start";
    enable = true;
    select = false;
    exclusives = [];
  }

(** [select_list] is the list of all select buttons. *)
let select_list =
  [
    one_player_select;
    two_player_select;
    slide_select;
    step_select;
    normal_select;
  ]

(** [assign_exclusives] assigns to each select button the selects that
    it is exclusive to. *)
let assign_exclusives =
  one_player_select.exclusives <- [ two_player_select ];
  two_player_select.exclusives <- [ one_player_select ];
  slide_select.exclusives <- [ step_select; normal_select ];
  step_select.exclusives <- [ slide_select; normal_select ];
  normal_select.exclusives <- [ step_select; slide_select ]

(** [enable_button] sets enable of [button] to be true. *)
let enable_button (button : button) = button.enable <- true

(** [disable_button] sets enable of [button] to be false. *)
let disable_button (button : button) = button.enable <- false

(** [pos_condition x_low x_high y_low y_high pos] returns true if [pos]
    is inside [\[x_low, x_high\]] and [\[y_low, y_high\]]. *)
let pos_condition x_low x_high y_low y_high (pos : int * int) =
  fst pos >= x_low
  && fst pos <= x_high
  && snd pos >= y_low
  && snd pos <= y_high

(** [button_cond butt_pos butt_dim enable mouse_pos] returns true if the
    button it references has been pressed and it is enabled. *)
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

(** [button_press button s] returns true if [button] has been pressed
    given the event [s] in this frame and false otherwise. *)
let button_press (button : button) (s : Graphics.status) : bool =
  let mouse_pos = Graphics.mouse_pos () in
  button_cond button.position
    (button.width, button.height)
    button.enable mouse_pos

(** [select_press sel s] returns true if select button [sel] has been
    pressed given the event [s] in this frame and false otherwise. *)
let select_press (sel : select) (s : Graphics.status) : bool =
  let mouse_pos = Graphics.mouse_pos () in
  button_cond sel.position (sel.width, sel.height) sel.enable mouse_pos

(** [update_select sel] updates the [select] field of [sel] to true and
    unselects all other other select buttons that are exclusive to it. *)
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

(** [select_button s] returns true if a select button has been pushed
    given the event [s]. If so, then it returns true and updates the
    select field of that button to true and unselects all other other
    select buttons that are exclusive to it. False otherwise. *)
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
  else if select_press step_select s then (
    update_select step_select;
    true)
  else if select_press normal_select s then (
    update_select normal_select;
    true)
  else false

(** [state_history] is the empty history. *)
let state_history = { state_list = []; num_steps = 0 }

(** [open_graph map_w map_h] opens a graphics window with dimensions
    [map_w]x[map_h]. For Windows users: Graphics.open_graph
    ("localhost:0.0 " ^ (string_of_int map_w) ^ "x" ^ (string_of_int
    map_h))*)
let open_graph map_w map_h =
  Graphics.open_graph
    (" " ^ string_of_int map_w ^ "x" ^ string_of_int map_h)

(** [initialize_window ()] draws the start menu to the screen and
    enables the select buttons and start button. It also assigns
    exclusive select buttons i.e. one player vs two player, etc. *)
let initialize_window () =
  open_graph (map_w * tile_size) ((map_h + 1) * tile_size);
  quit_button.position <- (fst quit_button.position, map_h * tile_size);
  reset_button.position <- (fst reset_button.position, map_h * tile_size);
  back_button.position <- (fst back_button.position, map_h * tile_size);
  Graphics.auto_synchronize false;
  Gui.draw_tiles tile_size tile_size empty_page;
  Gui.draw_button start_button;
  Gui.draw_select select_list;
  Graphics.auto_synchronize true;
  enable_button start_button;
  assign_exclusives

(** [read_key_button ()] returns the command in string when a key or a
    button is pressed *)
let read_key_button () =
  let s = Graphics.wait_next_event [ Key_pressed; Button_down ] in
  if
    button_press pause_button s
    && pause_button.name = "pause"
    && s.button
  then "pause"
  else if
    button_press pause_button s
    && pause_button.name = "resume"
    && s.button
  then "resume"
  else if button_press quit_button s && s.button then "quit"
  else if button_press start_button s && s.button then "start"
  else if button_press reset_button s && s.button then "start"
  else if button_press back_button s && s.button then "back"
  else if button_press return_button s && s.button then "return"
  else if select_button s && s.button then "select"
  else
    let key_char = s.key in
    Char.escaped key_char

(** [update_back history] returns the state corresponding to taking one
    step if there are steps left or the state with 0 steps left. *)
let update_back history =
  if state_history.num_steps >= 1 then (
    state_history.num_steps <- state_history.num_steps - 1;
    state_history.state_list <- List.tl state_history.state_list)
  else ();
  List.hd state_history.state_list

(** [update_history st history] updates the current state history
    according to the number of steps left after this turn. *)
let update_history st history =
  if
    state_history.num_steps = 0
    || List.hd state_history.state_list <> st
    || state_history.state_list = []
  then (
    state_history.num_steps <- state_history.num_steps + 1;
    state_history.state_list <- st :: state_history.state_list)
  else ()

(** [change_state st direction] returns a new state given the current
    state [st] and the direction of movement [direction]. If the
    movement is [Illegal] then this function returns the old state. *)
let change_state st direction room player_num =
  let state_of_result st = function Legal t -> t | Illegal -> st in
  state_of_result st (move st direction room player_num)

(** [selected ())] returns a tuple containing (player mode, block mode)
    depending on which selects are chosen. *)
let selected () =
  let player_mode = if one_player_select.select then "one" else "two" in
  let mode =
    if normal_select.select then "normal"
    else if slide_select.select then "sliding"
    else "limited"
  in
  (player_mode, mode)

(** [check_select ()] returns true if a valid selection of select
    buttons has been made. *)
let check_select () =
  (one_player_select.select || two_player_select.select)
  && (normal_select.select || slide_select.select || step_select.select)

(** [start_game_buttons ()] disables and enables certains buttons to
    allow for game play. *)
let start_game_buttons () =
  disable_button start_button;
  enable_button pause_button;
  enable_button quit_button;
  enable_button reset_button;
  enable_button back_button

(** [wait_restart st] waits for the player to finish selecting and
    presses the [start_button] to start the game after returning to the
    start menu in the pause menu. *)
let rec wait_restart (st : state) =
  match read_key_button () with
  | exception End_of_file -> wait_restart st
  | "start" ->
      let valid_select = check_select () in
      if valid_select then (
        start_game_buttons ();
        let st_new = duplicate_state (init_state (selected ())) in
        state_history.state_list <- [ duplicate_state st_new ];
        state_history.num_steps <- 0;
        st_new)
      else wait_restart st
  | "select" ->
      Gui.draw_select select_list;
      wait_restart st
  | _ -> wait_restart st

(** [handle_return st] reconfigures the buttons and selects for when the
    player wants to return to the start menu. *)
let handle_return st =
  start_game_buttons ();
  enable_button start_button;
  Graphics.auto_synchronize false;
  Graphics.clear_graph ();
  Gui.draw_tiles tile_size tile_size empty_page;
  Gui.draw_button start_button;
  Gui.draw_select select_list;
  Graphics.auto_synchronize true;
  enable_button start_button;
  assign_exclusives;
  st.active <- true;
  wait_restart st

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
  | Fst dir ->
      change_state st dir (get_room_by_id st.current_room_id st) Fst
  | Snd dir ->
      change_state st dir (get_room_by_id st.current_room_id st) Snd
  | Start -> init_state (selected ())
  | Back -> update_back history
  | Pause ->
      st.active <- false;
      st
  | Resume ->
      st.active <- true;
      st
  | Return -> handle_return st

(** [print_help ()] prints the help message to the terminal. *)
let print_help () =
  ANSITerminal.print_string [ ANSITerminal.cyan ]
    "\n\
    \ Close the window to stop the game or 'w', 's', 'a', 'd' to move \
     player 1 or 'i', 'k', 'j', 'l' to move player 2. \n\n"

(** [relabel_pause ()] reconfigures the pause button to be pause. *)
let relabel_pause () =
  pause_button.name <- "pause";
  pause_button.image <- "images/pause60x60.png"

(** [relabel_resume ()] reconfigures the pause button to be resume. *)
let relabel_resume () =
  pause_button.name <- "resume";
  pause_button.image <- "images/resume60x60.png"

(** [prompt_command st history] waits for a command, keyboard push or
    mouse click, to continue the game. *)
let prompt_command (st : state) (history : history) =
  try
    let s = read_key_button () in
    if st.active && s <> "pause" then (
      pause_button.name <- "pause";
      prompt_command_active st history (parse s))
    else if s = "return" && not st.active then (
      relabel_pause ();
      disable_button pause_button;
      prompt_command_active st history Return)
    else if s = "resume" && not st.active then (
      relabel_pause ();
      disable_button return_button;
      prompt_command_active st history Resume)
    else (
      relabel_resume ();
      enable_button return_button;
      prompt_command_active st history Pause)
  with Empty | Malformed ->
    print_help ();
    st

(** [print_win st] prints the win message to the screen. *)
let print_win st =
  let width = Text.get_hw st.current_room_id st tile_size in
  Text.draw_box width "YOU WIN!"

(** [print_lose st] prints the lose message to the screen. *)
let print_lose st =
  let width = Text.get_hw st.current_room_id st tile_size in
  Text.draw_box width "YOU LOSE!"

(** [print_game st] prints a state [st] to the screen using its [map]
    attribute. *)
let print_game (st : state) =
  Gui.draw_rect_images st tile_size tile_size;
  Gui.draw_hole_list st tile_size tile_size;
  Gui.draw_block_list st tile_size tile_size;
  Gui.draw_button quit_button;
  Gui.draw_button reset_button;
  Gui.draw_button back_button;
  Gui.draw_button pause_button;
  Gui.draw_break_list st tile_size tile_size;
  if st.current_room_id = "win" then print_win st;
  if st.current_room_id = "lose" then print_lose st

(** [draw_new_active_state st is_updated history] draws the new state
    [st] to the screen. *)
let draw_new_active_state st is_updated history =
  Graphics.auto_synchronize false;
  Graphics.clear_graph ();
  print_game st;
  Gui.draw_break_list st tile_size tile_size;
  Gui.draw_player st tile_size tile_size;
  if
    st.mode = Limit
    && st.current_room_id <> "lose"
    && st.current_room_id <> "win"
  then
    draw_box
      (get_hw st.current_room_id st tile_size)
      ("Steps left: " ^ string_of_int st.steps_left);
  Graphics.auto_synchronize true

(** [play_game st is_updated history] executes the game at state [st].
    It prints the map to the screen and prompts the user for a command. *)
let rec play_game st is_updated history =
  if st.active then (
    draw_new_active_state st is_updated history;
    update_history (duplicate_state st) history)
  else (
    Graphics.auto_synchronize false;
    Graphics.clear_graph ();
    draw_tiles tile_size tile_size empty_page;
    draw_button return_button;
    enable_button return_button;
    draw_button pause_button;
    Graphics.auto_synchronize true);
  let new_state = prompt_command st history in
  play_game new_state (new_state = st) history

(** [prompt_start s history] repeatedly prompts the user to press a key
    to start the game. *)
let rec prompt_start s history =
  print_endline "Please press any key to begin the game.\n";
  print_string "> ";
  match read_key_button () with
  | exception End_of_file -> ()
  | s -> start_game s history

(** [start_game s history] starts the game if [s] is 'start'. If [s] is
    'quit' then the game stops. Otherwise, it prompts for a valid
    start/quit command. [history] represents the state history the
    player has gone through so far. *)
and start_game s history =
  try
    match parse s with
    | Start ->
        start_game_buttons ();
        play_game
          (duplicate_state (init_state (selected ())))
          true history
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^ \n\n";
        exit 0
    | _ -> prompt_start s history
  with Empty | Malformed -> prompt_start s history

(** Prints the start game instructions/message to the screen given the
    current state [st]. *)
let print_start st =
  Graphics.set_color Graphics.black;
  let room = get_room_by_id st.current_room_id st in
  Graphics.moveto
    (room.height / 2 * tile_size)
    (room.width / 2 * tile_size);
  Graphics.draw_string "Press any key to start the game."

(** [wait_button state history] waits to start the game which is
    signalled when the user makes a valid selection of modes and hits
    the start button. *)
let wait_button state_history =
  let rec wait_start () =
    match read_key_button () with
    | exception End_of_file -> ()
    | "quit" -> start_game "quit" state_history
    | "start" ->
        let valid_select = check_select () in
        if valid_select then (
          state_history.state_list <-
            [ duplicate_state (init_state (selected ())) ];
          state_history.num_steps <- 0;
          start_game "start" state_history)
        else wait_start ()
    | "select" ->
        Graphics.auto_synchronize false;
        Gui.draw_select select_list;
        Graphics.auto_synchronize true;
        wait_start ()
    | _ -> wait_start ()
  in
  wait_start ()

(** [main ()] starts the game engine. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the\n 3110 Puzzle Game engine.\n";
  print_endline "Please press any key to\n begin the game.\n";
  print_string "> ";
  initialize_window ();
  Text.draw_box
    (map_h * tile_size, map_w * tile_size)
    "Select the mode you want to play in and press \"START\" to begin \
     the game. Use \"asdw\" for player 1 and \"jkli\" for player 2. \
     Fill all holes on the screen by pushing blocks into them, then go \
     to the exit to arrive at the next level.";
  wait_button state_history

(* Execute the game engine. *)
let () = main ()
