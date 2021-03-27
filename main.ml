open Types
open State

(** [change_state st direction] returns a new state given the current
    state [st] and the direction of movement [direction].

    If the movement is [Illegal] then this function returns the old
    state. *)
let change_state st direction =
  let state_of_result st = function Legal t -> t | Illegal -> st in
  state_of_result st (move direction st)

(** [prompt_command st] prompts the user for a command while in state
    [st] and returns the new state given the command.

    If the player enters an [Empty] or [Malformed] command then a
    message is printed to the screen and the old state is returned. *)
let prompt_command st =
  print_endline "Where do you go next?";
  try
    match parse (read_line ()) with
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^ \n\n";
        exit 0
    | Go direction -> change_state st direction
    | Start -> st
  with
  | Empty ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\nYou must type something. Type quit to stop the game or " w
        ", " a ", " s ", " d " to move the player. \n\n";
      st
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\nType quit to stop the game or " w ", " a ", " s ", " d
        " to move the player. \n\n";
      st

(** [print_gline line] prints one row of the map which is given by
    [line]. *)
let rec print_gline (line : tile list) =
  match line with
  | [] -> print_string "\n"
  | Hor_bound :: t ->
      print_string "-'";
      print_gline t
  | Ver_bound :: t ->
      print_string "|";
      print_gline t
  | Player _ :: t ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "X";
      print_gline t
  | Normal normal :: t ->
      if normal.is_hole then (
        ANSITerminal.print_string [ ANSITerminal.red ] "O";
        print_gline t)
      else (
        print_string " ";
        print_gline t)

(** [print_game st] prints a state [st] to the screen using its [map]
    attribute. *)
let print_game (st : state) =
  let map = st.map_tile_list in
  let rec print_map = function
    | [] -> print_string "\n"
    | h :: t ->
        print_gline h;
        print_map t
  in
  print_map map

(** [play_game st] executes the game at state [st]. It prints the map to
    the screen and prompts the user for a command. *)
let rec play_game st =
  print_game st;
  let new_state = prompt_command st in
  play_game new_state

(** [main ()] prompts the user to start the game, then starts it.

    This method will continually prompt the user until they enter
    "start".*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Puzzle Game engine.\n";
  let rec start_game =
    print_endline "Please type 'start' to begin the game.\n";
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | s -> (
        match parse s with
        | Start -> play_game init_state
        | _ -> start_game)
  in
  start_game

(* Execute the game engine. *)
let () = main ()
