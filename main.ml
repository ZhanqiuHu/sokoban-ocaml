open Types
open State
open Command

(** [change_state st direction] returns a new state given the current
    state [st] and the direction of movement [direction].

    If the movement is [Illegal] then this function returns the old
    state. *)
let change_state st direction =
  let state_of_result st = function Legal t -> t | Illegal -> st in
  state_of_result st (move st direction)

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
        "\n\
         You must type something. Type quit to stop the game or 'w', \
         's', 'a', 'd' to move the player. \n\n";
      st
  | Malformed ->
      ANSITerminal.print_string [ ANSITerminal.cyan ]
        "\n\
         Type quit to stop the game or 'w', 's', 'a', 'd' to move the \
         player. \n\n";
      st

(** [print_gline line] prints one row of the map which is given by
    [line].

    The helper function inside [print_tile tile] prints out [tile]
    according to its [ttype] attribute. *)
let rec print_gline (line : tile list) =
  let rec print_tile (tile : tile) =
    match tile.ttype with
    | Hor_bound -> print_string "-"
    | Ver_bound -> print_string "|"
    | Player _ -> ANSITerminal.print_string [ ANSITerminal.blue ] "X"
    | Normal normal ->
        if normal.is_hole then
          ANSITerminal.print_string [ ANSITerminal.red ] "O"
        else print_string " "
    | Exit -> ANSITerminal.print_string [ ANSITerminal.green ] "+"
    | Block _ -> print_string "#"
  in
  match line with
  | [] -> print_string "\n"
  | h :: t ->
      print_tile h;
      print_gline t

(** [print_game st] prints a state [st] to the screen using its [map]
    attribute. *)
let print_game (st : state) =
  let map = get_tile_list st in
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

(** [start_game s] starts the adventure is [s] is 'start'. If [s] is
    'quit' then the game stops. Otherwise, it prompts for a valid
    start/quit command. *)
let rec start_game s =
  try
    match parse s with
    | Start -> play_game init_state
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^ \n\n";
        exit 0
    | _ -> (
        print_endline "Please type 'start' to begin the game.\n";
        print_string "> ";
        match read_line () with
        | exception End_of_file -> ()
        | s -> start_game s )
  with
  | Empty -> (
      print_endline "Please type 'start' to begin the game.\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | s -> start_game s )
  | Malformed -> (
      print_endline "Please type 'start' to begin the game.\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | s -> start_game s )

(** [main ()] prompts the user to start the game, then starts it.

    This method will continually prompt the user until they enter
    "start".*)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Puzzle Game engine.\n";
  print_endline "Please type 'start' to begin the game.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | s -> start_game s

(* let update_map st = let map = get_tile_list st in let ppos =
   get_player_pos room(*insert function we just wrote that gets a
   room*)in match map with | [] -> [] | x::xs -> if x.position = ppos
   then | *)

(* Execute the game engine. *)
let () = main ()
