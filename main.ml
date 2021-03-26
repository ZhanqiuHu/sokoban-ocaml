open Types
open Map
open State

let prompt_command st =
  print_endline "Where do you go next?";
  try
    match parse (read_line ()) with
    | Quit ->
        ANSITerminal.print_string [ ANSITerminal.yellow ]
          "Goodbye and may the camel be with you! ^w^ \n\n";
        exit 0
    | Left object_phrase -> change_state object_phrase adv st
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

let rec print_gline line =
  match line with
  | [] -> print_string "\n"
  | Horizontal :: t ->
      print_string "-'";
      print_gline t
  | Vertical :: t ->
      print_string "|";
      print_gline t
  | Player :: t ->
      ANSITerminal.print_string [ ANSITerminal.red ] "X";
      print_gline t

let rec print_game map =
  match map with
  | [] -> print_string "\n"
  | h :: t ->
      print_gline h;
      print_game t

let rec play_game st =
  print_game st;
  let new_state = prompt_command st in
  play_game new_state

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the 3110 Puzzle Game engine.\n";
  play_game init_state

(* Execute the game engine. *)
let () = main ()
