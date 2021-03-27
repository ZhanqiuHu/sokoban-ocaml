(* * [main ()] prompts for the game to play, then starts it. open Map
   let play_game f = Stdlib.print_endline "Starting your adventure... ";

   let main () = ANSITerminal.print_string [ ANSITerminal.red ]
   "\n\nWelcome to the 3110 Text Adventure Game engine.\n";
   print_endline "Please enter the name of the game file you want to\n
   load.\n"; print_string "> "; match read_line () with | exception
   End_of_file -> () | file_name -> play_game file_name

   (* Execute the game engine. *)

   let () = main () *)
