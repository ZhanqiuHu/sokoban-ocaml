(** This module draws text boxes and text onto the screen. *)
open Types

(** [get_hw room_id st tile_size] takes the height and width of the room
    given by [room_id] in the given [st] and scales them according to
    [tile_size]. Returns the tuple of the scaled height and width.*)
val get_hw : string -> state -> int -> int * int

(** [parse_dialogue dialogue acc width] takes a string [dialogue] and
    breaks it into a string list [acc] according to the [width].*)
val parse_dialogue : string -> string list -> int -> string list

(** [draw_dialogue dia_list pos] moves the cursor to [pos] and draws
    each string in [dia_list] onto the screen. *)
val draw_dialogue : string list -> int -> unit

(** [draw_box room_width dialogue] takes a string [dialogue] and draws
    it to the screen after drawing a box and setting the text color.*)
val draw_box : int * int -> string -> unit
