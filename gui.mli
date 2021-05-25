(** Representation of gui. This Gui processes images through OCaml
    Graphics and prints out objects onto canvas.*)

open Types

(** [draw_button b] draws the button [b] onto the canvas*)
val draw_button : Types.button -> unit

(** [draw_select select_list] draws the select list [select_list] onto
    the canvas*)
val draw_select : Types.select list -> unit

(** [draw_tiles width height tile_list] draws the tile list list
    [tile_list] onto the canvas of [width] and [height]*)
val draw_tiles : int -> int -> Types.tile list list -> unit

(** [draw_rect_images st width height] draws the tile list list within
    state [st] onto the canvas of [width] and [height]*)
val draw_rect_images : state -> int -> int -> unit

(** [draw_player st wt ht] draws the current players in the state [st]
    to the screen given tile width [width] and tile height [height]. *)
val draw_player : Types.state -> int -> int -> unit

(** [draw_block_list st wt ht] draws the blocks in the state [st] to the
    screen given tile width [width] and tile height [height]. *)
val draw_block_list : Types.state -> int -> int -> unit

(** [draw_hole_list st width height] draws the holes in the state [st]
    to the screen given tile width [width] and tile height [height]. *)
val draw_hole_list : Types.state -> int -> int -> unit

(** [draw_break_list st wt ht] draws the current breakables in the state
    [st] to the screen given tile width [wt] and tile height [ht]. *)
val draw_break_list : Types.state -> int -> int -> unit
