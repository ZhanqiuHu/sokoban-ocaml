(** Representation of state. This module represents the state of a map
    as it is being played, including the player's current position, the
    blocks/holes/breakeables, the layout of the rooms, and functions
    that cause the state to change. *)

open Types

(** [init_state sel_list] returns the initial [state] when the game is
    started. [sel_list] is the tuple of selections, with the first
    element being the number of players, and the second element being
    the game mode.*)
val init_state : string * string -> state

(** The type [result] representing the result of an attempted movement. *)
type result =
  | Legal of state
  | Illegal

(** [get_player st] returns the [player list] in the current state [st] *)
val get_player : state -> player list

(** [get_tile_list st] returns the 2D [tile list list] of tiles that map
    the current room state [st]. *)
val get_tile_list : state -> tile list list

(** [get_blocks st] returns the [block list] in the current [st]. *)
val get_blocks : state -> block list

(** [get_hole_list room] returns the [hole list] in the current [room]. *)
val get_hole_list : room -> hole list

(** [get_breaks st] returns the [breakeable1 list] in the current [st]. *)
val get_breaks : state -> breakable1 list

(** [get_room_by_id room_id state] returns the [room] associated with
    the [room_id] in the current [state]. *)
val get_room_by_id : string -> state -> room

(** [duplicate_state st] returns the [state] with the same fields and
    field contents as [st]. *)
val duplicate_state : state -> state

(** [move st dir room player_num] returns a [result] given the current
    state [st], direction [dir], room [room], and player_num
    [player_num]. If the movement is allowed, it returns a result
    [Legal state] with the new state. If the movement is not allowed, it
    returns [Illegal]. *)
val move : state -> direction -> room -> player_num -> result
