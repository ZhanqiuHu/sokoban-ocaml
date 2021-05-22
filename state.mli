(* * Representation of state. This module represents the state of a map
   as it is being played, including the player's current position, the
   block pieces, the layout of the rooms, and functions that cause the
   state to change. *)

open Types

(** [init_state] returns the initial state when the game is started*)
val init_state : string * string -> state

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of state
  | Illegal

(** get_tile_list [state] returns the 2D list of tiles that map the
    current room state. *)

(* val get_tile_list : state -> tile list *)

val get_player : state -> player list

val get_tile_list : state -> tile list list

val get_blocks : state -> block list

val get_hole_list : room -> hole list

val get_breaks : state -> breakable1 list

val get_room_by_id : string -> state -> room

val initialize_state : state -> int -> state

val duplicate_state : state -> state

(** [move st dir] returns a result given the current state [st] and
    direction [dir]. If the movement is allowed, it returns a result
    [Legal state] with the new state. If the movement is not allowed, it
    returns [Illegal]. *)
val move : state -> direction -> room -> player_num -> result
