(* * Representation of state. This module represents the state of a map
   as it is being played, including the player's current position, the
   block pieces, the layout of the rooms, and functions that cause the
   state to change. *)
open Types

(** [init_state] returns the initial state when the game is started*)
val init_state : state

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of state
  | Illegal

(** get_tile_list [state] returns the 2D list of tiles that map the
    current room state. *)
val get_tile_list : state -> tile list list

(** [move st dir] returns a result given the current state [st] and
    direction [dir]. If the movement is allowed, it returns a result
    [Legal state] with the new state. If the movement is not allowed, it
    returns [Illegal]. *)
val move : state -> direction -> result
