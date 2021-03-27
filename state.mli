open Types
(** Representation of state.
    This module represents the state of a map as it is being
    played, including the player's current position, the block pieces, the 
    layout of the rooms, and functions that cause the state to change. *)

(** [init_state] returns the initial state when the game is started*)
val init_state : state

(** [move st dir] is [st'] if the player moves in direction [dir] from a
    previous state [st]. 
    *)
val move : state -> direction -> state