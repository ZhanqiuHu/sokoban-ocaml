(** Representation of dynamic map state.
    This module represents the state of a map as it is being
    played, including the player's current position, the puzzle pieces that have
    been correctly placed, and functions that cause the state to change. *)

(** The abstract type of values representing the game state. *)
type t

(** [init_state a] is the initial state of the game when playing
    map [a]. In that state the player is currently located in
    the starting position (left side center), and they have not placed any
    puzzle pieces. *)
val init_state : Map.t -> t

(** [current_player_position st] is the position in which the
    player currently is located in state [st]. *)
val current_player_position : t -> Map.position

(** [completed_puzzles st] is a set-like list of the room identifiers the
    adventurer has visited in state [st]. The adventurer has visited a
    room [rm] if their current room location is or has ever been [rm]. *)
val completed_puzzles : t -> Adventure.room_id list

(** The type representing the result of an attempted movement. *)
type result =
  | Legal of t
  | Illegal

(** [go exit adv st] is [r] if attempting to go through exit [exit] in
    state [st] and adventure [adv] results in [r]. If [exit] is an exit
    from the adventurer's current room, then [r] is [Legal st'], where
    in [st'] the adventurer is now located in the room to which [exit]
    leads. Otherwise, the result is [Illegal].
    Effects: none. [go] is not permitted to do any printing. *)
val go : Adventure.exit_name -> Adventure.t -> t -> result