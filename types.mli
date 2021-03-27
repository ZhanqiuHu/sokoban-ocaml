(** Representation of the obstacles, player, and tiles.*)

(** Represents the player *)
type player = {
  position : int * int;
  on_exit : bool;
}

(** Represents a block*)
type block = {
  position : int * int;
  in_hole : bool;
}

(** Represents a tile in the map*)

(** Represents a horizontal or vertical boundary*)
type boundary = {
  position : int * int;
  orientation : string;
}

(** Represents a hole in the map*)
type hole

(** Represents the types of actions a player can take*)
type player_action

(** Represents a map in the game*)

(** Represents a command in game*)
type command

(** Represents the current state*)
type state

type tiles =
  | Player of player
  | Block of block
  | Hole of hole
  | Boundary of boundary
  | Normal

type tile = {
  (* position : int * int; *)
  (* is_player : bool; is_block : bool; is_boundary : bool; is_hole :
     bool; is_norm : bool; *)
  mutable ttype : tiles;
}

type room = {
  room_id : string;
  width : float;
  height : float;
  mutable tile_list : tiles list;
  blocks : block list;
  holes : hole list;
}

(* val make_tile : int * int -> tiles -> tile *)

val make_bound : int * int -> string -> boundary

(* (** [init_state a] is the initial state of the game when playing map
   [a]. In that state the player is currently located in the starting
   position (left side center), and they have not placed any puzzle
   pieces. *) val init_state : Map.t -> t

   (** [current_player_position st] is the position in which the player
   currently is located in state [st]. *) val current_player_position :
   t -> Map.position

   (** [completed_puzzles st] is a set-like list of the room identifiers
   the adventurer has visited in state [st]. The adventurer has visited
   a room [rm] if their current room location is or has ever been [rm].
   *) val completed_puzzles : t -> Adventure.room_id list

   (** The type representing the result of an attempted movement. *)
   type result = | Legal of t | Illegal

   (** [go exit adv st] is [r] if attempting to go through exit [exit]
   in state [st] and adventure [adv] results in [r]. If [exit] is an
   exit from the adventurer's current room, then [r] is [Legal st'],
   where in [st'] the adventurer is now located in the room to which
   [exit] leads. Otherwise, the result is [Illegal]. Effects: none. [go]
   is not permitted to do any printing. *) val go : Adventure.exit_name
   -> Adventure.t -> t -> result *)
