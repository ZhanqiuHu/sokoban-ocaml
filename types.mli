(** Representation of the obstacles, player, and tiles.*)

(** Represents the player. on_exit is true when the player is on the
    exit *)
type player = { on_exit : bool }

(** Represents a block. in_hole is true when it overlaps a hole
    coordinate*)
type block = { in_hole : bool }

(** Represents a normal tile in the game. When is_hole is true,
    represents a hole*)
type normal = { is_hole : bool }

(** Represents the possible types of objects in the game. Player is the
    player, Block is a block, Hor_bound is a horizontal boundary,
    Ver_boundary is a vertical boundary, Normal is a normal tile, and
    Exit is the exit.*)
type ttypes =
  | Player of player
  | Block of block
  | Hor_bound
  | Ver_bound
  | Normal of normal
  | Exit

(** Represents an object in the game.*)
type tile = {
  position : int * int;
  mutable ttype : ttypes;
}

(** Represents a "level" in the game. blocks is the list of blocks in
    the room. holes represents the coordinates of all holes in the game. *)
type room = {
  room_id : string;
  width : int;
  height : int;
  mutable map_tile_list : tile list array;
  blocks : block list;
  holes : (int * int) list;
}

(** Represents the current room, all_rooms, and the current player
    position.*)
type state = {
  mutable current_room_id : string;
  all_rooms : room list;
  mutable player_pos : int * int;
}

type direction =
  | Left
  | Right
  | Up
  | Down

(** Represents player commands: moving, starting/stopping the game.*)
type command =
  | Start
  | Go of direction
  | Quit
