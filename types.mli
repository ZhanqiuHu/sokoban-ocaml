(** Representation of the obstacles, player, and tiles.*)

(** Represents the player. on_exit is true when the player is on the
    exit *)
type player = {
  position : int * int;
  on_exit : bool;
}
(** Represents a block. in_hole is true when it overlaps a hole
    coordinate*)
type block = {
  position : int * int;
  in_hole : bool;
}

(** Represents a hole in the game. *)
type hole = { position : int * int }

(** Represents the possible types of background objects in the game. 
    Obstacle is something the player cannot walk through, 
    and Normal is a walkable tile. Exit is the exit. *)
type ttypes =
  | Obstacle
  | Normal
  | Exit

(** Represents a background tile in the game.*)
type tile = {
  position : int * int;
  ttype : ttypes;
}

(**Represents types of objects in the game.*)
type game_object = 
| Player of player
| Block of block
| Hole of hole
| Tile of tile 


(** Represents a "level" in the game. blocks is the list of blocks in
    the room. holes list of holes in the level. num_holes is the number of
    holes in the level. *)
type room = {
  room_id : string;
  width : int;
  height : int;
  map_tile_list : tile list;
  init_blocks : block list;
  holes : hole list;
  num_holes : int;
  exit_pos : int * int;
  init_pos : int * int;
}

(** Represents the current room, all_rooms, and the current player
    position. filled_holes is the number of holes that are currently filled.
    exit_active is true when the number of filled holes is equal to the
    number of holes in the room. *)
type state = {
  mutable current_room_id : string;
  all_rooms : room list;
  player : player;
  mutable blocks : block list;
  filled_holes : int;
  exit_active : bool;
}

type direction =
  | Left
  | Right
  | Up
  | Down
  | Empty

(** Represents player commands: moving, starting/stopping the game.*)
type command =
  | Start
  | Go of direction
  | Quit
