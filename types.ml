type player = {
  position : int * int;
  on_exit : bool;
}

type block = {
  position : int * int;
  in_hole : bool;
}

type hole = { position : int * int }

type ttypes =
  | Obstacle
  | Normal
  | Exit

type tile = {
  position : int * int;
  ttype : ttypes;
}

type game_object =
  | Player of player
  | Block of block
  | Hole of hole
  | Tile of tile

type room = {
  room_id : string;
  width : int;
  height : int;
  map_tile_list : tile list list;
  init_blocks : block list;
  holes : hole list;
  num_holes : int;
  exit_pos : int * int;
  init_pos : int * int;
}

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

type command =
  | Start
  | Go of direction
  | Quit
