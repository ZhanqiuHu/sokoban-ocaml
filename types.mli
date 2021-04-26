type player_num =
  | Fst
  | Snd

type player = {
  position : int * int;
  on_exit : bool;
  player_num : player_num;
  player_img : string;
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

type breakable1 = {
  position : int * int;
  mutable hp : int;
}

type tile = {
  position : int * int;
  ttype : ttypes;
}

type game_object =
  | Player of player
  | Block of block
  | Hole of hole
  | Break1 of breakable1
  | Tile of tile

type room = {
  room_id : string;
  width : int;
  height : int;
  mutable map_tile_list : tile list list;
  init_blocks : block list;
  holes : hole list;
  num_holes : int;
  exit_pos : int * int;
  init_pos : int * int;
  init_breaks : breakable1 list;
}

type state = {
  mutable current_room_id : string;
  all_rooms : room list;
  mutable players : player list;
  mutable blocks : block list;
  mutable breaks : breakable1 list;
  filled_holes : int;
}

type direction =
  | Left
  | Right
  | Up
  | Down
  | Empty

type command =
  | Start
  | Fst of direction
  | Snd of direction
  | Quit
