type player = { on_exit : bool }

type block = { in_hole : bool }

type normal = { is_hole : bool }

(* maybe use variant to represent type of tile...? *)

type ttypes =
  | Player of player
  | Block of block
  | Hor_bound
  | Ver_bound
  | Normal of normal
  | Exit

type tile = {
  position : int * int;
  mutable ttype : ttypes;
}

type room = {
  room_id : string;
  width : int;
  height : int;
  mutable map_tile_list : tile list array;
  blocks : block list;
  holes : (int * int) list;
}

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

type command =
  | Start
  | Go of direction
  | Quit
