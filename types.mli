(** types.mli contains all the types used for the game. *)

(** [player_num] is [Fst] if this is player one and [Snd] if this is
    player 2. *)
type player_num =
  | Fst
  | Snd

(** [player] is the type for a player object. *)
type player = {
  position : int * int;
  on_exit : bool;
  player_num : player_num;
  player_img : string;
}

(** [block] is the type for a block that is pushable. *)
type block = {
  position : int * int;
  in_hole : bool;
}

(** [hole] is the type for a hole. *)
type hole = { position : int * int }

(** [ttypes] is the type for any tile. It is either an obstacle
    (nonmovable block), exit, or normal walkable tile. *)
type ttypes =
  | Obstacle
  | Normal
  | Exit

(** [breakable1] is the type for a breakable object. *)
type breakable1 = {
  position : int * int;
  mutable hp : int;
}

(** [button] is the type for a pushable button. *)
type button = {
  mutable position : int * int;
  mutable width : int;
  mutable height : int;
  mutable image : string;
  mutable name : string;
  mutable enable : bool;
}

(** [select] is the type for a selectable button. It can be selected or
    not selected. *)
type select = {
  mutable position : int * int;
  mutable width : int;
  mutable height : int;
  mutable image_select : string;
  mutable image_deselect : string;
  mutable name : string;
  mutable enable : bool;
  mutable select : bool;
  mutable exclusives : select list;
}

(** [tile] is the type for a single tile of the background. *)
type tile = {
  position : int * int;
  ttype : ttypes;
}

(** [game_object] is the type for a single object. It can be a player,
    block, hole, breakable, tile (background), button, or select. *)
type game_object =
  | Player of player
  | Block of block
  | Hole of hole
  | Break1 of breakable1
  | Tile of tile
  | Button of button
  | Select of select

(** [room] is the type for a single map of the game. *)
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
  step_limit : int;
}

(** [modes] is the type for a block mode of the game. It can be normal,
    sliding blocks, or step limit. *)
type modes =
  | Normal
  | Sliding
  | Limit

(** [state] is the type for a state of the game. *)
type state = {
  mutable mode : modes;
  mutable active : bool;
  mutable current_room_id : string;
  all_rooms : room list;
  mutable players : player list;
  mutable blocks : block list;
  mutable breaks : breakable1 list;
  filled_holes : int;
  mutable steps_left : int;
}

(** [history] is the state history. It keeps track of previously visited
    states and the current number of steps. *)
type history = {
  mutable state_list : state list;
  mutable num_steps : int;
}

(** [direction] is the type for a player movement. It can be left,
    right, up, down, or empty (none). *)
type direction =
  | Left
  | Right
  | Up
  | Down
  | Empty

(** [command] is the type for player input to the game. It can be start,
    player one movement, player two movement, quit, back, pause, resume,
    or return. *)
type command =
  | Start
  | Fst of direction
  | Snd of direction
  | Quit
  | Back
  | Pause
  | Resume
  | Return
