type player = {
  position : int * int;
  on_exit : bool;
}

let get_player_pos player = player.position

type block = {
  position : int * int;
  in_hole : bool;
}

let get_block_pos block = block.position

(*Where ttype is the tile type*)
(*I was thinking that if, for example, the player was on the tile, we
  could just update the tile and store the player's position in player*)
(*Instead of checking is_something maybe just pattern match against tile
  type?*)

(*where orientation is horizontal or vertical*)
type boundary = {
  position : int * int;
  orientation : string;
}

let get_boundary_type boundary = boundary.orientation

let make_bound pos orientation = { position = pos; orientation }

(*where active is whether the hole is filled or not*)
type hole = {
  position : int * int;
  active : bool;
}

(* maybe use variant to represent type of tile...? *)
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

(* let make_tile pos ttype = { position = pos; ttype } *)

let get_tile_type tile = tile.ttype

type player_action =
  | Start
  | Quit
  | Step

type room = {
  room_id : string;
  width : float;
  height : float;
  mutable tile_list : tiles list;
  blocks : block list;
  holes : hole list;
}

type state = {
  mutable current_room_id : string;
  all_rooms : room list;
  mutable player : player;
  mutable tiles : tiles list;
  mutable blocks : block list;
  mutable hole : hole list;
}

type command = {
  up : bool;
  down : bool;
  left : bool;
  right : bool;
}

(* let rec return_ttype tile_list acc= match tile_list with | [] -> acc
   | x::xs -> match x with | Boundary _-> let map1 = [] *)

(* let make_map1 map1 = for i = 0 to 10 do make_tile (i, 0) (Boundary
   (make_bound (i, 0) "horizontal"))::[] done *)
