open Types
open Map

(** Returns the tuple position of the player. *)
let get_player_pos st = st.player.position

(** Returns the id of the room given in this state. *)
let get_room_by_id room_id st =
  st.all_rooms
  |> List.filter (fun room -> room.room_id = room_id)
  |> List.hd

let init_state room : state =
  {
    current_room_id = "beginning";
    (* should be [map1] *) all_rooms = [ map1; win ];
    player = { position = (1, 1); on_exit = false };
    filled_holes = 0;
    exit_active = false;
    blocks = map1.init_blocks;
  }

let get_tile_by_loc loc room =
  room.map_tile_list
  |> List.filter (fun tile -> tile.position = loc)
  |> List.hd

(** Returns the new position of the object position given the direction
    of movement. *)
let new_pos old_pos direction =
  let x = fst old_pos in
  let y = snd old_pos in
  match direction with
  | Left -> (x - 1, y)
  | Right -> (x + 1, y)
  | Down -> (x, y - 1)
  | Up -> (x, y + 1)
  | Empty -> (x, y)

(** Returns true if the new position of the given object with the given
    direction of movement moves to an obstacle. False otherwise. *)
let collide st dir room =
  let pos = new_pos st dir in
  let tile = get_tile_by_loc pos room in
  match tile.ttype with Obstacle -> true | _ -> false

let get_background_list (room : room) : tile list = room.map_tile_list

let get_blocks (st : state) : block list = st.blocks

let get_hole_list (room : room) : hole list = room.holes

type result =
  | Legal of state
  | Illegal

(** Returns true if the player is currently on the exit in the current
    state. *)
let check_on_exit loc room = loc = room.exit_pos

(** Returns true if all the holes in the room have been filled. *)
let check_full st room = st.filled_holes = room.num_holes

(** Returns the next room given the current state. *)
let get_next_room st =
  let rec get_next lst =
    match lst with
    | [] -> raise Not_found
    | [ h ] -> raise Not_found
    | h :: h2 :: t ->
        if h.room_id = st.current_room_id then h2.room_id
        else get_next (h2 :: t)
  in
  get_next st.all_rooms

(** Checks if new position to move to is the same as the position of any
    of the other blocks and returns that block. Otherwise, it returns
    None.*)
let check_blocks new_pos (blocks : block list) =
  let rec get_block (lst : block list) =
    match lst with
    | [] -> None
    | h :: t -> if h.position = new_pos then Some h else get_block t
  in
  get_block blocks

(**Checks if block has been moved onto an exit or obstacle.*)
let is_not_normal (place : tile) =
  match place with
  | t -> (
      match t.ttype with
      | Obstacle -> true
      | Normal -> false
      | Exit -> true)

(**Checks if block movement is legal. That is, the block should not be
   moved onto an exit, block, or obstacle.*)
let block_legal
    (room : room)
    (new_pos : int * int)
    (block_list : block list) =
  let tile_list = room.map_tile_list in
  let rec tile_helper tile_list new_pos =
    match tile_list with
    | [] -> false
    | h :: t ->
        if h.position = new_pos && not (is_not_normal h) then true
        else tile_helper t new_pos
  in
  let rec block_helper (block_list : block list) new_pos =
    (*Returns true if not block*)
    match block_list with
    | [] -> true
    | h :: t ->
        if h.position = new_pos then false else block_helper t new_pos
  in
  tile_helper tile_list new_pos && block_helper block_list new_pos

(** Returns the block with the new position it is in and its in_hole
    attribute updated with the new position. *)
let new_block (b : block) (new_pos : int * int) (holes : hole list) :
    block =
  let rec check_hole (lst : hole list) =
    match lst with
    | [] -> { b with position = new_pos }
    | h :: t ->
        if h.position = b.position then
          { position = new_pos; in_hole = true }
        else check_hole t
  in
  check_hole holes

(** Returns the new block list given the block that moves, if any. *)
let new_block_list
    (room : room)
    (block : block option)
    (new_pos : int * int)
    (st : state) : block list =
  match block with
  | None -> st.blocks
  | Some b ->
      let rec new_block_list_helper
          (block_list : block list)
          (acc : block list) : block list =
        match block_list with
        | [] -> acc
        | h :: t ->
            if h = b then new_block b new_pos room.holes :: acc
            else new_block_list_helper block_list (h :: acc)
      in
      new_block_list_helper st.blocks []

(** Checks if the level is finished. That is the player is on the exit
    and all the holes are filled. *)
let next_level st =
  let next = get_next_room st in
  Legal
    {
      st with
      current_room_id = next;
      player =
        {
          position = (get_room_by_id next st).init_pos;
          on_exit = false;
        };
      blocks = [];
      filled_holes = 0;
      exit_active = false;
    }

(** Returns true if new player position is not a block or if it is a
    block but the block can be moved in the given direction. *)
let move_blocks p_pos dir st =
  match check_blocks p_pos st.blocks with
  | None -> true
  | Some b ->
      block_legal
        (get_room_by_id st.current_room_id st)
        (new_pos b.position dir)
        st.blocks

let move (st : state) (dir : direction) (room : room) : result =
  let current_rm = get_room_by_id st.current_room_id st in
  if collide st.player.position dir current_rm then Illegal
  else if check_on_exit st.player.position room && check_full st room
  then next_level st
  else
    let new_loc = new_pos st.player.position dir in
    let legal_move = move_blocks new_loc dir st in
    if not legal_move then Illegal
    else
      Legal
        {
          st with
          player =
            { position = new_loc; on_exit = check_on_exit new_loc room };
          blocks =
            new_block_list current_rm
              (check_blocks new_loc st.blocks)
              (new_pos new_loc dir) st;
        }

(* (* better change on_exit to on_hole *) type player = { on_exit : bool
   }

   type block = { in_hole : bool }

   type normal = { is_hole : bool }

   (* maybe use variant to represent type of tile...? *)

   type ttypes = | Player of player | Block of block | Hor_bound |
   Ver_bound | Normal of normal | Exit

   type tile = { position : int * int; mutable ttype : ttypes; }

   type room = { room_id : string; (* should be int *) width : int;
   height : int; mutable map_tile_list : tile list list; blocks : block
   list; holes : (int * int) list; }

   type state = { mutable current_room_id : string; all_rooms : room
   list; mutable player_pos : int * int; }

   type direction = | Left | Right | Up | Down *)

(* [get_player_pos room] is the pos of tile in [room] that is of ttype
   Player. *)
(* let get_player_pos room = let player_tile = room.map_tile_list |>
   List.flatten |> List.filter (fun tile -> match tile.ttype with Player
   _ -> true | _ -> false) |> List.hd in player_tile.position *)

(* let get_tile_list st = let rec get_room lst = match lst with | [] ->
   raise Not_found | h :: t -> if h.room_id = st.current_room_id then h
   else get_room t in (get_room st.all_rooms).map_tile_list *)
(* let rec update_loc_tile_to_player loc tile_list : tile list = match
   tile_list with | [] -> [] | h :: t -> (if h.position = loc then { h
   with ttype = Player { on_exit = false } } else h) ::
   update_loc_tile_to_player loc t

   let rec update_loc_tile_to_normal loc tile_list : tile list = match
   tile_list with | [] -> [] | h :: t -> (if h.position = loc then { h
   with ttype = Normal { is_hole = false } } else h) ::
   update_loc_tile_to_normal loc t *)

(* let list_to_nested_list room_width room_height lst = let rec
   loop_helper lst room_width n = match n with | 0 -> [] | _ ->
   List.filter (fun item -> ((10 * snd item.position) + fst
   item.position) / room_width = room_height - n) lst :: loop_helper lst
   room_width (n - 1) in loop_helper lst room_width room_height

   let rec update_room_in_rm_list updated_room rm_id rm_list : room list
   = match rm_list with | [] -> [] | h :: t -> (if h.room_id = rm_id
   then updated_room else h) :: update_room_in_rm_list updated_room
   rm_id t

   let update_location loc (dir : direction) = match dir with | Left ->
   (fst loc - 1, snd loc) | Right -> (fst loc + 1, snd loc) | Up -> (fst
   loc, snd loc - 1) | Down -> (fst loc, snd loc + 1) *)
(* let move (st : state) (dir : direction) : result = let current_loc =
   st.player_pos in let new_loc = update_location current_loc dir in let
   current_rm = get_room_by_id st.current_room_id st in if
   reach_boundary new_loc current_rm then Illegal else let updated_room
   = { current_rm with map_tile_list = current_rm.map_tile_list |>
   List.flatten |> update_loc_tile_to_normal current_loc |>
   update_loc_tile_to_player new_loc |> list_to_nested_list
   current_rm.width current_rm.height; } in Legal { st with all_rooms =
   update_room_in_rm_list updated_room st.current_room_id st.all_rooms;
   player_pos = new_loc; } *)
