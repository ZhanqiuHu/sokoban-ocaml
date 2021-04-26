open Types
open Map
open Genmap

(** Returns the tuple position of the player. *)
let get_player st = st.players

(** Returns the id of the room given in this state. *)
let get_room_by_id room_id st =
  st.all_rooms
  |> List.filter (fun room -> room.room_id = room_id)
  |> List.hd

let init_state : state =
  {
    current_room_id = "random";
    all_rooms = [ map2; win ];
    players =
      [
        {
          position = (1, 1);
          on_exit = false;
          player_num = Fst;
          player_img = "link60x60.png";
        };
        {
          position = (8, 8);
          on_exit = false;
          player_num = Snd;
          player_img = "images/player60x60.png";
        };
      ];
    filled_holes = 0;
    blocks = map2.init_blocks;
    breaks = map2.init_breaks;
  }

let get_tile_by_loc loc room =
  room.map_tile_list |> List.flatten
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
let collide old_pos dir room =
  let pos = new_pos old_pos dir in
  let tile = get_tile_by_loc pos room in
  match tile.ttype with Obstacle -> true | _ -> false

(** Updates st.breaks to be the correct hp given that the player will
    move according to the direction given. Returns whether the player is
    blocked *)
let break (st : state) old_pos dir =
  let pos = new_pos old_pos dir in
  let rec find_break (lst : breakable1 list) =
    match lst with
    | [] -> false
    | h :: t ->
        if h.position = pos then (
          h.hp <- h.hp - 1;
          h.hp >= 0)
        else find_break t
  in
  find_break st.breaks

let is_break (st : state) old_pos dir =
  let pos = new_pos old_pos dir in
  let rec find_break (lst : breakable1 list) =
    match lst with
    | [] -> false
    | h :: t -> if h.position = pos then true else find_break t
  in
  find_break st.breaks

let get_tile_list st =
  let rec get_room lst =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if h.room_id = st.current_room_id then h else get_room t
  in
  (get_room st.all_rooms).map_tile_list

let get_blocks (st : state) : block list = st.blocks

let get_hole_list (room : room) : hole list = room.holes

let get_breaks (st : state) : breakable1 list = st.breaks

type result =
  | Legal of state
  | Illegal

(** Returns true if the player is currently on the exit in the current
    state. *)
let check_on_exit loc room = loc = room.exit_pos

(** Returns true if all the holes in the room have been filled. *)
let check_full st room =
  if st.filled_holes = room.num_holes then (
    Stdlib.print_string "check_full";
    true)
  else false

let update_filled_holes blocks =
  let rec hole_counter blocks acc =
    match blocks with
    | [] -> acc
    | h :: t ->
        if h.in_hole then (
          Stdlib.print_string "T";
          hole_counter t (acc + 1))
        else (
          Stdlib.print_string "F";
          hole_counter t acc)
  in
  hole_counter blocks 0

(** Returns the next room given the current state. Assumes the win room
    is always at the end. (at least two rooms in the game) *)
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

let rec block_helper (block_list : block list) new_pos =
  (*Returns true if not block*)
  match block_list with
  | [] -> true
  | h :: t ->
      if h.position = new_pos then false else block_helper t new_pos

(**Checks if block movement is legal. That is, the block should not be
   moved onto an exit, block, obstacle, or breakable.*)
let block_legal (room : room) (new_pos : int * int) (st : state) =
  let rec tile_helper tile_list new_pos =
    match tile_list with
    | [] -> false
    | h :: t ->
        if h.position = new_pos && not (is_not_normal h) then true
        else tile_helper t new_pos
  in
  let rec break_helper (break_list : breakable1 list) new_pos =
    (*Returns true if not breakable*)
    match break_list with
    | [] -> true
    | h :: t ->
        if h.position = new_pos && h.hp >= 0 then false
        else break_helper t new_pos
  in
  let tile_list = List.flatten room.map_tile_list in
  tile_helper tile_list new_pos && break_helper st.breaks new_pos

let movement_legal (room : room) (new_pos : int * int) (st : state) =
  let rec tile_helper tile_list new_pos =
    match tile_list with
    | [] -> false
    | h :: t ->
        if h.position = new_pos && not (is_not_normal h) then true
        else tile_helper t new_pos
  in
  let rec break_helper (break_list : breakable1 list) new_pos =
    (*Returns true if not breakable*)
    match break_list with
    | [] -> true
    | h :: t ->
        if h.position = new_pos && h.hp >= 0 then false
        else break_helper t new_pos
  in
  let tile_list = List.flatten room.map_tile_list in
  tile_helper tile_list new_pos && break_helper st.breaks new_pos

(* && block_helper st.blocks new_pos *)

(** Returns the block with the new position it is in and its in_hole
    attribute updated with the new position. *)
let new_block (b : block) (new_pos : int * int) (holes : hole list) :
    block =
  let b = { b with in_hole = false } in
  let rec check_hole (lst : hole list) =
    match lst with
    | [] -> { b with position = new_pos }
    | h :: t ->
        if h.position = new_pos then
          { position = new_pos; in_hole = true }
        else check_hole t
  in
  check_hole holes

let get_gameobj_pos game_object =
  match game_object with
  | Player p -> p.position
  | Tile t -> t.position
  | Block b -> b.position
  | Break1 b1 -> b1.position
  | Hole h -> h.position

(** Returns true if new player position is not a block or if it is a
    block but the block can be moved in the given direction. *)
let move_blocks p_pos dir st =
  match check_blocks p_pos st.blocks with
  | None -> true
  | Some b ->
      block_legal
        (get_room_by_id st.current_room_id st)
        (new_pos b.position dir)
        st

let new_block_list
    (room : room)
    (block : block option)
    (new_pos : int * int)
    (st : state) : block list =
  match block with
  | None -> st.blocks
  | Some b ->
      List.map
        (fun block : block ->
          if block = b then new_block block new_pos room.holes
          else block)
        st.blocks

(** Returns the new block list given the obj that moves, if any. *)
let pushed_obj_new_block_list
    (room : room)
    (new_loc : int * int)
    (dir : direction)
    (st : state) : block list =
  List.map
    (fun (block : block) ->
      if block.position = new_loc && move_blocks new_loc dir st then
        new_block block (new_pos new_loc dir) room.holes
      else block)
    st.blocks

(** Returns the new player list given the obj that moves, if any. *)
let pushed_obj_new_player_list
    (room : room)
    (dir : direction)
    (new_loc : int * int)
    (st : state) : player list =
  List.map
    (fun (player : player) ->
      if
        player.position = new_loc
        && (not (collide new_loc dir room))
        && not (break st new_loc dir)
      then
        {
          player with
          position = new_pos new_loc dir;
          on_exit = check_on_exit (new_pos new_loc dir) room;
        }
      else player)
    st.players

(** Returns the new breakables list *)
let new_break_list break_list : breakable1 list =
  let rec update_break lst acc =
    match lst with
    | [] -> acc
    | h :: t ->
        if h.hp = 0 then update_break t acc
        else update_break t (h :: acc)
  in
  update_break break_list []

(** Checks the win condition: all players in the player_list need to be
    on the exit, and all holes must be filled *)
let check_win room st =
  let players = st.players in
  let rec check_exit (players : player list) acc =
    match players with
    | [] -> acc
    | h :: t -> check_on_exit h.position room && check_exit t acc
  in
  check_exit players (check_full st room)

let update_player_next_level st next =
  let players = st.players in
  let next_rm = get_room_by_id next st in
  List.map
    (fun player ->
      { player with position = next_rm.init_pos; on_exit = false })
    players

(** Checks if the level is finished. That is the player is on the exit
    and all the holes are filled. *)
let next_level st =
  let next = get_next_room st in
  let next_rm = get_room_by_id next st in
  Legal
    {
      st with
      current_room_id = next;
      players = update_player_next_level st next;
      blocks = next_rm.init_blocks;
      breaks = next_rm.init_breaks;
      filled_holes = 0;
    }

let determine_player_num st player_num =
  st.players
  |> List.filter (fun player -> player.player_num = player_num)
  |> List.hd

let update_player st new_loc room player_num =
  List.map
    (fun player ->
      if player.player_num = player_num then
        {
          player with
          position = new_loc;
          on_exit = check_on_exit new_loc room;
        }
      else player)
    st.players

let updated_player st player_num =
  st.players
  |> List.filter (fun player -> player.player_num = player_num)
  |> List.hd

(* let get_gameobj game_object = match game_object with | Player p ->
   Player | Tile t -> t | Block b -> Block b | Break1 b1 -> Break1 b1 |
   Hole h -> Hole h *)

(**Returns the position of a game_object*)
let get_gameobj_pos game_object =
  match game_object with
  | Player p -> p.position
  | Tile t -> t.position
  | Block b -> b.position
  | Break1 b1 -> b1.position
  | Hole h -> h.position

let check_overlap (pushed_obj : game_object) (st : state) =
  let overlap_with_player =
    List.fold_left
      (fun init (player : player) ->
        init || player.position = get_gameobj_pos pushed_obj)
      false st.players
  in
  let block_list = st.blocks in
  let overlap_with_block =
    List.fold_left
      (fun init (block : block) ->
        init || block.position = get_gameobj_pos pushed_obj)
      false block_list
  in
  overlap_with_player || overlap_with_block

let gen_state players blocks filled_holes st =
  { st with players; blocks; filled_holes }

(* let (combine_list : game_object list) = player_list @ block_list in *)

(**Returns the new state of game_object with new position *)
let move_game_object game_object dir st room : state =
  let pos = get_gameobj_pos game_object in
  let players = pushed_obj_new_player_list room dir pos st in
  let blocks = pushed_obj_new_block_list room pos dir st in
  gen_state players blocks (update_filled_holes blocks) st

let overlap_player pushed_obj st =
  List.filter
    (fun (player : player) ->
      player.position = get_gameobj_pos pushed_obj)
    st.players

let overlap_block pushed_obj st =
  List.filter
    (fun (block : block) -> block.position = get_gameobj_pos pushed_obj)
    st.blocks

let check_next_obj game_object st : game_object option =
  if check_overlap game_object st then
    let player = overlap_player game_object st in
    let blocks = overlap_block game_object st in
    if player = [] then Some (Block (List.hd blocks))
    else Some (Player (List.hd player))
  else None

(* let move_legal game_object pos dir st = match check_overlap
   game_object st with let move_blocks p_pos dir st = match check_blocks
   p_pos st.blocks with | None -> true | Some b -> block_legal
   (get_room_by_id st.current_room_id st) (new_pos b.position dir) st *)

(** Returns true if new player position is not a block or if it is a
    block but the block can be moved in the given direction. *)
let rec move_rec
    (st : state)
    (dir : direction)
    (pushed_obj : game_object) : result =
  if check_overlap pushed_obj st then
    let room = get_room_by_id st.current_room_id st in
    let state_before_check = move_game_object pushed_obj dir st room in
    if check_win room state_before_check then (
      Stdlib.print_string "win";
      next_level state_before_check)
    else
      let new_push_obj = check_next_obj pushed_obj st in
      match new_push_obj with
      | None -> raise (Failure "Not_possible")
      | Some obj -> move_rec state_before_check dir obj
  else Legal st

let move
    (st : state)
    (dir : direction)
    (room : room)
    (player_num : player_num) : result =
  let current_rm = get_room_by_id st.current_room_id st in
  let player = determine_player_num st player_num in
  if collide player.position dir current_rm then Illegal
  else if break st player.position dir then Illegal
  else
    let new_loc = new_pos player.position dir in
    let legal_move = move_blocks new_loc dir st in
    if not legal_move then Illegal
    else
      let new_block_list =
        new_block_list current_rm
          (check_blocks new_loc st.blocks)
          (new_pos new_loc dir) st
      in
      let updated_player_lst =
        update_player st new_loc room player_num
      in
      let state_before_check =
        {
          st with
          players =
            pushed_obj_new_player_list current_rm dir new_loc
              { st with players = updated_player_lst };
          blocks = new_block_list;
          filled_holes = update_filled_holes new_block_list;
        }
      in
      if check_win room state_before_check then (
        Stdlib.print_string "win";
        next_level state_before_check)
      else
        let pushed_obj =
          check_next_obj
            (Player (updated_player state_before_check player_num))
            st
        in
        match pushed_obj with
        | None -> Legal state_before_check
        | Some obj -> move_rec state_before_check dir obj
