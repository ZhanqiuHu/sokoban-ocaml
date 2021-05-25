open Types
open Map
open Genmap

(** Returns the tuple position of the player. *)
let get_player st = st.players

let rec copy_breaks breaks =
  match breaks with
  | h :: t -> { h with hp = h.hp } :: copy_breaks t
  | [] -> []

let duplicate_state st =
  {
    mode = st.mode;
    active = st.active;
    current_room_id = st.current_room_id;
    all_rooms = st.all_rooms;
    players = st.players;
    blocks = st.blocks;
    breaks = copy_breaks st.breaks;
    filled_holes = st.filled_holes;
    steps_left = st.steps_left;
  }

(** Returns the id of the room given in this state. *)
let get_room_by_id room_id st =
  List.find (fun room -> room.room_id = room_id) st.all_rooms

let player_one =
  {
    position = (1, 1);
    on_exit = false;
    player_num = Fst;
    player_img = "images/link60x60.png";
  }

let player_two =
  {
    position = (9, 8);
    on_exit = false;
    player_num = Snd;
    player_img = "images/player60x60.png";
  }

let init_state (sel_list : string * string) : state =
  {
    mode =
      (if "normal" = snd sel_list then Normal
      else if snd sel_list = "sliding" then Sliding
      else Limit);
    active = true;
    current_room_id = "random";
    all_rooms = [ map2; map_maze; win; lose ];
    players =
      (if "one" = fst sel_list then [ player_one ]
      else [ player_one; player_two ]);
    filled_holes = 0;
    blocks = map2.init_blocks;
    breaks = map2.init_breaks;
    steps_left = map2.step_limit;
  }

let rec init_breaks hp breaks_list =
  match breaks_list with
  | h :: t ->
      h.hp <- hp;
      init_breaks hp t
  | _ -> ()

let get_tile_by_loc loc room =
  List.find
    (fun tile -> tile.position = loc)
    (room.map_tile_list |> List.flatten)

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
let check_full st room = st.filled_holes = room.num_holes

let update_filled_holes blocks =
  let rec hole_counter blocks acc =
    match blocks with
    | [] -> acc
    | h :: t ->
        if h.in_hole then hole_counter t (acc + 1)
        else hole_counter t acc
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

let rec tile_helper tile_list new_pos =
  match tile_list with
  | [] -> false
  | h :: t ->
      let if_tile_normal =
        h.position = new_pos && not (is_not_normal h)
      in
      if if_tile_normal then true else tile_helper t new_pos

and break_helper (break_list : breakable1 list) new_pos =
  let rec break_helper_helper (break_list : breakable1 list) new_pos =
    (*Returns true if not breakable*)
    match break_list with
    | [] -> true
    | h :: t ->
        let if_break = h.position = new_pos && h.hp >= 0 in
        if if_break then false else break_helper t new_pos
  in
  break_helper_helper break_list new_pos

and player_helper
    (player_list : player list)
    new_position
    dir
    tile_list
    st =
  let rec player_helper_helper
      (player_list : player list)
      new_position
      dir
      tile_list
      st =
    match player_list with
    | [] -> true
    | h :: t ->
        if h.position = new_position then false
        else player_helper_helper t new_position dir tile_list st
  in
  player_helper_helper player_list new_position dir tile_list st

and block_helper (block_list : block list) new_position tile_list st dir
    =
  let rec block_helper_helper
      (block_list : block list)
      new_position
      tile_list
      st
      dir =
    let tile_break_player_check_legal =
      (not (tile_helper tile_list (new_pos new_position dir)))
      || (not (break_helper st.breaks (new_pos new_position dir)))
      || not
           (player_helper st.players
              (new_pos new_position dir)
              dir tile_list st)
    in
    match block_list with
    | [] -> true
    | h :: t ->
        let if_block_legal_move =
          h.position = new_position && tile_break_player_check_legal
        in
        if if_block_legal_move then false
        else block_helper t new_position tile_list st dir
  in
  block_helper_helper block_list new_position tile_list st dir

(**Checks if block movement is legal. That is, the block should not be
   moved onto an exit, block, obstacle, or breakable.*)
let block_legal_sliding
    (room : room)
    (new_ps : int * int)
    (st : state)
    (dir : direction) =
  let tile_list = List.flatten room.map_tile_list in

  tile_helper tile_list new_ps
  && break_helper st.breaks new_ps
  && player_helper st.players new_ps dir tile_list st
  && block_helper st.blocks new_ps tile_list st dir

(**Checks if block movement is legal. That is, the block should not be
   moved onto an exit, block, obstacle, or breakable.*)
let block_legal (room : room) (new_pos : int * int) (st : state) =
  let tile_list = List.flatten room.map_tile_list in
  tile_helper tile_list new_pos && break_helper st.breaks new_pos

(** Returns the block with the new position it is in and its in_hole
    attribute updated with the new position . *)
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

(** Returns true if new player position [p_pos] is not a block or if it
    is a block but the block can be moved in the given direction [dir]. *)
let move_blocks p_pos dir st =
  match check_blocks p_pos st.blocks with
  | None -> true
  | Some b ->
      block_legal
        (get_room_by_id st.current_room_id st)
        (new_pos b.position dir)
        st

let move_blocks_sliding p_pos dir st =
  match check_blocks p_pos st.blocks with
  | None -> true
  | Some b ->
      block_legal_sliding
        (get_room_by_id st.current_room_id st)
        (new_pos b.position dir)
        st dir

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

(** Returns the updated player list when win condition is achieved *)
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
      steps_left = next_rm.step_limit;
    }

(**Returns the new state with the player in the lose room.*)
let no_steps st =
  let next_rm = get_room_by_id "lose" st in
  Legal
    {
      st with
      current_room_id = "lose";
      players = update_player_next_level st "lose";
      blocks = next_rm.init_blocks;
      breaks = next_rm.init_breaks;
      filled_holes = 0;
      steps_left = next_rm.step_limit;
    }

(** Returns the player in st.players with the corrresponding
    [player_num] *)
let determine_player_num st player_num =
  List.find (fun player -> player.player_num = player_num) st.players

(** Returns the updated player list with the player with the
    corresponding [player_num] moved to [new_loc] *)
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

(** Returns the updated player with the position [new_loc] and updated
    field on_exit *)
let updated_player player new_loc room =
  {
    player with
    position = new_loc;
    on_exit = check_on_exit new_loc room;
  }

let modify_block_helper
    block
    new_loc
    dir
    room
    st
    result
    block_list_updated
    is_sliding =
  let modified_block =
    new_block block (new_pos new_loc dir) room.holes
  in
  let moveable =
    if is_sliding then move_blocks_sliding new_loc dir st
    else move_blocks new_loc dir st
  in
  if moveable then
    (result, Some modified_block, modified_block :: block_list_updated)
  else
    (Illegal, Some modified_block, modified_block :: block_list_updated)

(** Returns a tuple of (result, block_pushed, new_block_list) given that
    the pushed(moved) object in this round is a player *)
let pushed_player_new_block_list
    (room : room)
    (new_loc : int * int)
    (dir : direction)
    (st : state) : result * block option * block list =
  let is_sliding = st.mode = Sliding in
  let rec helper
      (init : result * block option * block list)
      (block_list : block list) =
    match block_list with
    | [] -> init
    | block :: tl -> (
        match helper init tl with
        | result, pushed_block, block_list_updated ->
            if block.position = new_loc then
              modify_block_helper block new_loc dir room st result
                block_list_updated is_sliding
            else (result, pushed_block, block :: block_list_updated))
  in
  helper (Legal st, None, []) st.blocks

let modify_player_helper
    (player : player)
    new_loc
    pushed_player
    dir
    room
    st
    result
    player_list_updated =
  let modified_player, if_move_legal =
    ( updated_player player (new_pos new_loc dir) room,
      (not (collide new_loc dir room)) && not (break st new_loc dir) )
  in
  if if_move_legal then
    ( result,
      Some modified_player,
      modified_player :: player_list_updated )
  else
    ( Illegal,
      Some modified_player,
      modified_player :: player_list_updated )

(** Returns a tuple of (result, player_pushed, new_player_list) given
    that the pushed(moved) object in this round is a player *)
let pushed_player_new_player_list
    (room : room)
    (dir : direction)
    (new_loc : int * int)
    (pushed_player : player)
    (st : state) : result * player option * player list =
  let rec helper
      (init : result * player option * player list)
      (player_list : player list) =
    match player_list with
    | [] -> init
    | player :: tl -> (
        match helper init tl with
        | result, pushed_player_op, player_list_updated ->
            let if_move_player =
              player.position = new_loc
              && player.player_num != pushed_player.player_num
              && player.on_exit = false
            in
            if if_move_player then
              modify_player_helper player new_loc pushed_player dir room
                st result player_list_updated
            else
              (result, pushed_player_op, player :: player_list_updated))
  in
  helper (Legal st, None, []) st.players

let pushed_block_new_block_moveable_helper
    result
    st
    new_loc
    dir
    block
    room
    is_sliding =
  let modified_block =
    new_block block (new_pos new_loc dir) room.holes
  in
  match result with
  | result, pushed_block, block_list_updated ->
      let moveable =
        if is_sliding then move_blocks_sliding new_loc dir st
        else move_blocks new_loc dir st
      in
      if moveable then
        ( result,
          Some modified_block,
          modified_block :: block_list_updated )
      else
        ( Illegal,
          Some modified_block,
          modified_block :: block_list_updated )

let pushed_block_block_branch_helper
    helper
    is_sliding
    acc
    init
    tl
    st
    new_loc
    dir
    block
    room =
  let acc_compare = if is_sliding then 1 else 0 in
  if acc = acc_compare then
    let result =
      if is_sliding then helper (acc - 1) init tl
      else helper acc init tl
    in
    pushed_block_new_block_moveable_helper result st new_loc dir block
      room is_sliding
  else
    match
      if is_sliding then helper acc init tl
      else helper (acc - 1) init tl
    with
    | result', pushed_block', block_list_updated' ->
        (result', pushed_block', block :: block_list_updated')

(** Returns a tuple of (result, block_pushed, new block list) given that
    the pushed(moved) object in this round is a block *)
let pushed_block_new_block_list room new_loc dir st :
    result * block option * block list =
  let is_sliding = st.mode = Sliding in
  let rec helper acc init (block_list : block list) =
    match block_list with
    | [] -> init
    | block :: tl -> (
        if block.position = new_loc then
          pushed_block_block_branch_helper helper is_sliding acc init tl
            st new_loc dir block room
        else
          match helper acc init tl with
          | result'', pushed_block'', block_list_updated'' ->
              (result'', pushed_block'', block :: block_list_updated''))
  in
  helper 1 (Legal st, None, []) st.blocks

let modify_player_helper'
    new_loc
    dir
    room
    st
    block_legal
    result
    modified_player
    player_list_updated =
  let if_legal =
    (not (collide new_loc dir room))
    && (not (break st new_loc dir))
    && block_legal
  in
  if if_legal then
    ( result,
      Some modified_player,
      modified_player :: player_list_updated )
  else
    ( Illegal,
      Some modified_player,
      modified_player :: player_list_updated )

(** Returns a tuple of (result, player_pushed, new_player_list) given
    that the pushed(moved) object in this round is a block *)
let pushed_block_new_player_list
    (room : room)
    (dir : direction)
    (new_loc : int * int)
    (st : state) : result * player option * player list =
  let is_sliding = st.mode = Sliding in
  let rec helper
      (init : result * player option * player list)
      (player_list : player list) =
    match player_list with
    | [] -> init
    | player :: tl -> (
        match helper init tl with
        | result, pushed_player, player_list_updated ->
            if player.position = new_loc then
              let modified_player, block_legal =
                ( updated_player player (new_pos new_loc dir) room,
                  if is_sliding then
                    block_legal_sliding room new_loc st dir
                  else true )
              in
              modify_player_helper' new_loc dir room st block_legal
                result modified_player player_list_updated
            else (result, pushed_player, player :: player_list_updated))
  in
  helper (Legal st, None, []) st.players

(** Returns the new state given the updated player list [players], the
    updated block list [blocks] and the updated #
    filled_holes[filled_holes] *)
let gen_state players blocks filled_holes st =
  { st with players; blocks; filled_holes }

(** Returns a turple of updated player list and updated block list after
    the movement given that the pushed(moved) object is a player *)
let move_game_object_helper_player (player : player) dir st room :
    player list * block list =
  let players =
    match
      pushed_player_new_player_list room dir player.position player st
    with
    | _, _, player_list_updated -> player_list_updated
  in
  let blocks =
    match pushed_player_new_block_list room player.position dir st with
    | _, _, block_list_updated -> block_list_updated
  in
  (players, blocks)

(** Returns a turple of updated player list and updated block list after
    the movement given that the pushed(moved) object is a block *)
let move_game_object_helper_block (block : block) dir st room :
    player list * block list =
  let players =
    match pushed_block_new_player_list room dir block.position st with
    | _, _, player_list_updated -> player_list_updated
  in
  let blocks =
    match pushed_block_new_block_list room block.position dir st with
    | _, _, block_list_updated -> block_list_updated
  in
  (players, blocks)

(**Returns the new state after the movement in the direction [dir] given
   the pushed(moved) [game_object] *)
let move_game_object game_object dir st room : state =
  match game_object with
  | Player player ->
      let players, blocks =
        move_game_object_helper_player player dir st room
      in
      gen_state players blocks (update_filled_holes blocks) st
  | Block block ->
      let players, blocks =
        move_game_object_helper_block block dir st room
      in
      gen_state players blocks (update_filled_holes blocks) st
  | _ -> raise (Failure "Not possible")

(** Returns a turple of (result, next_pushed_object) with the movement
    in direction [dir] given that the pushed(moved) object is a player *)
let check_next_obj_player (pushed_player : player) st room dir :
    result * game_object option =
  match
    pushed_player_new_block_list room pushed_player.position dir st
  with
  | Illegal, Some block, _ -> (Illegal, Some (Block block))
  | Legal _, Some block, _ -> (Legal st, Some (Block block))
  | Legal _, None, _ -> (
      match
        pushed_player_new_player_list room dir pushed_player.position
          pushed_player st
      with
      | Illegal, Some player, _ -> (Illegal, Some (Player player))
      | Legal _, Some player, _ -> (Legal st, Some (Player player))
      | Legal _, None, _ -> (Legal st, None)
      | _ -> raise (Failure "Impossible"))
  | _ -> raise (Failure "Impossible")

(** Returns a tuple of (result, next_pushed_object) with the movement in
    direction [dir] given that the pushed(moved) object is a block *)
let check_next_obj_block (pushed_block : block) st room dir :
    result * game_object option =
  match
    pushed_block_new_block_list room pushed_block.position dir st
  with
  | Illegal, Some block, _ -> (Illegal, Some (Block block))
  | Illegal, None, _ -> (Illegal, None)
  | Legal _, Some block, _ -> (Legal st, Some (Block block))
  | Legal _, None, _ -> (
      match
        pushed_block_new_player_list room dir pushed_block.position st
      with
      | Illegal, Some player, _ -> (Illegal, Some (Player player))
      | Legal _, Some player, _ -> (Legal st, Some (Player player))
      | Legal _, None, _ -> (Legal st, None)
      | _ -> raise (Failure "Impossible"))

(** Returns a tuple of (result, next_pushed_object) with the movement in
    direction [dir] *)
let check_next_obj game_object st room dir : result * game_object option
    =
  match game_object with
  | Player pushed_player ->
      check_next_obj_player pushed_player st room dir
  | Block pushed_block -> check_next_obj_block pushed_block st room dir
  | _ -> raise (Failure "Not_possible")

(** Returns the result given the pushed object in the last round of move
    [pushed_obj] *)
let rec move_rec
    (st : state)
    (dir : direction)
    (pushed_obj : game_object) : result =
  let room = get_room_by_id st.current_room_id st in
  let is_sliding = st.mode = Sliding in
  match check_next_obj pushed_obj st room dir with
  | result, None -> Legal st
  | result, Some obj -> (
      match result with
      | Illegal -> if is_sliding then Legal st else Illegal
      | Legal st ->
          let state_before_check =
            move_game_object pushed_obj dir st room
          in
          if check_win room state_before_check then
            next_level state_before_check
          else move_rec state_before_check dir obj)

(** Returns the updated state in the primary round of move(the first
    round before recursive moves) *)
let primary_move_update_state (st : state) room loc num pl_lst bl_lst =
  let limit =
    if st.mode = Limit then st.steps_left - 1 else st.steps_left
  in
  {
    st with
    players = update_player { st with players = pl_lst } loc room num;
    blocks = bl_lst;
    filled_holes = update_filled_holes bl_lst;
    steps_left = limit;
  }

(** Checks whether to call [move_rec] and what goes into the
    [pushed_obj] argument of the [move_rec] function, based on the
    [pushed_player] and [pushed_block] in this round *)
let checker_move_rec pushed_player pushed_block state_before_check dir =
  match (pushed_block, pushed_player) with
  | Some block, None -> move_rec state_before_check dir (Block block)
  | None, Some player -> move_rec state_before_check dir (Player player)
  | None, None -> Legal state_before_check
  | _ -> raise (Failure "Impossible")

(** Checks whether the movement of the player is legal *)
let check_player_notlegal pos st dir room =
  collide pos dir room || break st pos dir

(** Returns a tuple of (result, pushed_player, pushed_block,
    new_player_list, new_block_list) given the [player] before move and
    the [dir] *)
let check_pushed_legal current_rm new_loc player dir st =
  let legal_player_double_push, pushed_player, new_player_list =
    let updated_player = updated_player player new_loc current_rm in
    pushed_player_new_player_list current_rm dir new_loc updated_player
      st
  in
  let legal_block_push, pushed_block, new_block_list =
    pushed_player_new_block_list current_rm new_loc dir st
  in
  match (legal_player_double_push, legal_block_push) with
  | Illegal, _ | _, Illegal ->
      ( Illegal,
        pushed_player,
        pushed_block,
        new_player_list,
        new_block_list )
  | _ ->
      ( Legal st,
        pushed_player,
        pushed_block,
        new_player_list,
        new_block_list )

(** [move st dir room player_num] returns a [result] given the current
    state [st], direction [dir], room [room], and player_num
    [player_num]. If the movement is allowed, it returns a result
    [Legal state] with the new state. If the movement is not allowed, it
    returns [Illegal]. *)
let move
    (st : state)
    (dir : direction)
    (room : room)
    (player_num : player_num) : result =
  let current_rm = get_room_by_id st.current_room_id st in
  let player = determine_player_num st player_num in
  if check_player_notlegal player.position st dir current_rm then
    Illegal
  else
    let new_loc = new_pos player.position dir in
    let if_legal, pushed_player, pushed_block, pl_lst, bl_lst =
      check_pushed_legal current_rm new_loc player dir st
    in
    match if_legal with
    | Illegal -> Illegal
    | Legal _ ->
        let state_before_check =
          primary_move_update_state st current_rm new_loc player_num
            pl_lst bl_lst
        in
        if check_win room state_before_check then
          next_level state_before_check
        else if state_before_check.steps_left = 0 then
          no_steps state_before_check
        else
          checker_move_rec pushed_player pushed_block state_before_check
            dir
