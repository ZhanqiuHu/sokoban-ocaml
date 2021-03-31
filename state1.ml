(* let move_speed = 1 let object_size = (1,1) let player_size = (1,1)

   (* !!! player_command *) type object = | Tile of location | Obstacle
   of location | Portal of portal

   type block = {

   }

   type location = { coordinate: float * float; room : room; }

   type portal = { location: location; next_room_to : location; } type
   room = { height: float; width: float; object_list object list; }

   type state = { current_rm : room; player: player; }

   type player = { location: location; }

   let overlapping ((width1, height1), (x1,y1)) ((width2, height2),
   (x2,y2)) = let x1_min = x1 in let x1_max = x1 +. width1 in let y1_min
   = y1 in let y1_max = y1 +. height1 in

   let x2_min = x2 in let x2_max = x2 +. width2 in let y2_min = y2 in
   let y2_max = y2 +. height2 in

   let xs_overlap = if (x1_min >= x2_min && x1_min <= x2_max) || (x2_min
   >= x1_min && x2_min <= x1_max) then true else false in let ys_overlap
   = if (y1_min >= y2_min && y1_min <= y2_max) || ( y2_min >= y1_min &&
   y2_min <= y1_max) then true else false in xs_overlap && ys_overlap

   let rec get_obj_by_loc player loc object_list = match object_list
   with | [] -> Tile loc | h::t -> let overlap = (overlapping
   (player_size, h.location.coordinate) (object_size,
   h.location.coordinate)) in if overlap then h else get_obj_by_loc
   player loc t

   let is_outside loc room_height room_width = fst loc < 0. || snd loc <
   0. || fst loc > room_width || snd loc < room_height

   let update_coordinate command player = let player_cord =
   player.location.coordinate let x_delta = if command.a then fst
   player_cord - move_speed else if command.d then fst player_cord +
   move_speed else 0. in let y_delta = if command.s then snd player_cord
   - move_speed else if command.w then snd player_cord + move_speed else
   0. in ((fst player_cord + x_delta),(snd player_cord + y_delta))

   let move command player state= let updated_coord = update_coordinate
   command player in if is_outside updated_coord state.current_rm.height
   state.current_rm.width then player.location else loc =
   {player.location with coordinate = updated_coord} in target_obj =
   get_obj_by_loc player loc state.current_rm.object_list in match
   target_obj with | Tile t -> loc | Portal p -> p.next_room_to |
   Obstacle _ -> player.location

   let dir_key_pressed command = command.w || command.a || command.s ||
   command.d

   let update_location command player state = if dir_key_pressed command
   then move command player state else player.location

   let do' state = let updated_loc = update_location player_command
   state.player state in let updated_player = {state.player with
   location = updated_loc} in let updated_room = updated_loc.room in {
   state with current_rm = updated_room; player = updated_player; }

   (* (* better change on_exit to on_hole *) type player = { on_exit :
   bool }

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

   (* [get_player_pos room] is the pos of tile in [room] that is of
   ttype Player. *) let get_player_pos room = let player_tile =
   room.map_tile_list |> List.flatten |> List.filter (fun tile -> match
   tile.ttype with | Player _ -> true | _ -> false ) |> List.hd in
   player_tile.position

   let get_room_by_id room_id st = st.all_rooms |> List.filter (fun room
   -> room.room_id = room_id) |> List.hd

   let init_state = { current_room_id = "beginning"; (* should be [map1]
   *) all_rooms = []; (* in map1, should initialize player position *)
   player_pos = (1,1); }

   let get_tile_by_loc loc room = room.map_tile_list |> List.flatten |>
   List.filter (fun tile -> tile.position = loc) |> List.hd

   let reach_boundary loc room = let tile_on = get_tile_by_loc loc room
   in match tile_on.ttype with | Hor_bound -> true | Ver_bound -> true |
   _ -> false

   let update_loc_tile_to_player loc tile_list : tile list = match
   tile_list with | [] -> [] | h :: t -> (if h.position = loc then {h
   with ttype = Player {on_exit = false}} else h ) :: t

   let update_loc_tile_to_normal loc tile_list : tile list = match
   tile_list with | [] -> [] | h :: t -> (if h.position = loc then {h
   with ttype = Normal {is_hole = false}} else h ) :: t

   let list_to_nested_list room_width room_height lst = let rec
   loop_helper lst room_width n = match n with | 0 -> [] | _ ->
   List.filteri (fun i item -> i/room_width=room_height-n) lst ::
   loop_helper lst room_width (n-1) in (loop_helper lst room_width
   room_height)

   let update_room_in_rm_list updated_room st : room list = match
   st.all_rooms with | [] -> [] | h :: t -> (if h.room_id =
   st.current_room_id then updated_room else h) :: t

   let update_location loc (dir: direction) = match dir with | Left ->
   ((fst loc - 1), snd loc) | Right -> ((fst loc + 1), snd loc) | Up ->
   (fst loc, (snd loc - 1)) | Down -> (fst loc, (snd loc + 1))

   let move (st : state) (dir: direction) : state = let current_loc =
   st.player_pos in let new_loc = update_location current_loc dir in let
   current_rm = get_room_by_id st.current_room_id st in if
   reach_boundary new_loc current_rm then st else let updated_room =
   {current_rm with map_tile_list = current_rm.map_tile_list |>
   List.flatten |> update_loc_tile_to_normal current_loc |>
   update_loc_tile_to_player new_loc |> list_to_nested_list
   current_rm.width current_rm.height } in { st with all_rooms =
   update_room_in_rm_list updated_room st; player_pos = new_loc; } *)
