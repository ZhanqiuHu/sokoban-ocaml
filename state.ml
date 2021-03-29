open Types
open Map

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
let get_player_pos room =
  let player_tile =
    room.map_tile_list |> List.flatten
    |> List.filter (fun tile ->
           match tile.ttype with Player _ -> true | _ -> false)
    |> List.hd
  in
  player_tile.position

let get_room_by_id room_id st =
  st.all_rooms
  |> List.filter (fun room -> room.room_id = room_id)
  |> List.hd

let init_state =
  {
    current_room_id = "beginning";
    (* should be [map1] *)
    all_rooms = [ map1 ];
    (* in map1, should initialize player position *)
    player_pos = (1, 1);
  }

let get_tile_list st =
  let rec get_room lst =
    match lst with
    | [] -> raise Not_found
    | h :: t -> if h.room_id = st.current_room_id then h else get_room t
  in
  (get_room st.all_rooms).map_tile_list

let get_tile_by_loc loc room =
  room.map_tile_list |> List.flatten
  |> List.filter (fun tile -> tile.position = loc)
  |> List.hd

let reach_boundary loc room =
  let tile_on = get_tile_by_loc loc room in
  match tile_on.ttype with
  | Hor_bound -> true
  | Ver_bound -> true
  | _ -> false

let update_loc_tile_to_player loc tile_list : tile list =
  match tile_list with
  | [] -> []
  | h :: t ->
      (if h.position = loc then
       { h with ttype = Player { on_exit = false } }
      else h)
      :: t

let update_loc_tile_to_normal loc tile_list : tile list =
  match tile_list with
  | [] -> []
  | h :: t ->
      (if h.position = loc then
       { h with ttype = Normal { is_hole = false } }
      else h)
      :: t

let list_to_nested_list room_width room_height lst =
  let rec loop_helper lst room_width n =
    match n with
    | 0 -> []
    | _ ->
        List.filteri
          (fun i item -> i / room_width = room_height - n)
          lst
        :: loop_helper lst room_width (n - 1)
  in
  loop_helper lst room_width room_height

let update_room_in_rm_list updated_room st : room list =
  match st.all_rooms with
  | [] -> []
  | h :: t ->
      (if h.room_id = st.current_room_id then updated_room else h) :: t

let update_location loc (dir : direction) =
  match dir with
  | Left -> (fst loc - 1, snd loc)
  | Right -> (fst loc + 1, snd loc)
  | Up -> (fst loc, snd loc - 1)
  | Down -> (fst loc, snd loc + 1)

type result =
  | Legal of state
  | Illegal

let move (st : state) (dir : direction) : result =
  let current_loc = st.player_pos in
  let new_loc = update_location current_loc dir in
  let current_rm = get_room_by_id st.current_room_id st in
  if reach_boundary new_loc current_rm then Illegal
  else
    let updated_room =
      {
        current_rm with
        map_tile_list =
          current_rm.map_tile_list |> List.flatten
          |> update_loc_tile_to_normal current_loc
          |> update_loc_tile_to_player new_loc
          |> list_to_nested_list current_rm.width current_rm.height;
      }
    in
    Legal
      {
        st with
        all_rooms = update_room_in_rm_list updated_room st;
        player_pos = new_loc;
      }
