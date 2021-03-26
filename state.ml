let move_speed = 1
let object_size = (1,1)
let player_size = (1,1)

!!! player_command 

type room = {
  height: float;
  width: float;
  object_list = [...];
}

type state =  {
  current_rm : room;
  player: player; 
}

type player = {
  location: location;
}

type location = {
  coordinate: float * float;
  room : room;
}

type portal = {
  location: location;
  next_room_to : location;
}

type object = 
| Tile of location
| Obstacle of location
| Portal of portal

let overlapping ((width1, height1), (x1,y1)) ((width2, height2), (x2,y2)) =
  let x1_min = x1 in
  let x1_max = x1 +. width1 in
  let y1_min = y1 in
  let y1_max = y1  +. height1 in

  let x2_min = x2 in
  let x2_max = x2 +. width2 in
  let y2_min = y2 in
  let y2_max = y2 +. height2 in

  let xs_overlap =
    if (x1_min >= x2_min && x1_min <= x2_max) ||
       (x2_min >= x1_min && x2_min <= x1_max)
    then true else false in
  let ys_overlap =
    if (y1_min >= y2_min && y1_min <= y2_max) ||
      ( y2_min >= y1_min && y2_min <= y1_max)
    then true else false in
  xs_overlap && ys_overlap

let rec get_obj_by_loc player loc object_list =
  match object_list with
  | [] -> Tile loc
  | h::t ->
    let overlap = (overlapping (player_size, h.location.coordinate) (object_size, h.location.coordinate)) in
    if overlap then h else
    get_obj_by_loc player loc t

let is_outside loc room_height room_width =
  fst loc < 0. || snd loc < 0. || fst loc > room_width || snd loc < room_height


let update_coordinate command player = 
  let player_cord = player.location.coordinate
  let x_delta = 
    if command.a then fst player_cord - move_speed
    else if command.d then fst player_cord + move_speed
    else 0.
  in 
  let y_delta = 
    if command.s then snd player_cord - move_speed
    else if command.w then snd player_cord + move_speed
    else 0.
  in ((fst player_cord + x_delta),(snd player_cord + y_delta))



let move command player state=
  let updated_coord = update_coordinate command player in
  if is_outside updated_coord state.current_rm.height state.current_rm.width 
    then player.location
else loc = {player.location with coordinate = updated_coord} in 
  target_obj = get_obj_by_loc player loc state.current_rm.object_list in
      match target_obj with
      | Tile t -> loc
      | Portal p -> p.next_room_to
      | Obstacle _ -> player.location


let dir_key_pressed command =
  command.w || command.a || command.s || command.d


let update_location command player state =
  if dir_key_pressed command then
  move command player state
  else player.location


let do' state =
  let updated_loc = update_location player_command state.player state in
  let updated_player = {state.player with location = updated_loc} in
  let updated_room = updated_loc.room in
  {
    state with 
    current_rm = updated_room;
    player = updated_player;
  }

