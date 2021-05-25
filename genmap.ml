open Types

type 'a mutable_pos_list = { mutable list : 'a list }

type bool_box = { mutable bool_val : bool }

let rec init_breakables pos_list hp_val =
  match pos_list with
  | h :: t -> { position = h; hp = hp_val } :: init_breakables t hp_val
  | _ -> []

let rec init_blocks pos_list =
  match pos_list with
  | h :: t -> { position = h; in_hole = false } :: init_blocks t
  | _ -> []

let rec init_holes pos_list =
  match pos_list with
  | h :: t -> { position = h } :: init_holes t
  | _ -> []

let update_positions tile_map =
  let y_end = Array.length tile_map - 1 in
  let x_end = Array.length tile_map.(0) - 1 in
  for x = 0 to x_end do
    for y = 0 to y_end do
      tile_map.(y).(x) <-
        { position = (x, y_end - y); ttype = tile_map.(y).(x).ttype }
    done
  done;
  tile_map

let map_init map_w map_h init_val =
  update_positions (Array.make_matrix map_h map_w init_val)

let random_bool true_prob =
  let random_val = Random.float 100. in
  if random_val <= 100. *. true_prob then true else false

let init_boundary (init_map : tile array array) bound_val =
  let y_end = Array.length init_map - 1 in
  let x_end = Array.length init_map.(0) - 1 in
  for x = 0 to x_end do
    for y = 0 to y_end do
      if x = 0 || x = x_end then
        init_map.(y).(x) <-
          {
            position = init_map.(y).(x).position;
            ttype = bound_val.ttype;
          }
      else if y = 0 || y = y_end then
        init_map.(y).(x) <-
          {
            position = init_map.(y).(x).position;
            ttype = bound_val.ttype;
          }
      else ()
    done
  done;
  init_map

let set (map : tile array array) x_pos y_pos new_val =
  let y_end = Array.length map - 1 in
  map.(y_end - y_pos).(x_pos) <- new_val;
  map

let get (map : tile array array) x_pos y_pos new_val =
  let y_end = Array.length map - 1 in
  map.(y_end - y_pos).(x_pos)

let set_with_same_pos (map : tile array array) x_pos y_pos new_val =
  let y_end = Array.length map - 1 in
  map.(y_end - y_pos).(x_pos) <-
    {
      position = map.(y_end - y_pos).(x_pos).position;
      ttype = new_val.ttype;
    };
  map

let rec set_pos_list_with_same_pos
    (map : tile array array)
    pos_list
    new_val =
  match pos_list with
  | h :: t ->
      let _ = set_with_same_pos map (fst h) (snd h) new_val in
      set_pos_list_with_same_pos map t new_val
  | [] -> ()

let get map x_pos y_pos =
  let y_end = Array.length map - 1 in
  map.(y_end - y_pos).(x_pos)

let copy_map map =
  let new_map = Array.copy map in
  let y_end = Array.length map - 1 in
  for y = 0 to y_end do
    map.(y) <- Array.copy map.(y)
  done;
  new_map

let define_path_map (map : tile array array) path_list path_val =
  let y_end = Array.length map - 1 in
  let map_copy = copy_map map in
  let rec set_path path_pos_list =
    match path_pos_list with
    | (hx, hy) :: t ->
        let _ =
          map_copy.(y_end - hy).(hx) <-
            {
              position = map_copy.(hy).(hx).position;
              ttype = path_val.ttype;
            }
        in
        set_path t
    | _ -> ()
  in
  let _ = set_path path_list in
  map_copy

let iterate map func =
  let y_end = Array.length map - 1 in
  let x_end = Array.length map.(0) - 1 in
  for x = 0 to x_end do
    for y = 0 to y_end do
      func x y
    done
  done

let choose_obstacles
    map
    path_lst
    path_val
    tile_val
    obstacle_val
    true_prob =
  let path_map = define_path_map map path_lst path_val in
  let func x y =
    if
      map.(y).(x).ttype = tile_val.ttype
      && path_map.(y).(x).ttype <> path_val.ttype
    then
      if random_bool true_prob then
        map.(y).(x) <-
          {
            position = map.(y).(x).position;
            ttype = obstacle_val.ttype;
          }
      else ()
    else ()
  in
  iterate map func

let generate_breakables map path_list path_val tile_val true_prob =
  let pos_list = { list = [] } in
  let path_map = define_path_map map path_list path_val in
  let y_end = Array.length map - 1 in
  let x_end = Array.length map.(0) - 1 in
  for x = 0 to x_end do
    for y = 0 to y_end do
      if
        map.(y).(x).ttype = tile_val.ttype
        && path_map.(y).(x).ttype <> path_val.ttype
      then
        if random_bool true_prob then
          pos_list.list <- (x, y_end - y) :: pos_list.list
        else ()
      else ()
    done
  done;
  pos_list.list

let map_to_list map =
  let array_list = Array.to_list map in
  List.map (fun arr -> Array.to_list arr) array_list

let new_map_with_obstacles
    map_w
    map_h
    tile_val
    bound_val
    path_val
    obstacle_val
    path_pos_list
    obstacle_prob =
  let map = map_init map_w map_h tile_val in
  let _ = init_boundary map bound_val in
  let _ =
    choose_obstacles map path_pos_list path_val tile_val obstacle_val
      obstacle_prob
  in
  map

let generate_path_pos pos_list =
  let path_list = { list = pos_list } in
  let rec generate_path_pos_helper pos_lst path_lst =
    match pos_lst with
    | (x1, y1) :: (x2, y2) :: t ->
        let x_start = min x1 x2 in
        let x_end = max x1 x2 in
        let y_start = min y1 y2 in
        let y_end = max y1 y2 in
        for x = x_start to x_end do
          path_lst.list <- (x, y_start) :: path_lst.list
        done;
        for y = y_start to y_end do
          path_lst.list <- (x_end, y) :: path_lst.list
        done;
        generate_path_pos_helper ((x2, y2) :: t) path_lst
    | _ -> path_lst.list
  in
  generate_path_pos_helper pos_list path_list

let rec check_valid_object_position map position_list =
  match position_list with
  | (x, y) :: d ->
      (get map x y).ttype = Normal && check_valid_object_position map d
  | [] -> true

let rec check_valid_boundary map =
  let bool_box = { bool_val = true } in
  let y_end = Array.length map - 1 in
  let x_end = Array.length map.(0) - 1 in
  for x = 0 to x_end do
    for y = 0 to y_end do
      bool_box.bool_val <-
        (get map x y).ttype = Obstacle && bool_box.bool_val
    done
  done;
  bool_box.bool_val
