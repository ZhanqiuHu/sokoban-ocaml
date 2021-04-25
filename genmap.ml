open Types

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

let init_boundary init_map bound_val =
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

let set map x_pos y_pos new_val =
  let y_end = Array.length map - 1 in
  map.(y_end - y_pos).(x_pos) <- new_val;
  map

let copy_map map =
  let new_map = Array.copy map in
  let y_end = Array.length map - 1 in
  for y = 0 to y_end do
    map.(y) <- Array.copy map.(y)
  done;
  new_map

let define_path_map map path_list path_val =
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
    path_list
    path_val
    tile_val
    obstacle_val
    true_prob =
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
          map.(y).(x) <-
            {
              position = map.(y).(x).position;
              ttype = obstacle_val.ttype;
            }
        else ()
      else ()
    done
  done

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
  (* let _ = update_positions map in *)
  map

(* let map_try = let map_w = 10 in let map_h = 5 in let tile_val =
   "tile" in let bound_val = "bound" in let path_val = "path" in let
   obstacle_val = "obstacle" in let path_pos_list = [] in let
   obstacle_prob = 0.5 in new_map_with_obstacles map_w map_h tile_val
   bound_val path_val obstacle_val path_pos_list obstacle_prob *)
