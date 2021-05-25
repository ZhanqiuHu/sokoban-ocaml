open Types
open Genmap

(* Definition of types and constants *)
let path_val = { position = (0, 0); ttype = Obstacle }

let tile_val = { position = (0, 0); ttype = Normal }

let bound_val = { position = (0, 0); ttype = Obstacle }

let obstacle_val = { position = (0, 0); ttype = Obstacle }

let map_w = 20

let map_h = 10

(* Map 2 : generated using functions in genmap.ml *)

(** Values defined for Map 2*)
let exit_position2 = (1, 7)

let init_position2 = (1, 1)

let hole_pos_list2 = [ (18, 8); (17, 8) ]

let block_pos_list2 = [ (2, 2); (3, 2); (4, 6) ]

let predefined_path2 = []

let obstacle_prob2 = 0.2

let breakable_prob2 = 0.2

let breakable_hp2 = 1

let room_id2 = "random"

let step_limit2 = 200

(** Start generating Map 2*)
let path_pos_list2 =
  generate_path_pos [ exit_position2 ] [ init_position2 ] hole_pos_list2
    block_pos_list2
  @ predefined_path2

let map_array2 =
  let map =
    new_map_with_obstacles map_w map_h tile_val bound_val path_val
      obstacle_val path_pos_list2 obstacle_prob2
  in
  map

let breakable_pos_list2 =
  generate_breakables map_array2 path_pos_list2 path_val tile_val
    breakable_prob2

let breakable_list2 = init_breakables breakable_pos_list2 breakable_hp2

let map2 =
  {
    room_id = room_id2;
    width = map_w;
    height = map_h;
    map_tile_list =
      map_to_list
        (Genmap.set_with_same_pos map_array2 (fst exit_position2)
           (snd exit_position2)
           { position = exit_position2; ttype = Exit });
    init_blocks = init_blocks block_pos_list2;
    holes = init_holes hole_pos_list2;
    num_holes = List.length hole_pos_list2;
    exit_pos = exit_position2;
    init_pos = init_position2;
    init_breaks = breakable_list2;
    step_limit = step_limit2;
  }

(** Testing Map: use for testing *)

let map_test =
  let bound_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_prob = 0.0 in
  let map =
    new_map_with_obstacles map_w map_h tile_val bound_val path_val
      obstacle_val [] obstacle_prob
  in
  map

let testing_map =
  {
    room_id = "test";
    width = 6;
    height = 6;
    map_tile_list =
      map_to_list
        (Genmap.set_with_same_pos map_test 1 1
           { position = (1, 1); ttype = Exit });
    init_blocks =
      [
        { position = (2, 2); in_hole = false };
        { position = (3, 2); in_hole = false };
      ];
    holes = [ { position = (3, 3) } ];
    num_holes = 1;
    exit_pos = (1, 1);
    init_pos = (3, 1);
    init_breaks = [ { position = (4, 1); hp = 1 } ];
    step_limit = 3;
  }

(* Map 3 *)
let path_val3 = { position = (0, 0); ttype = Obstacle }

let init_block_pos3 = [ (16, 7); (12, 4) ]

let init_hole_pos3 = [ (6, 6); (15, 8) ]

let exit_pos3 = (18, 8)

let path_pos_list3 =
  generate_path_pos [ exit_pos3 ] [ (1, 1) ] init_block_pos3
    init_hole_pos3

let tile_val3 = { position = (0, 0); ttype = Normal }

let map_array3 =
  let map_w = 20 in
  let map_h = 10 in
  let bound_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_prob = 0.2 in
  let map =
    new_map_with_obstacles map_w map_h tile_val bound_val path_val
      obstacle_val path_pos_list3 obstacle_prob
  in
  map

let breakable_pos_list3 =
  generate_breakables map_array3 path_pos_list3 path_val3 tile_val3 0.3

let breakable_list3 = init_breakables breakable_pos_list3 1

let map3 =
  {
    room_id = "random3";
    width = 20;
    height = 10;
    map_tile_list =
      map_to_list
        (Genmap.set_with_same_pos map_array3 (fst exit_pos3)
           (snd exit_pos3)
           { position = (0, 0); ttype = Exit });
    init_blocks = init_blocks init_block_pos3;
    holes = init_holes init_hole_pos3;
    num_holes = List.length init_hole_pos3;
    exit_pos = exit_pos3;
    init_pos = (1, 1);
    init_breaks = breakable_list3;
    step_limit = 100;
  }

let win =
  {
    room_id = "win";
    width = map_w;
    height = map_h;
    init_blocks = [];
    holes = [];
    num_holes = 1;
    exit_pos = (100, 100);
    init_pos = (1, 1);
    init_breaks = [];
    step_limit = 1000;
    map_tile_list =
      init_boundary
        (map_init map_w map_h { position = (0, 0); ttype = Normal })
        { position = (0, 0); ttype = Obstacle }
      |> map_to_list;
  }

let lose = { win with room_id = "lose" }
