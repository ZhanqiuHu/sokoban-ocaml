open Types
open Genmap

(* Map 2 *)
let path_val = { position = (0, 0); ttype = Obstacle }

let path_pos_list =
  [
    (1, 1);
    (1, 2);
    (1, 8);
    (2, 2);
    (2, 1);
    (3, 1);
    (4, 1);
    (5, 1);
    (5, 3);
    (5, 2);
    (4, 2);
    (3, 2);
    (5, 4);
    (8, 8);
    (7, 1);
  ]

let rec loop lo hi acc constant tuple_pos =
  if lo > hi then acc
  else if tuple_pos = "fst" then
    loop (lo + 1) hi ((lo, constant) :: acc) constant tuple_pos
  else loop (lo + 1) hi ((constant, lo) :: acc) constant tuple_pos

let generate_path_pos_labyrinth =
  let path_seg_1 = loop 0 12 [] 1 "snd" in
  let path_seg_2 = loop 1 13 path_seg_1 12 "fst" in
  let path_seq_3 = loop 1 12 path_seg_2 13 "snd" in
  let path_seq_4 = loop 4 13 path_seq_3 1 "fst" in
  let path_seq_5 = loop 1 9 path_seq_4 4 "snd" in
  let path_seq_6 = loop 4 10 path_seq_5 9 "fst" in
  let path_seq_7 = loop 4 9 path_seq_6 10 "snd" in
  let path_seq_8 = loop 7 10 path_seq_7 4 "fst" in
  let path_seq_9 = loop 3 5 path_seq_8 7 "snd" in
  let path_seq_10 = loop 7 9 path_seq_9 5 "fst" in
  loop 3 7 path_seq_10 8 "snd"

(* let map3 = { room_id = "labyrinth"; width = 15; height = 15;
   map_tile_list = map_to_list (Genmap.set_with_same_pos map_labyrinth 7
   3 { position = (7, 3); ttype = Exit }); init_blocks = [ { position =
   (1, 1); in_hole = false }; { position = (1, 2); in_hole = false }; ];
   (*{ position = (3, 2); in_hole = false } *) holes = [ { position =
   (8, 3) }; { position = (8, 7) } ]; num_holes = 2; exit_pos = (7, 3);
   init_pos = (0, 0); init_breaks = [ { position = (1, 13); hp = 1 }; {
   position = (14, 12); hp = 1 }; { position = (3, 1); hp = 1 }; {
   position = (4, 10); hp = 1 }; { position = (11, 9); hp = 1 }; {
   position = (10, 3); hp = 1 }; { position = (6, 4); hp = 1 }; {
   position = (7, 6); hp = 1 }; ]; step_limit = 20; } *)

let tile_val = { position = (0, 0); ttype = Normal }

let map_w = 20

let map_h = 10

let path_pos_list2 =
  generate_path_pos [ (12, 8); (11, 4) ] [ (1, 7) ]
    [ (2, 2); (3, 2); (4, 6) ]
    [ (1, 1) ]

let map_array2 =
  let bound_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_prob = 0.25 in
  let map =
    new_map_with_obstacles map_w map_h tile_val bound_val path_val
      obstacle_val path_pos_list2 obstacle_prob
  in
  map

let map_test =
  let bound_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_prob = 0.0 in
  let map =
    new_map_with_obstacles map_w map_h tile_val bound_val path_val
      obstacle_val [] obstacle_prob
  in
  map

let breakable_pos_list =
  generate_breakables map_array2 path_pos_list path_val tile_val 0.2

let breakable_list = init_breakables breakable_pos_list 1

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
    (*{ position = (3, 2); in_hole = false } *)
    holes = [ { position = (3, 3) } ];
    num_holes = 1;
    exit_pos = (1, 1);
    init_pos = (3, 1);
    init_breaks = [ { position = (4, 1); hp = 1 } ];
    step_limit = 15;
  }

let map2 =
  {
    room_id = "random";
    width = map_w;
    height = map_h;
    map_tile_list =
      map_to_list
        (Genmap.set_with_same_pos map_array2 1 7
           { position = (1, 7); ttype = Exit });
    init_blocks =
      [
        { position = (2, 2); in_hole = false };
        { position = (3, 2); in_hole = false };
        { position = (4, 6); in_hole = false };
      ];
    (*{ position = (3, 2); in_hole = false } *)
    holes = [ { position = (12, 8) }; { position = (11, 4) } ];
    num_holes = 2;
    exit_pos = (1, 7);
    init_pos = (1, 1);
    init_breaks = breakable_list;
    step_limit = 100;
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
    width = 10;
    height = 10;
    init_blocks = [];
    holes = [];
    num_holes = 1;
    exit_pos = (100, 100);
    init_pos = (1, 1);
    init_breaks = [];
    step_limit = 1000;
    map_tile_list =
      [
        (*First row*)
        [
          { position = (0, 0); ttype = Obstacle };
          { position = (1, 0); ttype = Obstacle };
          { position = (2, 0); ttype = Obstacle };
          { position = (3, 0); ttype = Obstacle };
          { position = (4, 0); ttype = Obstacle };
          { position = (5, 0); ttype = Obstacle };
          { position = (6, 0); ttype = Obstacle };
          { position = (7, 0); ttype = Obstacle };
          { position = (8, 0); ttype = Obstacle };
          { position = (9, 0); ttype = Obstacle };
        ];
        [
          { position = (0, 1); ttype = Obstacle };
          { position = (1, 1); ttype = Normal };
          { position = (2, 1); ttype = Normal };
          { position = (3, 1); ttype = Normal };
          { position = (4, 1); ttype = Normal };
          { position = (5, 1); ttype = Normal };
          { position = (6, 1); ttype = Normal };
          { position = (7, 1); ttype = Normal };
          { position = (8, 1); ttype = Normal };
          { position = (9, 1); ttype = Obstacle };
        ];
        [
          { position = (0, 2); ttype = Obstacle };
          { position = (1, 2); ttype = Normal };
          { position = (2, 2); ttype = Normal };
          { position = (3, 2); ttype = Normal };
          { position = (4, 2); ttype = Normal };
          { position = (5, 2); ttype = Normal };
          { position = (6, 2); ttype = Normal };
          { position = (7, 2); ttype = Normal };
          { position = (8, 2); ttype = Normal };
          { position = (9, 2); ttype = Obstacle };
        ];
        [
          { position = (0, 3); ttype = Obstacle };
          { position = (1, 3); ttype = Normal };
          { position = (2, 3); ttype = Normal };
          { position = (3, 3); ttype = Normal };
          { position = (4, 3); ttype = Normal };
          { position = (5, 3); ttype = Normal };
          { position = (6, 3); ttype = Normal };
          { position = (7, 3); ttype = Normal };
          { position = (8, 3); ttype = Normal };
          { position = (9, 3); ttype = Obstacle };
        ];
        [
          { position = (0, 4); ttype = Obstacle };
          { position = (1, 4); ttype = Normal };
          { position = (2, 4); ttype = Normal };
          { position = (3, 4); ttype = Normal };
          { position = (4, 4); ttype = Normal };
          { position = (5, 4); ttype = Normal };
          { position = (6, 4); ttype = Normal };
          { position = (7, 4); ttype = Normal };
          { position = (8, 4); ttype = Normal };
          { position = (9, 4); ttype = Obstacle };
        ];
        [
          { position = (0, 5); ttype = Obstacle };
          { position = (1, 5); ttype = Normal };
          { position = (2, 5); ttype = Normal };
          { position = (3, 5); ttype = Normal };
          { position = (4, 5); ttype = Normal };
          { position = (5, 5); ttype = Normal };
          { position = (6, 5); ttype = Normal };
          { position = (7, 5); ttype = Normal };
          { position = (8, 5); ttype = Normal };
          { position = (9, 5); ttype = Obstacle };
        ];
        [
          { position = (0, 6); ttype = Obstacle };
          { position = (1, 6); ttype = Normal };
          { position = (2, 6); ttype = Normal };
          { position = (3, 6); ttype = Normal };
          { position = (4, 6); ttype = Normal };
          { position = (5, 6); ttype = Normal };
          { position = (6, 6); ttype = Normal };
          { position = (7, 6); ttype = Normal };
          { position = (8, 6); ttype = Normal };
          { position = (9, 6); ttype = Obstacle };
        ];
        [
          { position = (0, 7); ttype = Obstacle };
          { position = (1, 7); ttype = Normal };
          { position = (2, 7); ttype = Normal };
          { position = (3, 7); ttype = Normal };
          { position = (4, 7); ttype = Normal };
          { position = (5, 7); ttype = Normal };
          { position = (6, 7); ttype = Normal };
          { position = (7, 7); ttype = Normal };
          { position = (8, 7); ttype = Normal };
          { position = (9, 7); ttype = Obstacle };
        ];
        [
          { position = (0, 8); ttype = Obstacle };
          { position = (1, 8); ttype = Normal };
          { position = (2, 8); ttype = Normal };
          { position = (3, 8); ttype = Normal };
          { position = (4, 8); ttype = Normal };
          { position = (5, 8); ttype = Normal };
          { position = (6, 8); ttype = Normal };
          { position = (7, 8); ttype = Normal };
          { position = (8, 8); ttype = Normal };
          { position = (9, 8); ttype = Obstacle };
        ];
        [
          { position = (0, 9); ttype = Obstacle };
          { position = (1, 9); ttype = Obstacle };
          { position = (2, 9); ttype = Obstacle };
          { position = (3, 9); ttype = Obstacle };
          { position = (4, 9); ttype = Obstacle };
          { position = (5, 9); ttype = Obstacle };
          { position = (6, 9); ttype = Obstacle };
          { position = (7, 9); ttype = Obstacle };
          { position = (8, 9); ttype = Obstacle };
          { position = (9, 9); ttype = Obstacle };
        ];
      ];
  }

let lose = { win with room_id = "lose" }
