open Types
open Genmap

type list_box = { mutable l : int * int list }

let map_try =
  let map_w = 10 in
  let map_h = 10 in
  let tile_val = { position = (0, 0); ttype = Normal } in
  let bound_val = { position = (0, 0); ttype = Obstacle } in
  let path_val = { position = (0, 0); ttype = Obstacle } in
  let obstacle_val = { position = (0, 0); ttype = Obstacle } in
  let path_pos_list =
    [
      (1, 1);
      (1, 2);
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
      (9, 9);
    ]
  in
  let obstacle_prob = 0.25 in
  let map =
    new_map_with_obstacles map_w map_h tile_val bound_val path_val
      obstacle_val path_pos_list obstacle_prob
  in
  map

let map2 =
  {
    room_id = "random";
    width = 10;
    height = 10;
    map_tile_list =
      map_to_list
        (Genmap.set_with_same_pos map_try 1 8
           { position = (1, 8); ttype = Exit });
    init_blocks = [ { position = (2, 2); in_hole = false } ];
    (*{ position = (3, 2); in_hole = false } *)
    holes = [ { position = (5, 4) } ];
    num_holes = 1;
    exit_pos = (1, 8);
    init_pos = (1, 1);
    init_breaks =
      [
        { position = (4, 1); hp = 1 };
        { position = (7, 1); hp = 1 };
        { position = (5, 3); hp = 1 };
        { position = (4, 2); hp = 1 };
        { position = (5, 7); hp = 1 };
        { position = (5, 8); hp = 1 };
        { position = (4, 6); hp = 1 };
      ];
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
