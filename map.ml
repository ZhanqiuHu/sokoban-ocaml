open Types

let map1 =
  {
    room_id = "beginning";
    width = 10;
    height = 10;
    init_blocks = [ { position = (2, 2); in_hole = false } ];
    holes = [ { position = (3, 3) } ];
    num_holes = 1;
    exit_pos = (1, 10);
    init_pos = (1, 1);
    map_tile_list =
      [
        { position = (0, 0); ttype = Obstacle };
        { position = (0, 1); ttype = Obstacle };
        { position = (0, 2); ttype = Obstacle };
        { position = (0, 3); ttype = Obstacle };
        { position = (0, 4); ttype = Obstacle };
        { position = (0, 5); ttype = Obstacle };
        { position = (0, 6); ttype = Obstacle };
        { position = (0, 7); ttype = Obstacle };
        { position = (0, 8); ttype = Obstacle };
        { position = (0, 9); ttype = Obstacle };
        { position = (0, 10); ttype = Obstacle };
        (*new*)
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
        { position = (10, 0); ttype = Obstacle };
        (*new*)
        { position = (0, 10); ttype = Obstacle };
        { position = (1, 10); ttype = Obstacle };
        { position = (2, 10); ttype = Obstacle };
        { position = (3, 10); ttype = Obstacle };
        { position = (4, 10); ttype = Obstacle };
        { position = (5, 10); ttype = Obstacle };
        { position = (6, 10); ttype = Obstacle };
        { position = (7, 10); ttype = Obstacle };
        { position = (8, 10); ttype = Obstacle };
        { position = (9, 10); ttype = Obstacle };
        { position = (10, 10); ttype = Obstacle };
        (*new*)
        { position = (10, 0); ttype = Obstacle };
        { position = (10, 1); ttype = Obstacle };
        { position = (10, 2); ttype = Obstacle };
        { position = (10, 3); ttype = Obstacle };
        { position = (10, 4); ttype = Obstacle };
        { position = (10, 5); ttype = Obstacle };
        { position = (10, 6); ttype = Obstacle };
        { position = (10, 7); ttype = Obstacle };
        { position = (10, 8); ttype = Obstacle };
        { position = (10, 9); ttype = Obstacle };
        { position = (10, 10); ttype = Obstacle };
        (*inside*)
        { position = (1, 1); ttype = Normal };
        { position = (1, 2); ttype = Normal };
        { position = (1, 3); ttype = Normal };
        { position = (1, 4); ttype = Normal };
        { position = (1, 5); ttype = Normal };
        { position = (1, 6); ttype = Normal };
        { position = (1, 7); ttype = Normal };
        { position = (1, 8); ttype = Normal };
        { position = (1, 9); ttype = Normal };
        (*inside*)
        { position = (2, 1); ttype = Normal };
        { position = (2, 2); ttype = Normal };
        { position = (2, 3); ttype = Normal };
        { position = (2, 4); ttype = Normal };
        { position = (2, 5); ttype = Normal };
        { position = (2, 6); ttype = Normal };
        { position = (2, 7); ttype = Normal };
        { position = (2, 8); ttype = Normal };
        { position = (2, 9); ttype = Normal };
        (*inside*)
        { position = (3, 1); ttype = Normal };
        { position = (3, 2); ttype = Normal };
        { position = (3, 3); ttype = Normal };
        { position = (3, 4); ttype = Normal };
        { position = (3, 5); ttype = Normal };
        { position = (3, 6); ttype = Normal };
        { position = (3, 7); ttype = Normal };
        { position = (3, 8); ttype = Normal };
        { position = (3, 9); ttype = Normal };
        (*inside*)
        { position = (4, 1); ttype = Normal };
        { position = (4, 2); ttype = Normal };
        { position = (4, 3); ttype = Normal };
        { position = (4, 4); ttype = Normal };
        { position = (4, 5); ttype = Normal };
        { position = (4, 6); ttype = Normal };
        { position = (4, 7); ttype = Normal };
        { position = (4, 8); ttype = Normal };
        { position = (4, 9); ttype = Normal };
        (*inside*)
        { position = (5, 1); ttype = Normal };
        { position = (5, 2); ttype = Normal };
        { position = (5, 3); ttype = Normal };
        { position = (5, 4); ttype = Normal };
        { position = (5, 5); ttype = Normal };
        { position = (5, 6); ttype = Normal };
        { position = (5, 7); ttype = Normal };
        { position = (5, 8); ttype = Normal };
        { position = (5, 9); ttype = Normal };
        (*inside*)
        { position = (6, 1); ttype = Normal };
        { position = (6, 2); ttype = Normal };
        { position = (6, 3); ttype = Normal };
        { position = (6, 4); ttype = Normal };
        { position = (6, 5); ttype = Normal };
        { position = (6, 6); ttype = Normal };
        { position = (6, 7); ttype = Normal };
        { position = (6, 8); ttype = Normal };
        { position = (6, 9); ttype = Normal };
        (*inside*)
        { position = (7, 1); ttype = Normal };
        { position = (7, 2); ttype = Normal };
        { position = (7, 3); ttype = Normal };
        { position = (7, 4); ttype = Normal };
        { position = (7, 5); ttype = Normal };
        { position = (7, 6); ttype = Normal };
        { position = (7, 7); ttype = Normal };
        { position = (7, 8); ttype = Normal };
        { position = (7, 9); ttype = Normal };
        (*inside*)
        { position = (8, 1); ttype = Normal };
        { position = (8, 2); ttype = Normal };
        { position = (8, 3); ttype = Normal };
        { position = (8, 4); ttype = Normal };
        { position = (8, 5); ttype = Normal };
        { position = (8, 6); ttype = Normal };
        { position = (8, 7); ttype = Normal };
        { position = (8, 8); ttype = Normal };
        { position = (8, 9); ttype = Normal };
        (*inside*)
        { position = (9, 1); ttype = Normal };
        { position = (9, 2); ttype = Normal };
        { position = (9, 3); ttype = Normal };
        { position = (9, 4); ttype = Normal };
        { position = (9, 5); ttype = Normal };
        { position = (9, 6); ttype = Normal };
        { position = (9, 7); ttype = Normal };
        { position = (9, 8); ttype = Normal };
        { position = (9, 9); ttype = Normal };
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
    exit_pos = (1, 10);
    init_pos = (1, 1);
    map_tile_list = [ { position = (0, 0); ttype = Normal } ];
  }

(* let map1 = { room_id = "beginning"; width = 10; height = 10;
   map_tile_list = [ (*First row*) [ { position = (0, 0); ttype =
   Hor_bound }; { position = (1, 0); ttype = Hor_bound }; { position =
   (2, 0); ttype = Hor_bound }; { position = (3, 0); ttype = Hor_bound
   }; { position = (4, 0); ttype = Hor_bound }; { position = (5, 0);
   ttype = Hor_bound }; { position = (6, 0); ttype = Hor_bound }; {
   position = (7, 0); ttype = Hor_bound }; { position = (8, 0); ttype =
   Hor_bound }; { position = (9, 0); ttype = Hor_bound }; ]; [ {
   position = (0, 1); ttype = Ver_bound }; { position = (1, 1); ttype =
   Player { on_exit = false } }; { position = (2, 1); ttype = Normal {
   is_hole = false } }; { position = (3, 1); ttype = Normal { is_hole =
   false } }; { position = (4, 1); ttype = Normal { is_hole = false } };
   { position = (5, 1); ttype = Normal { is_hole = false } }; { position
   = (6, 1); ttype = Normal { is_hole = false } }; { position = (7, 1);
   ttype = Normal { is_hole = false } }; { position = (8, 1); ttype =
   Normal { is_hole = false } }; { position = (9, 1); ttype = Ver_bound
   }; ]; [ { position = (0, 2); ttype = Ver_bound }; { position = (1,
   2); ttype = Normal { is_hole = false } }; { position = (2, 2); ttype
   = Normal { is_hole = false } }; { position = (3, 2); ttype = Normal {
   is_hole = false } }; { position = (4, 2); ttype = Normal { is_hole =
   false } }; { position = (5, 2); ttype = Normal { is_hole = false } };
   { position = (6, 2); ttype = Normal { is_hole = false } }; { position
   = (7, 2); ttype = Normal { is_hole = false } }; { position = (8, 2);
   ttype = Normal { is_hole = false } }; { position = (9, 2); ttype =
   Ver_bound }; ]; [ { position = (0, 3); ttype = Ver_bound }; {
   position = (1, 3); ttype = Normal { is_hole = false } }; { position =
   (2, 3); ttype = Normal { is_hole = false } }; { position = (3, 3);
   ttype = Normal { is_hole = false } }; { position = (4, 3); ttype =
   Normal { is_hole = false } }; { position = (5, 3); ttype = Normal {
   is_hole = false } }; { position = (6, 3); ttype = Normal { is_hole =
   false } }; { position = (7, 3); ttype = Normal { is_hole = false } };
   { position = (8, 3); ttype = Normal { is_hole = false } }; { position
   = (9, 3); ttype = Ver_bound }; ]; [ { position = (0, 4); ttype =
   Ver_bound }; { position = (1, 4); ttype = Normal { is_hole = false }
   }; { position = (2, 4); ttype = Normal { is_hole = false } }; {
   position = (3, 4); ttype = Normal { is_hole = false } }; { position =
   (4, 4); ttype = Normal { is_hole = false } }; { position = (5, 4);
   ttype = Normal { is_hole = false } }; { position = (6, 4); ttype =
   Normal { is_hole = false } }; { position = (7, 4); ttype = Normal {
   is_hole = false } }; { position = (8, 4); ttype = Normal { is_hole =
   false } }; { position = (9, 4); ttype = Ver_bound }; ]; [ { position
   = (0, 5); ttype = Ver_bound }; { position = (1, 5); ttype = Normal {
   is_hole = false } }; { position = (2, 5); ttype = Normal { is_hole =
   false } }; { position = (3, 5); ttype = Normal { is_hole = false } };
   { position = (4, 5); ttype = Normal { is_hole = false } }; { position
   = (5, 5); ttype = Normal { is_hole = false } }; { position = (6, 5);
   ttype = Normal { is_hole = false } }; { position = (7, 5); ttype =
   Normal { is_hole = false } }; { position = (8, 5); ttype = Normal {
   is_hole = false } }; { position = (9, 5); ttype = Ver_bound }; ]; [ {
   position = (0, 6); ttype = Ver_bound }; { position = (1, 6); ttype =
   Normal { is_hole = false } }; { position = (2, 6); ttype = Normal {
   is_hole = false } }; { position = (3, 6); ttype = Normal { is_hole =
   false } }; { position = (4, 6); ttype = Normal { is_hole = false } };
   { position = (5, 6); ttype = Normal { is_hole = false } }; { position
   = (6, 6); ttype = Normal { is_hole = false } }; { position = (7, 6);
   ttype = Normal { is_hole = false } }; { position = (8, 6); ttype =
   Normal { is_hole = false } }; { position = (9, 6); ttype = Ver_bound
   }; ]; [ { position = (0, 7); ttype = Ver_bound }; { position = (1,
   7); ttype = Normal { is_hole = false } }; { position = (2, 7); ttype
   = Normal { is_hole = false } }; { position = (3, 7); ttype = Normal {
   is_hole = false } }; { position = (4, 7); ttype = Normal { is_hole =
   false } }; { position = (5, 7); ttype = Normal { is_hole = false } };
   { position = (6, 7); ttype = Normal { is_hole = false } }; { position
   = (7, 7); ttype = Normal { is_hole = false } }; { position = (8, 7);
   ttype = Normal { is_hole = false } }; { position = (9, 7); ttype =
   Ver_bound }; ]; [ { position = (0, 8); ttype = Ver_bound }; {
   position = (1, 8); ttype = Normal { is_hole = false } }; { position =
   (2, 8); ttype = Normal { is_hole = false } }; { position = (3, 8);
   ttype = Normal { is_hole = false } }; { position = (4, 8); ttype =
   Normal { is_hole = false } }; { position = (5, 8); ttype = Normal {
   is_hole = false } }; { position = (6, 8); ttype = Normal { is_hole =
   false } }; { position = (7, 8); ttype = Normal { is_hole = false } };
   { position = (8, 8); ttype = Normal { is_hole = false } }; { position
   = (9, 8); ttype = Ver_bound }; ]; [ { position = (0, 9); ttype =
   Hor_bound }; { position = (1, 9); ttype = Hor_bound }; { position =
   (2, 9); ttype = Hor_bound }; { position = (3, 9); ttype = Hor_bound
   }; { position = (4, 9); ttype = Hor_bound }; { position = (5, 9);
   ttype = Hor_bound }; { position = (6, 9); ttype = Hor_bound }; {
   position = (7, 9); ttype = Hor_bound }; { position = (8, 9); ttype =
   Hor_bound }; { position = (9, 9); ttype = Hor_bound }; ]; ]; blocks =
   []; holes = []; } *)
