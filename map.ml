open Types

let map1 =
  {
    room_id = "beginning";
    width = 10.0;
    height = 10.0;
    map_tile_list =
      [
        (*First row*)
        [
          { position = (0, 0); ttype = Hor_bound };
          { position = (1, 0); ttype = Hor_bound };
          { position = (2, 0); ttype = Hor_bound };
          { position = (3, 0); ttype = Hor_bound };
          { position = (4, 0); ttype = Hor_bound };
          { position = (5, 0); ttype = Hor_bound };
          { position = (6, 0); ttype = Hor_bound };
          { position = (7, 0); ttype = Hor_bound };
          { position = (8, 0); ttype = Hor_bound };
          { position = (9, 0); ttype = Hor_bound };
        ];
        [
          { position = (0, 1); ttype = Hor_bound };
          { position = (1, 1); ttype = Hor_bound };
          { position = (2, 1); ttype = Hor_bound };
          { position = (3, 1); ttype = Hor_bound };
          { position = (4, 1); ttype = Hor_bound };
          { position = (5, 1); ttype = Hor_bound };
          { position = (6, 1); ttype = Hor_bound };
          { position = (7, 1); ttype = Hor_bound };
          { position = (8, 1); ttype = Hor_bound };
          { position = (9, 1); ttype = Hor_bound };
        ];
        [
          { position = (0, 2); ttype = Hor_bound };
          { position = (1, 2); ttype = Hor_bound };
          { position = (2, 2); ttype = Hor_bound };
          { position = (3, 2); ttype = Hor_bound };
          { position = (4, 2); ttype = Hor_bound };
          { position = (5, 2); ttype = Hor_bound };
          { position = (6, 2); ttype = Hor_bound };
          { position = (7, 2); ttype = Hor_bound };
          { position = (8, 2); ttype = Hor_bound };
          { position = (9, 2); ttype = Hor_bound };
        ];
        [
          { position = (0, 3); ttype = Hor_bound };
          { position = (1, 3); ttype = Hor_bound };
          { position = (2, 3); ttype = Hor_bound };
          { position = (3, 3); ttype = Hor_bound };
          { position = (4, 3); ttype = Hor_bound };
          { position = (5, 3); ttype = Hor_bound };
          { position = (6, 3); ttype = Hor_bound };
          { position = (7, 3); ttype = Hor_bound };
          { position = (8, 3); ttype = Hor_bound };
          { position = (9, 3); ttype = Hor_bound };
        ];
        [
          { position = (0, 4); ttype = Hor_bound };
          { position = (1, 4); ttype = Hor_bound };
          { position = (2, 4); ttype = Hor_bound };
          { position = (3, 4); ttype = Hor_bound };
          { position = (4, 4); ttype = Hor_bound };
          { position = (5, 4); ttype = Hor_bound };
          { position = (6, 4); ttype = Hor_bound };
          { position = (7, 4); ttype = Hor_bound };
          { position = (8, 4); ttype = Hor_bound };
          { position = (9, 4); ttype = Hor_bound };
        ];
        [
          { position = (0, 5); ttype = Hor_bound };
          { position = (1, 5); ttype = Hor_bound };
          { position = (2, 5); ttype = Hor_bound };
          { position = (3, 5); ttype = Hor_bound };
          { position = (4, 5); ttype = Hor_bound };
          { position = (5, 5); ttype = Hor_bound };
          { position = (6, 5); ttype = Hor_bound };
          { position = (7, 5); ttype = Hor_bound };
          { position = (8, 5); ttype = Hor_bound };
          { position = (9, 5); ttype = Hor_bound };
        ];
        [
          { position = (0, 6); ttype = Hor_bound };
          { position = (1, 6); ttype = Hor_bound };
          { position = (2, 6); ttype = Hor_bound };
          { position = (3, 6); ttype = Hor_bound };
          { position = (4, 6); ttype = Hor_bound };
          { position = (5, 6); ttype = Hor_bound };
          { position = (6, 6); ttype = Hor_bound };
          { position = (7, 6); ttype = Hor_bound };
          { position = (8, 6); ttype = Hor_bound };
          { position = (9, 6); ttype = Hor_bound };
        ];
        [
          { position = (0, 7); ttype = Hor_bound };
          { position = (1, 7); ttype = Hor_bound };
          { position = (2, 7); ttype = Hor_bound };
          { position = (3, 7); ttype = Hor_bound };
          { position = (4, 7); ttype = Hor_bound };
          { position = (5, 7); ttype = Hor_bound };
          { position = (6, 7); ttype = Hor_bound };
          { position = (7, 7); ttype = Hor_bound };
          { position = (8, 7); ttype = Hor_bound };
          { position = (9, 7); ttype = Hor_bound };
        ];
        [
          { position = (0, 8); ttype = Hor_bound };
          { position = (1, 8); ttype = Hor_bound };
          { position = (2, 8); ttype = Hor_bound };
          { position = (3, 8); ttype = Hor_bound };
          { position = (4, 8); ttype = Hor_bound };
          { position = (5, 8); ttype = Hor_bound };
          { position = (6, 8); ttype = Hor_bound };
          { position = (7, 8); ttype = Hor_bound };
          { position = (8, 8); ttype = Hor_bound };
          { position = (9, 8); ttype = Hor_bound };
        ];
        [
          { position = (0, 9); ttype = Hor_bound };
          { position = (1, 9); ttype = Hor_bound };
          { position = (2, 9); ttype = Hor_bound };
          { position = (3, 9); ttype = Hor_bound };
          { position = (4, 9); ttype = Hor_bound };
          { position = (5, 9); ttype = Hor_bound };
          { position = (6, 9); ttype = Hor_bound };
          { position = (7, 9); ttype = Hor_bound };
          { position = (8, 9); ttype = Hor_bound };
          { position = (9, 9); ttype = Hor_bound };
        ];
      ];
    blocks = [];
    holes = [];
  }
