open Types

let map1 =
  {
    room_id = "beginning";
    width = 10.0;
    height = 10.0;
    tile_list =
      [
        Boundary { position = (0, 0); orientation = "horizontal" };
        Boundary { position = (1, 0); orientation = "horizontal" };
        Boundary { position = (2, 0); orientation = "horizontal" };
        Boundary { position = (3, 0); orientation = "horizontal" };
        Boundary { position = (4, 0); orientation = "horizontal" };
        Boundary { position = (5, 0); orientation = "horizontal" };
        Boundary { position = (6, 0); orientation = "horizontal" };
        Boundary { position = (7, 0); orientation = "horizontal" };
        Boundary { position = (8, 0); orientation = "horizontal" };
        Boundary { position = (9, 0); orientation = "horizontal" };
      ];
    blocks = [];
    holes = [];
  }
