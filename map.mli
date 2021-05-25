(** map.mli contains the maps used for the game. *)
open Types

(** [win] represents the winning room. *)
val win : room

(** [map2] represents the initial room. *)
val map2 : room

(** [map_m] represents a maze room. *)
val map_maze : room

(** [lose] represents the losing room. *)
val lose : room

(** [testing_map] represents the room for testing. *)
val testing_map : room
