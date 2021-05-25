(** map.mli contains the maps used for the game. *)
open Types

(** [win] represents the winning room. *)
val win : room

(** [map2] represents the initial room. *)
val map2 : room

(** [map3] represents a room. *)
val map3 : room

(** [lose] represents the losing room. *)
val lose : room

(** [testing_map] represents the room for testing. *)
val testing_map : room
