(** genmap.mli contains the functions used for generating a random map. *)
open Types

(** type for a mutable list *)
type 'a mutable_pos_list = { mutable list : 'a list }

(** type for a mutable bool object *)
type bool_box = { mutable bool_val : bool }

(** [init_breakables pos_list hp_val] generate breakables given their
    positions [pos_list] and [hp] value *)
val init_breakables : (int * int) list -> int -> Types.breakable1 list

(** [init_blocks pos_list] generate blocks given their positions
    [pos_list]*)
val init_blocks : (int * int) list -> Types.block list

(** [init_holes pos_list] generate holes given their positions
    [pos_list] *)
val init_holes : (int * int) list -> Types.hole list

(** [update_positions tile_map] update positions of tiles in [tiles_map]
    according to their positions in the array [tile_map] *)
val update_positions : Types.tile array array -> Types.tile array array

(** [map_init map_w map_h init_val] create a map with width [map_w]
    height [map_h] and initial value [init_val] *)
val map_init : int -> int -> Types.tile -> Types.tile array array

(** [init_boundary init_map bound_val] generate a map with boundaries
    [bound_val] based on [init_map] *)
val init_boundary :
  Types.tile array array -> Types.tile -> Types.tile array array

(** [set map x_pos y_pos new_val] set the value in [map] with position
    [x_pos] and [y_pos] to [new_val]*)
val set :
  Types.tile array array ->
  int ->
  int ->
  Types.tile ->
  Types.tile array array

(** [get map x_pos y_pos] get the value in [map] with position [x_pos]
    and [y_pos] *)
val get : 'a array array -> int -> int -> 'a

(** [set_with_same_pos map x_pos y_pos new_val] set the value in [map]
    with position [x_pos] and [y_pos] to [new_val] but keeps the
    original position of the value *)
val set_with_same_pos :
  Types.tile array array ->
  int ->
  int ->
  Types.tile ->
  Types.tile array array

(** [set_pos_list_with_same_pos map pos_list new_val] sets tiles in
    [map] with positions in [pos_list] to [new_val], keeping their
    original positions *)
val set_pos_list_with_same_pos :
  Types.tile array array -> (int * int) list -> Types.tile -> unit

(** [copy_map map] returns a copy of map *)
val copy_map : 'a array array -> 'a array array

(** [define_path_map map path_list path_val] returns a copy of the map
    with a path defined by [path_list] and [path_val] *)
val define_path_map :
  Types.tile array array ->
  (int * int) list ->
  Types.tile ->
  Types.tile array array

(** [choose_obstacles map path_list path_val tile_val obstacle_val true_prob]
    updates a map with obstacles [obstacle_val] randomly chosen with a
    probablity of [true_prob], given defined [path_list], [path_val],
    and [tile_val]. The map should have its boundaries defined. *)
val choose_obstacles :
  Types.tile array array ->
  (int * int) list ->
  Types.tile ->
  Types.tile ->
  Types.tile ->
  float ->
  unit

(** [generate_breakables map path_list path_val tile_val true_prob]
    returns breakables randomly chosen with a probablity of [true_prob],
    given defined [map] with boundaries and obstacles, [path_list],
    [path_val], and [tile_val]. *)
val generate_breakables :
  Types.tile array array ->
  (int * int) list ->
  Types.tile ->
  Types.tile ->
  float ->
  (int * int) list

(** [map_to_list map] returns a immutable copy of the map. *)
val map_to_list : 'a array array -> 'a list list

(** [new_map_with_obstacles map_w map_h tile_val bound_val path_val 
    obstacle_val path_pos_list obstacle_prob]
    generate a map with width [map_w] and height [map_h]. The map has
    tile value [tile_val], boundary value [bound_val], and path value
    [path_val]. Obstacles [obstacle_val] will be chosen at places that
    are not pathes or boundaries. The probability of obstacles appearing
    is defined by [obstacle_prob]. *)
val new_map_with_obstacles :
  int ->
  int ->
  Types.tile ->
  Types.tile ->
  Types.tile ->
  Types.tile ->
  (int * int) list ->
  float ->
  Types.tile array array

(** [generate_path_pos pos_list] returns a path so all positions in
    [pos_list] are connected by the path. *)
val generate_path_pos : (int * int) list -> (int * int) list
