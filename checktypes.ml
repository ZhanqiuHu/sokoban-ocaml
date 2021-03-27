module type Types = sig
  type player

  type block

  type tiles

  type tile

  type boundary

  type hole

  type player_action

  type room

  type state

  type command

  val get_player_pos : player -> int * int

  val get_block_pos : block -> int * int

  val get_tile_pos : tile -> int * int

  val get_boundary_type : boundary -> string
end

(* module TypesCheck : TypesSig = Types *)

(* module AdventureCheck : AdventureSig = Adventure

   module type CommandSig = sig type object_phrase = string list

   type command = | Go of object_phrase | Quit

   exception Empty

   exception Malformed

   val parse : string -> command end

   module CommandCheck : CommandSig = Command

   module type StateSig = sig type t

   val init_state : Adventure.t -> t

   val current_room_id : t -> string

   val visited : t -> string list

   type result = | Legal of t | Illegal

   val go : Adventure.exit_name -> Adventure.t -> t -> result end

   module StateCheck : StateSig = State

   module type AuthorSig = sig val hours_worked : int end

   module AuthorCheck : AuthorSig = Author *)
