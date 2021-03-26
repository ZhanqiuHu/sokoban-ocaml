(** Parsing of player commands. *)

type step = int

type object_phrase = string list

type direction =
  | Left (* A *)
  | Right (* D *)
  | Up (* W *)
  | Down (* S *)
  | None
(* Does not move *)

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a direction or steps. *)
type command =
  | Start
  | Go of direction
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase. Examples:

    - [parse "    go   left  "] is [Go \["left"\]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} 1. if the verb is not one of "quit", "start", and "go"
    2. if the verb is "quit" and there is a non-empty object phrase 3.
    if the verb is "start" and there is a non-empty object phrase 4. if
    the verb is "go" and there is an empty object phrase.*)
val parse : string -> command
