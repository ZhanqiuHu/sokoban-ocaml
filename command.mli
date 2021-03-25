(** Parsing of player commands. *)

type step = int

type object_phrase = string list

type direction =
  | Left
  | Right
  | Around

(** The type [command] represents a player command that is decomposed
    into a verb and possibly a direction or steps. *)
type command =
  | Turn of direction
  | Go of step (* steps *)
  | Quit

(** Raised when an empty command is parsed. *)
exception Empty

(** Raised when a malformed command is encountered. *)
exception Malformed

(** [parse str] parses a player's input into a [command], as follows.
    The first word (i.e., consecutive sequence of non-space characters)
    of [str] becomes the verb. The rest of the words, if any, become the
    object phrase. Examples:

    - [parse "    go   clock   tower   "] is [Go \["clock"; "tower"\]]
    - [parse "quit"] is [Quit].

    Requires: [str] contains only alphanumeric (A-Z, a-z, 0-9) and space
    characters (only ASCII character code 32; not tabs or newlines,
    etc.).

    Raises: [Empty] if [str] is the empty string or contains only
    spaces.

    Raises: [Malformed] if the command is malformed. A command is
    {i malformed} if the verb is neither "quit" nor "go", or if the verb
    is "quit" and there is a non-empty object phrase, or if the verb is
    "go" and there is an empty object phrase.*)
val parse : string -> command
