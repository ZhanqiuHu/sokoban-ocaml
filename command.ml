(* Note: You may introduce new code anywhere in this file. *)
type step = int

type object_phrase = string list

type direction =
  | Left
  | Right
  | Up
  | Down
  | None

type command =
  | Start
  | Go of direction
  | Quit

exception Empty

exception Malformed

let rec remove_spaces str_list = List.filter (fun x -> x <> "") str_list

(** [split string] returns a string list containg the continuous
    segments separated by spaces in [string]. The returned string list
    does not contain any empty string or any string contianing spaces. *)
let split string = String.split_on_char ' ' string |> remove_spaces

(** [is_empty str] returns true if [str] is empty or only contains
    spaces. Otherwise returns false. *)
let is_empty str = split str = []

(** [is_invalid_go str_list] is true only if [str_list] only contains
    one element: "go" *)
let is_invalid_dir str_list =
  match str_list with
  | [ "a" ] -> false
  | [ "d" ] -> false
  | [ "w" ] -> false
  | [ "s" ] -> false
  | _ -> true

(** [is_malformed str] is true if [str] is not in the form of "go <arg1>
    ... <argn>" or "quit" aftering removing spaces at the begining and
    at the end. *)
let is_malformed str =
  let str_lst = split str in
  if str_lst = [ "start" ] then false
  else if str_lst = [ "quit" ] then false
  else if not (is_invalid_dir str_lst) then false
  else true

(** [form_go_command str_list] generates a command given [str_list].
    Requires: str_list is a valid object phrase starting with go. *)
let form_go_command str_list =
  match str_list with
  | [ "a" ] -> Go Left
  | [ "d" ] -> Go Right
  | [ "w" ] -> Go Up
  | [ "s" ] -> Go Down
  | _ -> Go None

let parse str : command =
  if is_empty str then raise Empty
  else if is_malformed str then raise Malformed
  else if List.hd (split str) = "quit" then Quit
  else if List.hd (split str) = "start" then Start
  else form_go_command (split str)
