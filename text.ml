open Camlimages
open State
open Graphics
open Png
open Images
open Jpeg
open Types
open Genmap

(*Returns the width of the current room*)
let get_width room_id st tile_size =
  let room = get_room_by_id st room_id in
  room.width * tile_size

(*Draws a text box at the bottom of the screen*)
let rec parse_dialogue dialogue acc width =
  match dialogue with
  | str ->
      let len = String.length str * 6 in
      if len + 10 > width - 10 then
        let last_space =
          String.rindex_from str ((width / 6) - 10) ' '
        in
        let sub1 = String.sub str 0 last_space in
        let sub2 =
          String.sub str (last_space + 1)
            (String.length str - 1 - last_space)
        in
        parse_dialogue sub2 (sub1 :: acc) width
      else List.rev (str :: acc)

let rec draw_dialogue dia_list pos width =
  match dia_list with
  | [] -> ()
  | h :: t ->
      Graphics.moveto 10 pos;
      Graphics.draw_string h;
      draw_dialogue t (pos - 15) width

let draw_box room_width (dialogue : string) =
  (* Graphics.draw_rect 0 height width 20; *)
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 room_width 60;
  Graphics.set_color Graphics.white;
  draw_dialogue (parse_dialogue dialogue [] room_width) 40 room_width

(* let len = String.length h in if len + 10 <= width - 10 then (
   Graphics.moveto 10 pos; Graphics.draw_string h; draw_dialogue t (pos
   - 15) width) else let last_space = String.rindex_from h (width - 10)
   ' ' in let sub1 = String.sub h 0 last_space in let sub2 = String.sub
   h (last_space + 1) (len - 1 - last_space) in draw_dialogue (sub1 ::
   sub2 :: t) pos width *)

(* let check_mouse_pos = fst (Graphics.mouse_pos ()) *)
