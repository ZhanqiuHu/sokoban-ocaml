open State
open Graphics
open Types

(*Returns the height, width of the current room*)
let get_hw (room_id : string) st tile_size =
  let room = get_room_by_id room_id st in
  (room.height * tile_size, room.width * tile_size)

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

let rec draw_dialogue dia_list pos =
  match dia_list with
  | [] -> ()
  | h :: t ->
      Graphics.moveto 10 pos;
      Graphics.draw_string h;
      draw_dialogue t (pos - 15)

let draw_box room_dim (dialogue : string) =
  let width = snd room_dim in
  Graphics.set_color Graphics.black;
  Graphics.fill_rect 0 0 width 60;
  Graphics.set_color Graphics.white;
  draw_dialogue (parse_dialogue dialogue [] width) 40
