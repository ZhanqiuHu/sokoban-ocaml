open Camlimages
open State
open Graphics
open Png
open Images
open Jpeg
open Types
open Genmap

let index8_helper bitmap =
  let w = bitmap.Index8.width
  and h = bitmap.Index8.height
  and colormap = bitmap.Index8.colormap.map in
  let cmap =
    Array.map (fun { r; g; b } -> Graphics.rgb r g b) colormap
  in
  if bitmap.Index8.transparent <> -1 then
    cmap.(bitmap.Index8.transparent) <- transp;
  Array.init h (fun i ->
      Array.init w (fun j -> cmap.(Index8.unsafe_get bitmap j i)))

let index16_helper bitmap =
  let w = bitmap.Index16.width
  and h = bitmap.Index16.height
  and colormap = bitmap.Index16.colormap.map in
  let cmap = Array.map (fun { r; g; b } -> rgb r g b) colormap in
  if bitmap.Index16.transparent <> -1 then
    cmap.(bitmap.Index16.transparent) <- transp;
  Array.init h (fun i ->
      Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))

let rgb24_helper bitmap =
  let w = bitmap.Rgb24.width and h = bitmap.Rgb24.height in
  Array.init h (fun i ->
      Array.init w (fun j ->
          let { r; g; b } = Rgb24.unsafe_get bitmap j i in
          rgb r g b))

(* [array_of_image img] transforms a given image to a color color array. *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap -> index8_helper bitmap
  | Index16 bitmap -> index16_helper bitmap
  | Rgb24 bitmap -> rgb24_helper bitmap
  | Rgba32 _ -> failwith "RGBA not supported"
  | Cmyk32 _ -> failwith "CMYK not supported"

(* [get_img img] returns an image according to input file name. *)
let get_img img = Png.load img [] |> array_of_image |> make_image

let make_transp img =
  let replace =
    Array.map (fun col ->
        if 16777215 - col < 350000 then transp else col)
  in
  Array.map (fun arr -> replace arr) img

(* [get_img_transparent img] returns a transparent image according to the
 * given file name. *)
let get_img_transparent img =
  Png.load img [] |> array_of_image |> make_transp |> make_image

let tile_to_img tile =
  match tile.ttype with
  | Obstacle -> "images/brick60x60.png"
  | Normal -> "images/grass60x60.png"
  | Exit -> "images/door360x60.png"

let draw_button (b : Types.button) =
  if b.enable then
    draw_image (get_img b.image) (fst b.position) (snd b.position)

let rec draw_select (select_list : Types.select list) =
  match select_list with
  | [] -> ()
  | h :: t ->
      if h.select then
        draw_image
          (get_img h.image_select)
          (fst h.position) (snd h.position)
      else
        draw_image
          (get_img h.image_deselect)
          (fst h.position) (snd h.position);
      draw_select t

let draw_hor_images (tile_list : 'a list) width height =
  let rec draw_helper tile_list =
    match tile_list with
    | h :: t ->
        draw_image
          (get_img (tile_to_img h))
          (fst h.position * width)
          (snd h.position * height);
        draw_helper t
    | [] -> ()
  in
  draw_helper tile_list

let rec draw_tiles width height tile_list =
  match tile_list with
  | h_list :: t_list_list ->
      draw_hor_images h_list width height;
      draw_tiles width height t_list_list
  | [] -> ()

let draw_rect_images (st : state) width height =
  let tile_list = get_tile_list st in
  draw_tiles width height tile_list

let list_to_array tile_list map_width map_height =
  let map = map_init map_width map_height (List.hd tile_list) in
  let rec update_map tile_list =
    match tile_list with
    | h :: t ->
        let _ = set map (fst h.position) (snd h.position) h in
        update_map t
    | _ -> ()
  in
  update_map tile_list;
  map

let array_to_nested_list arr = Genmap.map_to_list arr

let list_to_nested_list tile_list map_width map_height =
  list_to_array tile_list map_width map_height |> array_to_nested_list

let flatten_list (nested_list : Types.tile list list) : Types.tile list
    =
  List.flatten nested_list

let nested_to_map nested_list map_width map_height =
  list_to_array (flatten_list nested_list) map_width map_height

let draw_player (st : state) width height =
  let rec draw_helper (player_list : player list) =
    match player_list with
    | h :: t ->
        draw_image
          (get_img_transparent h.player_img)
          (fst h.position * width)
          (snd h.position * height);
        draw_helper t
    | [] -> ()
  in
  let player_list = get_player st in
  draw_helper player_list

let draw_block_list (st : state) width height =
  let rec draw_helper (block_list : block list) =
    match block_list with
    | h :: t ->
        draw_image
          (get_img_transparent "images/yblock60x60.png")
          (fst h.position * width)
          (snd h.position * height);
        draw_helper t
    | [] -> ()
  in
  let block_list = get_blocks st in
  draw_helper block_list

let draw_hole_list (st : state) width height =
  let rec draw_helper (hole_list : hole list) =
    match hole_list with
    | h :: t ->
        draw_image
          (get_img_transparent "images/cross60x60.png")
          (fst h.position * width)
          (snd h.position * height);
        draw_helper t
    | [] -> ()
  in
  let hole_list =
    get_hole_list (get_room_by_id st.current_room_id st)
  in
  draw_helper hole_list

let call_draw pos (s : string) w h =
  draw_image (get_img_transparent s) (fst pos * w) (snd pos * h)

let draw_break_list (st : state) wt ht =
  let rec draw_helper (break_list : breakable1 list) =
    match break_list with
    | h :: t ->
        let pos = h.position in
        if h.hp > 0 then call_draw pos "images/cracks60x60.png" wt ht
        else if h.hp = 0 then (
          call_draw pos "images/grass60x60.png" wt ht;
          call_draw pos "images/heaps60x60.png" wt ht)
        else ();
        draw_helper t
    | [] -> ()
  in
  let break_list = get_breaks st in
  draw_helper break_list
