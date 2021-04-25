open Camlimages
open State
open Graphics
open Png
open Images
open Jpeg
open Types
open Genmap

(* [array_of_image img] transforms a given image to a color color array. *)
let array_of_image img =
  match img with
  | Images.Index8 bitmap ->
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
  | Index16 bitmap ->
      let w = bitmap.Index16.width
      and h = bitmap.Index16.height
      and colormap = bitmap.Index16.colormap.map in
      let cmap = Array.map (fun { r; g; b } -> rgb r g b) colormap in
      if bitmap.Index16.transparent <> -1 then
        cmap.(bitmap.Index16.transparent) <- transp;
      Array.init h (fun i ->
          Array.init w (fun j -> cmap.(Index16.unsafe_get bitmap j i)))
  | Rgb24 bitmap ->
      let w = bitmap.Rgb24.width and h = bitmap.Rgb24.height in
      Array.init h (fun i ->
          Array.init w (fun j ->
              let { r; g; b } = Rgb24.unsafe_get bitmap j i in
              rgb r g b))
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
  | Obstacle -> "brick60x60.png"
  | Normal -> "grass60x60.png"
  | Exit -> "images/door360x60.png"

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

let draw_rect_images (st : state) width height =
  let rec draw_helper tile_list =
    match tile_list with
    | h_list :: t_list_list ->
        draw_hor_images h_list width height;
        draw_helper t_list_list
    | [] -> ()
  in
  let tile_list = get_tile_list st in
  draw_helper tile_list

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
          (get_img_transparent "link60x60.png")
          (fst h.position * width)
          (snd h.position * height);
        draw_helper t
    | [] -> ()
  in
  let player_list = get_player st in
  draw_helper player_list

(* Graphics.draw_image (get_img_transparent "link60x60.png") (fst
   player_pos * 60) (snd player_pos * 60) *)

let draw_block_list (st : state) width height =
  let rec draw_helper (block_list : block list) =
    match block_list with
    | h :: t ->
        draw_image
          (get_img_transparent "yblock60x60.png")
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
          (get_img_transparent "cross60x60.png")
          (fst h.position * width)
          (snd h.position * height);
        draw_helper t
    | [] -> ()
  in
  let hole_list =
    get_hole_list (get_room_by_id st.current_room_id st)
  in
  draw_helper hole_list

let draw_break_list (st : state) width height =
  let rec draw_helper (break_list : breakable1 list) =
    match break_list with
    | h :: t ->
        if h.hp > 0 then
          draw_image
            (get_img_transparent "images/cracks60x60.png")
            (fst h.position * width)
            (snd h.position * height)
        else if h.hp = 0 then (
          draw_image
            (get_img_transparent "grass60x60.png")
            (fst h.position * width)
            (snd h.position * height);
          draw_image
            (get_img_transparent "images/heaps60x60.png")
            (fst h.position * width)
            (snd h.position * height))
        else ();
        draw_helper t
    | [] -> ()
  in
  let break_list = get_breaks st in
  draw_helper break_list
