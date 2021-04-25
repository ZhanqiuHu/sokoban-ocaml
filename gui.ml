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
  | Player _ -> "grass60x60.png"
  | Block _ -> "brick60x60.png"
  | Ver_bound -> "brick60x60.png"
  | Hor_bound -> "brick60x60.png"
  | Normal _ -> "grass60x60.png"
  | Exit -> "brick60x60.png"

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

let draw_rect_images (tile_list : 'a list list) width height =
  let rec draw_helper tile_list =
    match tile_list with
    | h_list :: t_list_list ->
        draw_hor_images h_list width height;
        draw_helper t_list_list
    | [] -> ()
  in
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

let draw_player (st : Types.state) =
  let player_pos = st.player_pos in
  Graphics.draw_image
    (get_img_transparent "link60x60.png")
    (fst player_pos * 60)
    (snd player_pos * 60)
