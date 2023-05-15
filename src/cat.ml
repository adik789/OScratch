let load_texture = true

type direction =
  | Left
  | Right

let cat_image : Raylib.Image.t ref =
  ref (Raylib.load_image "200px-ScratchCat-Small.png")

type cat = {
  mutable direction : direction;
  mutable rect : Raylib.Rectangle.t;
  texture : Raylib.Texture.t option;
  (* mutable *)
  mutable pos : Raylib.Vector2.t;
  (* mutable *)
  color : Raylib.Color.t;
}

let cat_width = ref 200.
let cat_height = ref 200.
let pos_vector = Raylib.Vector2.create 500. 100.
let cat_rect = Raylib.Rectangle.create 0. 0. !cat_width !cat_height

let cat_texture () =
  if load_texture then Some (Raylib.load_texture_from_image !cat_image)
  else None

let init_cat () =
  {
    rect = cat_rect;
    texture = cat_texture ();
    pos = pos_vector;
    color = Raylib.Color.white;
    direction = Left;
  }

let move_right (pixels : float) =
  Raylib.Vector2.set_x (init_cat ()).pos
    (Raylib.Vector2.x (init_cat ()).pos +. pixels)

let move_left (pixels : float) =
  Raylib.Vector2.set_x (init_cat ()).pos
    (Raylib.Vector2.x (init_cat ()).pos -. pixels)

let move_up (pixels : float) =
  Raylib.Vector2.set_y (init_cat ()).pos
    (Raylib.Vector2.y (init_cat ()).pos -. pixels)

let move_down (pixels : float) =
  Raylib.Vector2.set_y (init_cat ()).pos
    (Raylib.Vector2.y (init_cat ()).pos +. pixels)

let change_direction () =
  cat_width := !cat_width *. -1.;
  Raylib.Rectangle.set_width cat_rect !cat_width;
  match (init_cat ()).direction with
  | Right -> (init_cat ()).direction <- Left
  | Left -> (init_cat ()).direction <- Right

(** Should never be a bug*)
let extract_texture o =
  match o with
  | Some s -> s
  | None -> Raylib.load_texture "200px-ScratchCat-Small.png"

let grow (scale : float) =
  cat_width := scale *. !cat_width;
  cat_height := scale *. !cat_height;
  Raylib.Rectangle.set_width cat_rect !cat_width;
  Raylib.Rectangle.set_height cat_rect !cat_height;
  Raylib.image_resize (Raylib.addr !cat_image) (int_of_float !cat_width)
    (int_of_float !cat_height)

let shrink (scale : float) = grow (1. /. scale)

let vectorize_size (r : Raylib.Rectangle.t) =
  Raylib.Vector2.create (Raylib.Rectangle.width r) (Raylib.Rectangle.height r)

let draw_cat () =
  let init_cat = init_cat () in
  if load_texture then
    Raylib.draw_texture_rec
      (extract_texture init_cat.texture)
      init_cat.rect init_cat.pos init_cat.color
  else
    Raylib.draw_rectangle_v init_cat.pos
      (vectorize_size init_cat.rect)
      init_cat.color

let get_x () = Raylib.Vector2.x (init_cat ()).pos
let get_y () = Raylib.Vector2.y (init_cat ()).pos
