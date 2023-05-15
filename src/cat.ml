let load_texture = false

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
  mutable color : Raylib.Color.t;
}

let cat_width = ref 200.
let cat_height = ref 200.
let pos_vector = Raylib.Vector2.create 500. 100.
let cat_rect = Raylib.Rectangle.create 0. 0. !cat_width !cat_height

let cat_texture () =
  if load_texture then Some (Raylib.load_texture_from_image !cat_image)
  else None

let mutable_color = ref Raylib.Color.white
let textbox_image = Raylib.load_image "text_bubble.png"

let init_cat () =
  {
    rect = cat_rect;
    texture = cat_texture ();
    pos = pos_vector;
    color = !mutable_color;
    direction = Left;
  }

let screen_width () = if load_texture then Raylib.get_screen_width () else 1000

let screen_height () =
  if load_texture then Raylib.get_screen_height () else 1000

let reset_cat () =
  Raylib.Vector2.set_x (init_cat ()).pos 500.;
  Raylib.Vector2.set_y (init_cat ()).pos 100.;
  mutable_color := Raylib.Color.white;
  cat_width := 200.;
  cat_height := 200.;
  Raylib.Rectangle.set_width cat_rect !cat_width

let move_right (pixels : float) =
  if
    Raylib.Vector2.x (init_cat ()).pos +. !cat_width +. pixels
    > float_of_int (screen_width ())
  then
    Raylib.Vector2.set_x (init_cat ()).pos
      (float_of_int (screen_width ()) -. !cat_width)
  else
    Raylib.Vector2.set_x (init_cat ()).pos
      (Raylib.Vector2.x (init_cat ()).pos +. pixels)

let move_left (pixels : float) =
  if
    Raylib.Vector2.x (init_cat ()).pos -. pixels
    < float_of_int (screen_width () / 2)
  then
    Raylib.Vector2.set_x (init_cat ()).pos (float_of_int (screen_width () / 2))
  else
    Raylib.Vector2.set_x (init_cat ()).pos
      (Raylib.Vector2.x (init_cat ()).pos -. pixels)

let move_up (pixels : float) =
  if Raylib.Vector2.y (init_cat ()).pos -. pixels < float_of_int 60 then
    Raylib.Vector2.set_y (init_cat ()).pos (float_of_int 60)
  else
    Raylib.Vector2.set_y (init_cat ()).pos
      (Raylib.Vector2.y (init_cat ()).pos -. pixels)

let move_down (pixels : float) =
  if
    Raylib.Vector2.y (init_cat ()).pos +. !cat_height +. pixels
    > float_of_int (screen_height ())
  then
    Raylib.Vector2.set_y (init_cat ()).pos
      (float_of_int (screen_height ()) -. !cat_height)
  else
    Raylib.Vector2.set_y (init_cat ()).pos
      (Raylib.Vector2.y (init_cat ()).pos +. pixels)

(** Should never be a bug*)
let change_direction () =
  cat_width := !cat_width *. -1.;
  Raylib.Rectangle.set_width cat_rect !cat_width;
  match (init_cat ()).direction with
  | Right -> (init_cat ()).direction <- Left
  | Left -> (init_cat ()).direction <- Right

let change_color color = mutable_color := color

let extract_texture o =
  match o with
  | Some s -> s
  | None -> Raylib.load_texture "200px-ScratchCat-Small.png"

let grow (scale : float) =
  if !cat_width *. scale > 500. || !cat_height *. scale > 500. then (
    cat_width := 500.;
    cat_height := 500.)
  else (
    cat_width := scale *. !cat_width;
    cat_height := scale *. !cat_height);
  Raylib.Rectangle.set_width cat_rect !cat_width;
  Raylib.Rectangle.set_height cat_rect !cat_height;
  Raylib.image_resize (Raylib.addr !cat_image) (int_of_float !cat_width)
    (int_of_float !cat_height);
  (* Check to see if needs to be resized*)
  move_right 0.;
  move_down 0.

let shrink (scale : float) =
  (* grow (1. /. scale) *)
  if !cat_width /. scale < 100. || !cat_height /. scale < 100. then (
    cat_width := 100.;
    cat_height := 100.)
  else (
    cat_width := !cat_width /. scale;
    cat_height := !cat_height /. scale);
  Raylib.Rectangle.set_width cat_rect !cat_width;
  Raylib.Rectangle.set_height cat_rect !cat_height;
  Raylib.image_resize (Raylib.addr !cat_image) (int_of_float !cat_width)
    (int_of_float !cat_height)

let say_text (s : string) =
  Raylib.image_resize (textbox_image |> Raylib.addr) 200 200;

  Raylib.image_draw_text
    (textbox_image |> Raylib.addr)
    s 30 30 16 Raylib.Color.black;

  let temp_text = Raylib.load_texture_from_image textbox_image in

  Raylib.draw_texture temp_text
    (int_of_float (Raylib.Vector2.x (init_cat ()).pos +. 50.))
    (int_of_float (Raylib.Vector2.y (init_cat ()).pos -. 50.))
    Raylib.Color.white;

  print_endline s

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
let get_width () = Raylib.Rectangle.width (init_cat ()).rect
let get_height () = Raylib.Rectangle.height (init_cat ()).rect
