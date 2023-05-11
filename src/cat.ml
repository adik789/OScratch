type direction =
  | Left
  | Right

type cat = {
  mutable direction : direction;
  mutable rect : Raylib.Rectangle.t;
  texture : Raylib.Texture.t;
  (* mutable *)
  mutable pos : Raylib.Vector2.t;
  (* mutable *)
  color : Raylib.Color.t;
}

let cat_width = ref 200.
let pos_vector = Raylib.Vector2.create 500. 100.
let cat_rect = Raylib.Rectangle.create 0. 0. !cat_width 200.
let cat_texture () = Raylib.load_texture "200px-ScratchCat-Small.png"

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

let change_direction () =
  cat_width := !cat_width *. -1.;
  Raylib.Rectangle.set_width cat_rect !cat_width;
  match (init_cat ()).direction with
  | Right -> (init_cat ()).direction <- Left
  | Left -> (init_cat ()).direction <- Right

let draw_cat () =
  let init_cat = init_cat () in
  Raylib.draw_texture_rec init_cat.texture init_cat.rect init_cat.pos
    init_cat.color
