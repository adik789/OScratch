(* type direction = Right *)

type cat = {
  (* mutable direction : direction; *)
  rect : Raylib.Rectangle.t;
  texture : Raylib.Texture.t;
  (* mutable *)
  mutable pos : Raylib.Vector2.t;
  (* mutable *)
  color : Raylib.Color.t;
}

let pos_vector = Raylib.Vector2.create 500. 100.
let cat_rect = Raylib.Rectangle.create 0. 0. 200. 200.

let init_cat () =
  {
    (* direction = Right; *)
    rect = cat_rect;
    texture = Raylib.load_texture "200px-ScratchCat-Small.png";
    pos = pos_vector;
    color = Raylib.Color.white;
  }

let move_left (pixels : float) =
  Raylib.Vector2.set_x (init_cat ()).pos
    (Raylib.Vector2.x (init_cat ()).pos +. pixels)

let draw_cat () =
  let init_cat = init_cat () in
  Raylib.draw_texture_rec init_cat.texture init_cat.rect init_cat.pos
    init_cat.color
