type cat = {
  (* mutable direction : direction; *)
  rect : Raylib.Rectangle.t;
  texture : Raylib.Texture.t;
  (* mutable *)
  mutable pos : Raylib.Vector2.t;
  (* mutable *)
  color : Raylib.Color.t;
}
(* [cat] is the representation of cat image*)

val init_cat : unit -> cat
val move_left : float -> unit
(* [move_left] is the rectange representing the cat after moving [pixels] in the
   forward direction. The cat is then updated in the field *)

val draw_cat : unit -> unit
