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
(* [cat] is the representation of cat image*)

val init_cat : unit -> cat
(* [init_cat] is the representation of the cat at its current position. This is
   the record that gets mutated in all following functions*)

val move_right : float -> unit
(* [move_left] is the rectange representing the cat after moving [pixels] in the
   forward direction. The cat is then updated in the field *)

val change_direction : unit -> unit

(*[change_direction] changes the direction field in [init_cat] and reflects the
  image of the cat*)
val draw_cat : unit -> unit
