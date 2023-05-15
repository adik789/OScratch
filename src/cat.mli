val load_texture : bool
(** [load_texture] is if the cats texture should be loaded. MUST BE SET TO FALSE
    FOR OUNIT TESTS*)

type direction =
  | Left
  | Right  (** [direction] is the current direction that the cat is facing*)

type cat = {
  mutable direction : direction;
  mutable rect : Raylib.Rectangle.t;
  texture : Raylib.Texture.t option;
  (* mutable *)
  mutable pos : Raylib.Vector2.t;
  (* mutable *)
  mutable color : Raylib.Color.t;
}
(* [cat] is the representation of cat image*)

val init_cat : unit -> cat
(* [init_cat] is the representation of the cat at its current position. This is
   the record that gets mutated in all following functions*)

val move_right : float -> unit
(* [move_right] is the rectange representing the cat after moving [pixels] to
   the right. The cat is then updated in the field *)

val move_left : float -> unit
(* [move_left] is the rectange representing the cat after moving [pixels] to the
   left. The cat is then updated in the field *)

val move_up : float -> unit
(* [move_up] is the rectange representing the cat after moving [pixels] up. The
   cat is then updated in the field *)

val move_down : float -> unit
(* [move_down] is the rectange representing the cat after moving [pixels] down.
   The cat is then updated in the field *)

val change_direction : unit -> unit
(*[change_direction] changes the direction field in [init_cat] and reflects the
  image of the cat*)

val change_color : Raylib.Color.t -> unit
(*[change_color] changes the color field in [init_cat] and reflects the change
  on the image of the cat*)

val get_x : unit -> float
(*[get_x] is the current x value of the cat. MAINLY USED FOR TESTING*)

val get_y : unit -> float
(*[get_y] is the current y value of the cat. MAINLY USED FOR TESTING*)

val draw_cat : unit -> unit
