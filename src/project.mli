(** Representation of a OScratch Game Project.

    This module represents the OScratch Game, code blocks and operations. It
    handles all GUI setup as well as all user interaction. *)

type operation
(** The abstract type of values representing code block operations. *)

type code_block
(** The abstract type of values representing code blocks. *)

val setup : unit -> unit
(** [setup ()] initializes the GUI window and sets target fps *)

val draw_cat : unit -> unit
(** [draw_cat ()] draws the texture of the cat image on screen. *)

val change_rect : Raylib.Rectangle.t -> float -> float -> unit
(** [change_rect rect x y] changes the x and y coordinate of rect to the given x
    and y coordinates *)

val within : Raylib.Rectangle.t -> float -> float -> bool
val change_rect_position : Raylib.Rectangle.t -> unit
val create_code_blocks : unit -> unit
val visible_false : unit -> unit
val remove_block_tc : unit -> unit
val draw_on_screen : unit -> unit
val sort_exec_order : unit -> code_block list
val sort_block_position : unit -> unit
val run_code_blocks : code_block list -> unit
val setup_view : unit -> unit
val loop : unit -> unit -> unit

(**********************************************************************
  *Funtions for Testing *)

val create_move_test : unit -> code_block
val create_turn_test : unit -> code_block
val list_to_string : string list -> string
val grab_string_screen : unit -> string list
(**********************************************************************)
