(** Representation of a OScratch Game Project.

    This module represents the OScratch Game, code blocks and operations. It
    handles all GUI setup as well as all user interaction. *)

type operation
(** The abstract type of values representing code block operations. *)

type code_block
(** The abstract type of values representing code blocks. *)

val on_screen : code_block list ref
(** The list representing all code_blocks that are currently being displayed on
    screen in the GUI*)

val setup : unit -> unit
(** [setup ()] t initializes the graphics window with a resolution of 800x450 It
    then sets the target FPS to 60 using functions provided by the raylib
    library. *)

val draw_cat : unit -> unit
(** [draw_cat ()] draws the texture of the cat image on screen. *)

val change_rect : Raylib.Rectangle.t -> float -> float -> unit
(** [change_rect rect x y] changes the x and y coordinate of rect to the given x
    and y coordinates *)

val within : Raylib.Rectangle.t -> float -> float -> bool
(** [within rect x y] is whether or not x and y fall within the bounds of
    Rectangle rect *)

val change_rect_position : Raylib.Rectangle.t -> unit
(** [change_rect_position rect] changes the x and y coordinates of rect to the
    current x and y coordinates of the cursor on screen *)

val create_code_blocks : unit -> unit
(** [create_code_blocks ()] detects a left mouse click within any of the
    stationary code blocks, and prepends the respective code_block to on_screen
    (Left Click in move -> create and prepend move code_block. Left Click in
    turn -> create prepend turn code_block. Left Click in wait -> create prepend
    wait code_block) *)

val visible_false : unit -> unit
val remove_block_tc : unit -> unit

val draw_on_screen : unit -> unit
(** [draw_on_screen ()] detects code_block interaction with any code_blocks on
    screen, and draws all code_block rectangles in on_screen onto the GUI, with
    updated coordinates *)

val sort_exec_order : code_block list -> code_block list
(** [sort_exec_order on_screen] sorts on_screen so that the code_blocks are
    organized according to thier y-coordinate position on the GUI. Blocks closer
    to the top are thus placed at the front of the list, establishing at
    top-down execution order *)

val sort_block_position : code_block list -> Raylib.Rectangle.t list
(** [sort_block_position ()] formats all code_blocks in on_screen, so that they
    are organized by execution order, and change the coordinates of all
    code_block rectanlges so that they are spaced evenly and aligned in parallel
    within the code space, and returns a list of the sorted rectangles for the
    code_blocks *)

val run_code_blocks : code_block list -> unit
(**[run_code_blocks lst] runs all the code blocks in the given lst of type
   [code_block] if their test attribute = false, if the code_block has the
   attribute test = true, then it will not run on the GUI instead it will be ran
   through [grab_text] and the result will be appended to the string_on_screen
   ref *)

val setup_view : unit -> unit
(** [setup_view ()] initializes the sections of the OScratch program, as well as
    initializes all title and message text to display on the GUI*)

val loop : unit -> unit -> unit
(** [loop] executes all operations required by the OScratch game on loop as long
    as the window is still open*)

(**********************************************************************
  *Funtions for Testing *)

val create_move_test : float -> float -> code_block
(** [create_move_test ()] makes a testing code block with operation type Move *)

val create_turn_test : float -> float -> code_block
(**[create_turn_test ()] makes a testing code block with operation type Turn *)

val create_wait_test : float -> float -> code_block

val list_to_string : string list -> string
(** [list_to_string lst] returns the contents of lst as a string Example: lst =
    [ "hello" ; "friend"] then the result = "hello friend "*)

val text_grab : code_block -> string
(** [text_grab block] takes in a code block and then returns the operation type
    as a string (Turn -> "Turn", Move -> "Move", etc...) *)

val grab_string_screen : unit -> string list
(** [grab_string_screen ()] returns the accumulated string_on_screen ref *)

(**********************************************************************)
