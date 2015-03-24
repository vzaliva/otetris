(* Simple graphical tetris using SDL library *)

open Batteries
open BatList
open Tetris

(* Game board dimensions *)
let board_width = 10 and board_height = 22
(* Using fixed gravity  for now *)
let gravity = 0.05

let main () =
  Random.self_init();
  let (state:(Tetris.state ref)) = ref (initial_state board_width board_height) in
  
  Sdl.init [`VIDEO];
  let screen = Sdlvideo.set_video_mode 400 400 [`DOUBLEBUF] in
  at_exit Sdl.quit;
  Sdlttf.init ();
  at_exit Sdlttf.quit;    
  Sdltimer.delay 2000;
  Sdl.quit ()
           
let _ = main ()                
