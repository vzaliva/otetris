(* Simple graphical tetris using SDL library *)

open Batteries
open BatList
open Tetris
open Sdlevent
open Sdlkey
       
(* Game board dimensions *)
let board_width = 10 and board_height = 22
(* Using fixed gravity  for now *)
let gravity = 0.05

let tickUserEventNo = 0
                
let rec loop state =
  let cstate = !state in
  let rstate = initial_state cstate.width cstate.height in
  match wait_event () with
  | KEYDOWN {keysym=KEY_UP} ->
     state := if cstate.over then rstate else update_state RotateCw cstate;
     loop state
  | KEYDOWN {keysym=KEY_DOWN} ->
     state := if cstate.over then rstate else update_state RotateCCw cstate;
     loop state
  | KEYDOWN {keycode=' '} ->
     state := if cstate.over then rstate else update_state HardDrop cstate;
     loop state
  | KEYDOWN {keysym=KEY_LEFT} ->
     state := if cstate.over then rstate else update_state MoveLeft cstate;
     loop state
  | KEYDOWN {keysym=KEY_RIGHT} ->
     state := if cstate.over then rstate else update_state MoveRight cstate;
     loop state
  | USER tickUserEventNo ->
     print_string "Hello1\n";
     (if not cstate.over then
       state := update_state Tick cstate);
     loop state
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     ()
  | _ ->
     loop state

let rec timer_fun (flag, callback) =
  if !flag then
    Thread.exit
  else
    (Thread.delay 1.;
     callback;
     timer_fun (flag, callback))
                    
let main () =
  Random.self_init();
  let (state:(Tetris.state ref)) = ref (initial_state board_width board_height) in
  Sdl.init [`VIDEO];
  let screen = Sdlvideo.set_video_mode 400 400 [`DOUBLEBUF] in
  at_exit Sdl.quit;
  Sdlttf.init ();
  at_exit Sdlttf.quit;
  Sdlwm.set_caption ~title:"GTetris" ~icon:"GTetris";
  let timer_flag = ref false in
  let timer_cb = print_string "Hello0\n"; Sdlevent.add [USER tickUserEventNo] in
  let timer_thread = Thread.create timer_fun (timer_flag, timer_cb) in
  loop state;
  timer_flag := true;
  Thread.join timer_thread

let _ = main ()                
