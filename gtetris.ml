(* Simple graphical tetris using SDL library *)

open Batteries
open BatList
open Tetris
open Sdlevent
open Sdlkey
open Sdlvideo
open Sdlttf

(* Game board dimensions *)
let board_width = 10 and board_height = 22
(* Using fixed gravity  for now *)
let gravity = 0.05

(* misc colors and dimensions *)
let block_side = 20
let glass_color = (255,255,255)

(* additional internal constants *)
let tickUserEventNo = 0
let opaque = 255 (* for alpha channel values in RGBA *)
let font_filename  = "arial.ttf"
let font_size = 24


let rgb_color_map = function
  | Cyan -> (0,255,255)
  | Yellow -> (255,255,0)
  | Purple ->  (128,0,128)
  | Green ->  (0,255,0)
  | Red -> (255,0,0)
  | Blue -> (0,0,255)
  | Orange ->  (255, 165, 0)

let cell_color = function
  | Empty -> black
  | Color x -> rgb_color_map x

let rec timer_loop (flag, callback) =
  if !flag then
    Thread.exit
  else
    (Thread.delay 0.5;
     callback ();
     (timer_loop (flag, callback)))

let box screen x y w h c a =
  let r1 = rect x y 0 0 and r2 = rect (x+w) (y+h) 0 0 in
  ignore (Sdlgfx.boxRGBA screen r1 r2 c a)

let rectangle screen x y w h c a =
  let r1 = rect x y 0 0 and r2 = rect (x+w) (y+h) 0 0 in
  ignore (Sdlgfx.rectangleRGBA screen r1 r2 c a)

let draw_cell screen v x y =
  box screen ((x+1)*block_side) (y*block_side) block_side block_side (cell_color v) opaque

let draw_tetromino screen state =
  ignore (BatList.map (
              (fun (x, y) -> draw_cell screen (Color state.tetromino.color) x y)
              % (xyplus state.position)
              % (rotate (rotation_matrix state.rotation) state.tetromino.center)
            ) state.tetromino.geometry)

let show_game_over screen e =
  let font = open_font font_filename font_size in
  let text = render_text_blended font "Game Over" ~fg:Sdlvideo.white in
  let (t_w,t_h,_) = surface_dims text in
  let text_box = rect
                           ((e.r_w - t_w) /2)
                           ((e.r_h - t_h) /2)
                           t_w t_h in
  let f_x=(text_box.r_x-font_size)  and
      f_y = (text_box.r_y-font_size) and
      f_w = (text_box.r_w+(2*font_size)) and
      f_h = (text_box.r_h+(2*font_size)) in
  box screen f_x f_y f_w f_h black opaque;
  rectangle screen f_x f_y f_w f_h white opaque;
  blit_surface ~dst_rect:text_box ~src:text ~dst:screen ();
  ()

let draw state screen =
  ignore (iter2D state.cells state.width (draw_cell screen));
  if (state.over) then
    show_game_over screen (rect 0 0 (block_side*(board_width+2)) (block_side*(board_height+1)))
  else
    draw_tetromino screen state;
  flip screen

let draw_walls screen =
  box screen 0 0 block_side (block_side*board_height) glass_color opaque;
  box screen (block_side*(board_width+1)) 0 block_side (block_side*board_height) glass_color opaque;
  box screen 0 (block_side*board_height) (block_side*(board_width+2)) block_side glass_color opaque;
  flip screen

let rec loop state =
  let cstate = !state in
  let rstate = initial_state cstate.width cstate.height in
  draw cstate (get_video_surface ());
  match wait_event () with
  | KEYDOWN {keysym=KEY_UP} ->
     state := if cstate.over then rstate else update_state RotateCw cstate;
     loop state
  | KEYDOWN {keysym=KEY_DOWN} ->
     state := if cstate.over then rstate else update_state RotateCCw cstate;
     loop state
  | KEYDOWN {keysym=KEY_SPACE} ->
     state := if cstate.over then rstate else update_state HardDrop cstate;
     loop state
  | KEYDOWN {keysym=KEY_LEFT} ->
     state := if cstate.over then rstate else update_state MoveLeft cstate;
     loop state
  | KEYDOWN {keysym=KEY_RIGHT} ->
     state := if cstate.over then rstate else update_state MoveRight cstate;
     loop state
  | USER tickUserEventNo ->
     (if not cstate.over then
        state := update_state Tick cstate);
     loop state
  | KEYDOWN {keysym=KEY_ESCAPE} ->
     ()
  | _ ->
     loop state

let main () =
  Random.self_init();
  let (state:(Tetris.state ref)) = ref (initial_state board_width board_height) in
  Sdl.init [`VIDEO];
  let screen_width = block_side*(board_width+2) and screen_height=block_side*(board_height+1) in
  let screen = set_video_mode screen_width screen_height [`DOUBLEBUF] in
  at_exit Sdl.quit;
  Sdlttf.init ();
  at_exit Sdlttf.quit;
  Sdlwm.set_caption ~title:"GTetris" ~icon:"GTetris";
  draw_walls screen ;
  let timer_flag = ref false
  and timer_cb () = Sdlevent.add [USER tickUserEventNo]  in
  let timer_thread = Thread.create timer_loop (timer_flag, timer_cb) in
  loop state;
  timer_flag := true;
  Thread.join timer_thread

let _ = main ()                
