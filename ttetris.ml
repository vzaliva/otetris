 (*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)

(* Simple text tetris *)

open Batteries
open BatList
open Tetris

let board_width = 10 and board_height = 22 

open Lwt
open Lwt_react
open LTerm_geom
open LTerm_text
open LTerm_key

let term_color_map = function
  | Cyan -> LTerm_style.cyan
  | Yellow -> LTerm_style.yellow
  | Purple -> LTerm_style.lred
  | Green -> LTerm_style.green
  | Red -> LTerm_style.red
  | Blue -> LTerm_style.lblue
  | Orange -> LTerm_style.lyellow


let cell_color = function
  | Empty -> LTerm_style.black
  | Color x -> term_color_map x
  
let cell_char = function
  | Empty -> S" "
  | Color x -> S"#"


type event_or_tick = LEvent of LTerm_event.t | LTick
let wait_for_event ui = LTerm_ui.wait ui >>= fun x -> return (LEvent x)
let wait_for_tick () = Lwt_unix.sleep 0.5 >>= fun () -> return (LTick)

let rec loop ui state event_thread tick_thread =
  Lwt.choose [ event_thread; tick_thread ]
  >>= function
  | LEvent (LTerm_event.Key{ code = Up }) ->
     state := update_state Rotate !state;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key{ code = Down }) ->
     state := update_state Drop !state;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key{ code = Left }) ->
     state := update_state MoveLeft !state;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key{ code = Right }) ->
     state := update_state MoveRight !state;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key{ code = Escape }) ->
     return ()
  | LTick ->
     state := update_state Tick !state;
     LTerm_ui.draw ui;
     loop ui state event_thread (wait_for_tick ())
  | ev ->
     loop ui state (wait_for_event ui) tick_thread
          
let draw_cell ctx v x y = LTerm_draw.draw_styled ctx y (x+1) (eval [B_bg (cell_color v); (cell_char v); E_fg])

let draw_tetromino ctx state =
  ignore (BatList.map (
              (fun (x,y) -> draw_cell ctx (Color state.tetromino.color) x y)
              % (xyplus state.position)                
              % (rotate (rotation_matrix state.rotation) state.tetromino.center)
            ) state.tetromino.geometry) 
  
let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  let w = state.field.width and h=state.field.height in
  LTerm_draw.draw_frame ctx { row1 = -1; col1 = 0; row2 = h+1; col2 = w+3 } LTerm_draw.Heavy;
  let ctx = LTerm_draw.sub ctx { row1 = 0; col1 = 1; row2 = h; col2 = w+2 } in
  iter2D state.field.cells w (draw_cell ctx);
  draw_tetromino ctx state

lwt () =
  lwt term = Lazy.force LTerm.stdout in

  Random.self_init();
  let (state:(Tetris.state ref)) = ref (initial_state board_width board_height) in

  lwt ui = LTerm_ui.create term (fun matrix size -> draw matrix size !state) in
  try_lwt
    loop ui state (wait_for_event ui) (wait_for_tick ())
  finally
    LTerm_ui.quit ui
