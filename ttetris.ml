 (*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)

(* Simple text tetris *)

open Batteries
open BatList
open Tetris

open Lwt
open Lwt_react
open LTerm_geom
open LTerm_text
open LTerm_key
open LTerm_style

open CamomileLibraryDyn.Camomile
       

(* Game board dimensions *)
let board_width = 10 and board_height = 22
(* Using fixed gravity  for now *)
let gravity = 0.05

let term_color_map = function
  | Cyan -> cyan
  | Yellow -> yellow
  | Purple -> lred
  | Green -> green
  | Red -> red
  | Blue -> lblue
  | Orange -> lyellow

let cell_color = function
  | Empty -> black
  | Color x -> term_color_map x
  
let cell_char = function
  | Empty -> S" "
  | Color _ -> S"#"

type event_or_tick = LEvent of LTerm_event.t | LTick
let wait_for_event ui = LTerm_ui.wait ui >>= fun x -> return (LEvent x)
let wait_for_tick () = Lwt_unix.sleep (gravity_period gravity) >>= fun () -> return (LTick)

let rec loop ui state event_thread tick_thread =
  Lwt.choose [ event_thread; tick_thread ] >>= fun e ->
  let cstate = !state in
  let rstate = initial_state cstate.width cstate.height in
  match e with
  | LEvent (LTerm_event.Key {code = Up}) ->
     state := if cstate.over then rstate else update_state RotateCw cstate;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key {code = Down}) ->
     state := if cstate.over then rstate else update_state RotateCCw cstate;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key {code = Char _}) ->
     state := if cstate.over then rstate else update_state Drop cstate;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key {code = Left}) ->
     state := if cstate.over then rstate else update_state MoveLeft cstate;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key {code = Right}) ->
     state := if cstate.over then rstate else update_state MoveRight cstate;
     LTerm_ui.draw ui;
     loop ui state (wait_for_event ui) tick_thread
  | LEvent (LTerm_event.Key {code = Escape}) ->
     return ()
  | LTick ->
     if not cstate.over then
       state := update_state Tick cstate;
     LTerm_ui.draw ui;
     loop ui state event_thread (wait_for_tick ())
  | _ ->
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
  let draw_legend =
    let lctx = LTerm_draw.sub ctx { row1 = 0; col1 = state.width+6; row2 = state.height; col2 = 60 } in
    LTerm_draw.draw_styled lctx 1 0 (eval [B_fg cyan; S (Printf.sprintf "Score: %u" state.score); E_fg]);
    LTerm_draw.draw_styled lctx 2 0 (eval [B_fg lblue; S (Printf.sprintf "Level: %u" state.level); E_fg]);

    LTerm_draw.draw_styled lctx 5 0 (eval [B_fg green; S"Controls:"; E_fg]);
    LTerm_draw.draw_styled lctx 6 2 (eval [B_fg yellow; S"<-, ->"; E_fg]);
    LTerm_draw.draw_styled lctx 6 12 (eval [B_fg white; S": move"; E_fg]);
    LTerm_draw.draw_styled lctx 7 2 (eval [B_fg yellow; S"Up, Down"; E_fg]);
    LTerm_draw.draw_styled lctx 7 12 (eval [B_fg white; S": rotate"; E_fg]);
    LTerm_draw.draw_styled lctx 8 2 (eval [B_fg yellow; S"Space"; E_fg]);
    LTerm_draw.draw_styled lctx 8 12 (eval [B_fg white; S": drop"; E_fg]);
    LTerm_draw.draw_styled lctx 9 2 (eval [B_fg yellow; S"Esc"; E_fg]);
    LTerm_draw.draw_styled lctx 9 12 (eval [B_fg white; S": quit"; E_fg]);

    LTerm_draw.draw_styled lctx (state.height-1) 0 (eval [B_fg blue; S"https://github.com/vzaliva/otetris"; E_fg])
  in
  let draw_glass = 
    let w = state.width and h=state.height in
    LTerm_draw.draw_frame ctx { row1 = -1; col1 = 0; row2 = h+1; col2 = w+3 } LTerm_draw.Heavy;
    let ctx = LTerm_draw.sub ctx { row1 = 0; col1 = 1; row2 = h; col2 = w+2 } in
    ignore (iter2D state.cells w (draw_cell ctx));
    if (state.over) then
      let my = state.height/2 in
      let mctx = LTerm_draw.sub ctx { row1 = my-1; col1 = 0; row2 = my+2; col2 = w+1 } in
      let bst = {
          bold=Some true;
          underline=None;
          blink=Some true;
          reverse=Some true;
          foreground=Some red;
          background=Some black;
        } in 
      LTerm_draw.fill mctx ?style:(Some bst) (UChar.of_char '*');
      LTerm_draw.draw_frame mctx { row1 = 0; col1 = 0; row2 = 3; col2 = w+1} LTerm_draw.Heavy;
      LTerm_draw.draw_styled mctx 1 1 (eval [B_fg red; S"Game over"; E_fg])
    else
      draw_tetromino ctx state
  in
  draw_glass ; draw_legend

lwt () =
  Random.self_init();
  let (state:(Tetris.state ref)) = ref (initial_state board_width board_height) in
  lwt term = Lazy.force LTerm.stdout in
  lwt ui = LTerm_ui.create term (fun matrix size -> draw matrix size !state) in
  try_lwt
    loop ui state (wait_for_event ui) (wait_for_tick ())
  finally
    LTerm_ui.quit ui
