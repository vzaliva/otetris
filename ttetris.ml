 (*pp camlp4o `ocamlfind query -i-format lwt` `ocamlfind query -predicates syntax,preprocessor -a-format -r lwt.syntax` *)

(* Simple text tetris *)

open Batteries;;
open BatList;;
open Tetris

let board_width = 10 and board_height = 22 ;;

open Lwt
open Lwt_react
open LTerm_geom
open LTerm_text
open LTerm_key

let initial_state : Tetris.state =
  Random.self_init();
  let p = pick_random all_tetrominoes in
  {score = 0;
   field = make_field board_width board_height;
   tetromino = p;
   position = spawn_position p board_width;
   rotation = R0;
  } ;;
  
let term_color_map = function
  | Cyan -> LTerm_style.cyan
  | Yellow -> LTerm_style.yellow
  | Purple -> LTerm_style.lred
  | Green -> LTerm_style.green
  | Red -> LTerm_style.red
  | Blue -> LTerm_style.lblue
  | Orange -> LTerm_style.lyellow
;;

let cell_color = function
  | Empty -> LTerm_style.black
  | Color x -> term_color_map x;;
  
let cell_char = function
  | Empty -> S" "
  | Color x -> S"#";;
  
let rec loop ui state =
  LTerm_ui.wait ui >>= function
    | LTerm_event.Key{ code = Up } ->
        LTerm_ui.draw ui;
        loop ui state
    | LTerm_event.Key{ code = Down } ->
        LTerm_ui.draw ui;
        loop ui state
    | LTerm_event.Key{ code = Left } ->
        LTerm_ui.draw ui;
        loop ui state
    | LTerm_event.Key{ code = Right } ->
        LTerm_ui.draw ui;
        loop ui state
    | LTerm_event.Key{ code = Escape } ->
        return ()
    | ev ->
        loop ui state

let draw_cell ctx v x y = LTerm_draw.draw_styled ctx y (x+1) (eval [B_bg (cell_color v); (cell_char v); E_fg]);;

let draw_tetromino ctx state =
  ignore (BatList.map (
      (fun (x,y) -> draw_cell ctx (Color state.tetromino.color) x y)
      % (rotate (rotation_matrix state.rotation) state.tetromino.center)
      % (xyplus state.position)
    ) state.tetromino.geometry) ;;

let draw ui matrix state =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  let w = state.field.width and h=state.field.height in
  LTerm_draw.draw_frame ctx { row1 = -1; col1 = 0; row2 = h+1; col2 = w+2 } LTerm_draw.Heavy;
  let ctx = LTerm_draw.sub ctx { row1 = 0; col1 = 1; row2 = h; col2 = w+1 } in
  iter2D state.field.cells w (draw_cell ctx);
  draw_tetromino ctx state

lwt () =
  lwt term = Lazy.force LTerm.stdout in

  let (state:(Tetris.state ref)) = ref (initial_state) in

  lwt ui = LTerm_ui.create term (fun matrix size -> draw matrix size !state) in
  try_lwt
    loop ui state
  finally
    LTerm_ui.quit ui
