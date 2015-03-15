open Batteries;;
open BatList;;

type color =  Cyan  | Yellow  | Purple  | Green  | Red  | Blue  | Orange ;;

type cell = Empty | Color of color ;;

type tetrimino_kind = I | J | L | O | S | T | Z ;;

type xy = (int*int) ;;
(* let xy x y = (x,y);; *)
let xyplus a b : xy = let (ax,ay)=a and (bx,by)=b in (ax+bx,ay+by);;

type tetrimino = {
    kind: tetrimino_kind;
    geometry: xy list;
    center: float*float;
    color: color;
} ;;

(* List of all tetrominoes. Initial geometry is given as they should appear
first.  Coordinate system have coordinate center on top left.  *)
let all_tetrominoes = [
  { kind = I;
    geometry = [(0,0);(1,0);(2,0);(3,0)];
    center = (1.5, 0.);
    color = Cyan };
  { kind = J;
    geometry = [(0,0);(0,1);(1,1);(2,1)];
    center = (1.,1.);
   color = Blue };
  { kind = L;
    geometry=[(0,1);(1,1);(2,1);(2,0)];
    center = (1.,1.);
    color = Orange };
  { kind = O;
    geometry = [(0,0);(1,0);(0,1);(1,1)];
    center = (0.5,0.5);
    color = Yellow };
  { kind = S;
    geometry = [(0,1);(1,1);(1,0);(2,0)];
    center = (1.,1.);
    color = Green };
  { kind = T;
    geometry = [(0,1);(1,1);(2,1);(1,0)];
    center = (1.,1.);
    color = Purple };
  { kind = Z;
    geometry = [(0,0);(1,0);(1,1);(2,1)];
    center = (1.,1.);
    color = Red }
  ];;

(* 2x2 rotation matrices stored by row *)
type rotation = R0 | R90 | R180 | R270 ;;
  
let rotation_matrix = function
  | R0 -> (1.,0.,0.,1.)
  | R90 -> (0.,-1.,1.,0.)
  | R180 -> (-1.,0.,0.,-1.)
  | R270 -> (0.,1.,-1.,0.);;

let clockwise_rotation = function  
  | R0 -> R90
  | R90 -> R180
  | R180 -> R270
  | R270 -> R0;;
  
let rotate (r:float*float*float*float) (c:float*float) (p:xy) : xy =
  let (xc,yc) = c and (x,y) = p and (r11,r12,r21,r22) = r in
  let rx = float x and ry = float y in
  let dx = rx -. xc and dy = ry -. yc in
  (truncate ((dx *. r11 +. dy *. r12)  +. xc),
   truncate ((dx *. r21 +. dy *. r22)  +. yc));;

type field = {width:int; height:int; cells:cell list};;

let make_field w h : field
  = {width=w; height=h; cells = make (w*h) Empty };;

let pick_random l  =  nth l (Random.int (length l));;
                         
type state = {
    score: int;
    field: field;
    tetromino: tetrimino;
    position: xy;
    rotation: rotation;
  } ;;
  
let iter2D l w f =
  let rec inter2D' l f w x y=
    if is_empty l then ()
    else ((f (hd l) x y) ; inter2D' (tl l) f w (if x=w then 0 else x+1) (if x=w then (y+1) else y))
    in inter2D' l f (w-1) 0 0;;

let spawn_position p board_width = (truncate ((float board_width /. 2.) -. (fst p.center)), 0);;

type action =  MoveLeft | MoveRight | Rotate | Drop | Tick;;

let cell_is_empty f (x,y)  =
  match nth f.cells (y*f.width+x) with
  | Empty -> true
  | Color _ -> false;;
  
let cell_in_range f (x,y) = x>=0 && x<f.width && y>=0 && y<=(f.height-1) ;;

let cell_available f xy = cell_in_range f xy && cell_is_empty f xy ;;
    
let fits f x y t r =
  BatList.fold_left (&&) true
                    (BatList.map ((cell_available f)
                                  % (xyplus (x,y))
                                  % (rotate (rotation_matrix r) t.center)) t.geometry);;
    
let rec update_state event state : state =
  let (x,y) = state.position in
  match event with
  | MoveLeft -> 
     if fits state.field (x-1) y state.tetromino state.rotation then
       {score = state.score; field = state.field; tetromino = state.tetromino; rotation = state.rotation;
        position = (x-1,y)}
     else state
  | MoveRight -> 
     if fits state.field (x+1) y state.tetromino state.rotation then
       {score = state.score; field = state.field; tetromino = state.tetromino; rotation = state.rotation;
        position = (x+1,y)}
     else state
  | Rotate ->
     let r = clockwise_rotation state.rotation in
     if fits state.field x y state.tetromino r then
       {score = state.score; field = state.field; tetromino = state.tetromino; position = state.position;
        rotation = r}
     else state
  | Drop ->
     if fits state.field x (y+1) state.tetromino state.rotation then
       update_state Drop {score = state.score; field = state.field; tetromino = state.tetromino; rotation = state.rotation;
                          position = (x,y+1)}
     else state  (* TODO: emboss in glass; check for endgame. generate new piece *)
  | Tick ->
     if fits state.field x (y+1) state.tetromino state.rotation then
       {score = state.score; field = state.field; tetromino = state.tetromino; rotation = state.rotation;
        position = (x,y+1)}
     else state (* TODO: emboss in glass; check for endgame. generate new piece *)

