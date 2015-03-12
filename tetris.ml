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
    initial_position: xy;
    color: color;
} ;;

(* List of all blocks. Initial geometry is given as they should appear
first.  Coordinate system have coordinate center on top left.  *)
let all_blocks = [
  { kind = I;
    geometry = [(0,0);(1,0);(2,0);(3,0)];
    center = (1.5, 0.5);
    initial_position = (0,0);
    color = Cyan };
  { kind = J;
    geometry = [(0,0);(1,0);(2,0);(0,-1)];
    center = (1.,0.);
    initial_position = (0,0);
   color = Blue };
  { kind = L;
    geometry=[(0,0);(1,0);(2,0);(2,-1)];
    center = (1.,0.);
    initial_position = (0,0);
    color = Orange };
  { kind = O;
    geometry = [(0,0);(1,0);(0,1);(1,1)];
    center = (0.5,0.5);
    initial_position = (0,0);
    color = Yellow };
  { kind = S;
    geometry = [(0,0);(1,0);(1,-1);(2,-1)];
    center = (1.,0.);
    initial_position = (0,0);
    color = Green };
  { kind = T;
    geometry = [(0,0);(1,0);(2,0);(1,-1)];
    center = (1.0,0.0);
    initial_position = (0,0);
    color = Purple };
  { kind = Z;
    geometry = [(0,0);(1,0);(1,1);(2,1)];
    center = (1.,1.);
    initial_position = (0,0);
    color = Red }
  ];;

(* 2x2 rotation matrices stored by row *)
type rotation = R0 | R90 | R180 | R270 ;;
  
let rotation_matrix = function
  | R0 -> (1.,0.,0.,1.)
  | R90 -> (0.,-1.,1.,0.)
  | R180 -> (-1.,0.,0.,-1.)
  | R270 -> (0.,1.,-1.,0.);;

let right_rotation = function  
  | R0 -> R90
  | R90 -> R180
  | R180 -> R270
  | R270 -> R0;;

let left_rotation = function  
  | R0 -> R270
  | R90 -> R0
  | R180 -> R90
  | R270 -> R180;;

let rotate (r:float*float*float*float) (c:float*float) (p:xy) : xy =
  let (xc,yc) = c and (x,y) = p and (r11,r12,r21,r22) = r in
  let rx = float x and ry = float y in
  (truncate ((rx -. xc) *. r11 +. (ry -. yc) *. r12  +. xc),
   truncate ((rx -. xc) *. r21 +. (ry -. yc) *. r22  +. xc));;

type field = {width:int; height:int; cells:cell list};;

let make_field w h : field
  = {width=w; height=h; cells = make (w*h) Empty };;

let pick_random l  =  nth l (Random.int (length l));;
                         
type state = {
    score: int;
    field: field;
    piece: tetrimino;
    position: xy;
    rotation: rotation;
  } ;;
  
let iter2D l w f =
  let rec inter2D' l f w x y=
    if is_empty l then ()
    else ((f (hd l) x y) ; inter2D' (tl l) f w (if x=w then 0 else x+1) (if x=w then (y+1) else y))
    in inter2D' l f (w-1) 0 0;;
  
type action =  MoveLeft | MoveRight | Rotate | Drop ;;

