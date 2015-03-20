open Batteries
open BatList


(* Conversion between G and tick period (in seconds)
"Gravity is expressed in unit G, where 1G = 1 cell per frame, and 0.1G = 1 cell per 10 frames."
http://tetris.wikia.com/wiki/Drop
 *) 
let gravity_period g = 1. /. (g *. 60.)

type color =  Cyan  | Yellow  | Purple  | Green  | Red  | Blue  | Orange
type cell = Empty | Color of color
type tetrimino_kind = I | J | L | O | S | T | Z

type xy = (int*int) 
let xyplus a b : xy = let (ax,ay)=a and (bx,by)=b in (ax+bx,ay+by)
let xyeq a b = let (ax,ay)=a and (bx,by)=b in ax=bx && ay=by

type tetrimino = {
    kind: tetrimino_kind;
    geometry: xy list;
    center: float*float;
    color: color;
} 

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
  ]

(* 2x2 rotation matrices stored by row *)
type rotation = R0 | R90 | R180 | R270 
  
let rotation_matrix = function
  | R0 -> (1.,0.,0.,1.)
  | R90 -> (0.,-1.,1.,0.)
  | R180 -> (-1.,0.,0.,-1.)
  | R270 -> (0.,1.,-1.,0.)

let clockwise_rotation = function
  | R0 -> R90
  | R90 -> R180
  | R180 -> R270
  | R270 -> R0

let counter_clockwise_rotation = function
  | R90 -> R0
  | R180 -> R90
  | R270 -> R180
  | R0 -> R270

let rotate (r:float*float*float*float) (c:float*float) (p:xy) : xy =
  let (xc,yc) = c and (x,y) = p and (r11,r12,r21,r22) = r in
  let rx = float x and ry = float y in
  let dx = rx -. xc and dy = ry -. yc in
  (truncate ((dx *. r11 +. dy *. r12)  +. xc),
   truncate ((dx *. r21 +. dy *. r22)  +. yc))

(* TODO: use http://tetris.wikia.com/wiki/Random_Generator *)  
let pick_random l  =  nth l (Random.int (length l))
                         
type state = {
    level: int;
    score: int;
    width: int;
    height: int;
    cells: cell list;
    tetromino: tetrimino;
    position: xy;
    rotation: rotation;
    over: bool
  } 
  
let iter2D l w f =
  let rec inter2D' l f w x y=
    if is_empty l then []
    else (f (hd l) x y) :: (inter2D' (tl l) f w (if x=w then 0 else x+1) (if x=w then (y+1) else y))
    in inter2D' l f (w-1) 0 0

type action =  MoveLeft | MoveRight | RotateCw | RotateCCw | Drop | Tick

let is_empty = function
  | Empty -> true
  | _ -> false

let cell_available state (x,y) =
  (x>=0 && x<state.width && y>=0 && y<=(state.height-1)) &&
  is_empty (nth state.cells (y*state.width+x))
    
let fits state = 
  BatList.fold_left (&&) true
                    (BatList.map ((cell_available state)
                                  % (xyplus state.position)
                                  % (rotate (rotation_matrix state.rotation) state.tetromino.center))
                                 state.tetromino.geometry)

let emboss state =
  let c = BatList.map
            ((xyplus state.position) % (rotate (rotation_matrix state.rotation) state.tetromino.center))
            state.tetromino.geometry in
  {state with cells=
                (iter2D state.cells state.width
                        (fun v px py ->
                         if exists (xyeq (px,py)) c
                         then Color state.tetromino.color
                         else v))
  }

let spawn_position p board_width = (truncate ((float board_width /. 2.) -. (fst p.center)), 0)

let new_pice_or_game_over state =
  let p = pick_random all_tetrominoes in
  let (x,y) = spawn_position p state.width in
  let newstate = {state with tetromino=p; position=(x,y); rotation=R0} in
  if fits newstate then
    newstate
  else
    {state with over=true}

let initial_state board_width board_height =
  let p = pick_random all_tetrominoes in
  {level = 0;
   score = 0;
   width = board_width;
   height = board_height;
   cells = make (board_width*board_height) Empty;
   tetromino = p;
   position = spawn_position p board_width;
   rotation = R0;
   over = false;
  }

let rec is_full_line = function
  | [] -> true
  | x::xs -> if is_empty x then false else is_full_line xs

(* Calculates how many points user should be awarded based on
number of cleared lines and current level.
http://tetris.wikia.com/wiki/Scoring *)
let scrore_update nlines level = match nlines with
  | 0 -> 0
  | 1 ->  40 * (level + 1)
  | 2 -> 100 * (level + 1)
  | 3 -> 300 * (level + 1)
  | 4 -> 1200 * (level + 1)
  | _ -> invalid_arg "impossibe # lines cleared"

let clear_lines state =
  let lines = ntake state.width state.cells in
  let olines = filter (not % is_full_line) lines in
  let dropped = (length lines) - (length olines) in
  let extra = make (dropped*state.width) Empty in
  {state with
    cells=append extra (flatten olines);
    score = state.score + (scrore_update dropped state.level)
  }
    
let rec update_state event state : state =
  let (x,y) = state.position in
  let try_state s = if fits s then s else state in
  match event with
  | MoveLeft -> try_state {state with position = (x-1,y)}
  | MoveRight -> try_state {state with position = (x+1,y)}
  | RotateCw -> try_state {state with rotation = clockwise_rotation state.rotation}
  | RotateCCw -> try_state {state with rotation = counter_clockwise_rotation state.rotation}
  | Drop ->
     let newstate = {state with position = (x,y+1)} in
     if fits newstate then update_state Drop newstate
     else (new_pice_or_game_over % clear_lines % emboss) state 
  | Tick ->
     let newstate = {state with position = (x,y+1)} in
     if fits newstate then newstate
     else (new_pice_or_game_over % clear_lines % emboss) state


