(* useful type aliases *)
type point = float * float
type range = float * float

module type DrawingBackend = sig
  type internal
  type color
  val internal : internal (* backend specific settings *)
  val init : internal -> unit
  val ending : unit -> unit

  val width : unit -> int
  val height : unit -> int

  val normalize : range -> range -> point -> point

  val rgb : int -> int -> int -> color

  val draw_text : color -> [<`Center|`Left |`Right ] -> point -> string -> unit

  val draw_line : color -> point -> point -> unit

  val draw_circle : color -> point -> float -> unit
  val fill_circle : color -> point -> float -> unit

  val draw_poly : color -> point list -> unit
  val fill_poly : color -> point list -> unit

end

module Make(D:DrawingBackend) = struct

  (**************************************)
  (* Initialization and Backend setting *)
  (**************************************)

  let _ = D.init D.internal

  let ending = D.ending

  let width = D.width () |> float
  let height = D.height () |> float

  let x_min = ref 0.
  let x_max = ref 0.
  let y_min = ref 0.
  let y_max = ref 0.

  (* screen as a polygon *)
  let screen = [(0.,0.); (width,0.); (width,height); (0.,height)]

  let to_backend_coord (x,y) =
     D.normalize (!x_min,!x_max) (!y_min,!y_max) (x,y)

  (* few constants colors *)
  let black     = D.rgb 0 0 0
  let white     = D.rgb 255 255 255
  let lightgray = D.rgb 211 211 211
  let gray      = D.rgb 128 128 128

  (********************************)
  (* redefining drawing utilities *)
  (********************************)

  let clear () =
    D.fill_poly white [(0.,0.); (width,0.); (width,height); (0.,height)]

  let draw_line col p1 p2 =
    let p1 = to_backend_coord p1 and p2 = to_backend_coord p2 in
    D.draw_line col p1 p2

  let draw_text col pos (text_x, text_y) text =
    let text_x,text_y = to_backend_coord (text_x, text_y) in
    D.draw_text col pos (text_x, text_y) text

  let fill_circle col (cx,cy as center) rad =
    let (px,_ as p) = to_backend_coord center in
    let (px',_) = to_backend_coord (cx+.rad,cy) in
    let rad = px' -. px in
    D.fill_circle col p rad

  let fill_poly col vertices =
    match vertices with
    | [] -> ()
    | [x] -> fill_circle col x 2.
    | [(xa,ya);(xb,yb)] -> draw_line col (xa,ya) (xb,yb)
    | _ ->
       let vertices = List.rev_map to_backend_coord vertices in
       D.fill_poly col vertices

  let draw_poly col vertices =
    match vertices with
    | [] -> ()
    | [x] ->  fill_circle col x 8.
    | [a;b] -> draw_line col a b
    | _ ->
       let vertices = List.rev_map to_backend_coord vertices in
       D.draw_poly col vertices

   (* Filled, black-outlined polygon *)
  let polygon col vertices =
    fill_poly col vertices;
    draw_poly black vertices

end
