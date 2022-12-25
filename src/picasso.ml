module Colors = Colors
module Rendering = Rendering
module Rendering3d = Rendering3d
module Drawable = Drawable

(*TODO: remove this when next release of apron is done (i.e > v0.9.13) *)
let () =
  let open Apron.Manager in
  Printexc.register_printer (function
    | Error e -> Some (Format.asprintf "%a" print_exclog e)
    | _ -> None )

exception BackendError = Manager.BackendError

let in_gtk_canvas = Canvas.build

let in_graphics_canvas = GraphX.build

let to_latex ?filename ?tikz_only:(t = true) render =
  Texcanvas.tikz_only := t ;
  Tex.output ?filename render

let to_svg = Svg.output

let to_obj = Obj.output

let backends : (string * (Rendering.t -> unit)) list =
  [ ("gtk", in_gtk_canvas)
  ; ("graphics", in_graphics_canvas)
  ; ("svg", fun r -> Svg.output r) ]

let show render =
  let rec find_backend = function
    | [] -> (* should not occur *) failwith "no backend available"
    | [(_, b)] -> b render
    | (_n1, b1) :: ((n2, _b2) :: _ as tl) ->
        ( try b1 render ; raise Exit
          with BackendError s ->
            Format.eprintf "Picasso warning:\n%s, retrying with %s\n" s n2 ) ;
        find_backend tl
  in
  try find_backend backends with Exit -> ()
