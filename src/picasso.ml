module Colors = Colors
module Rendering = Rendering
module Rendering3d = Rendering3d
module Drawable = Drawable

exception BackendError = Tools.BackendError

let in_gtk_canvas render = Canvas.build render

let in_graphics_canvas render = GraphX.build render

let to_latex ?tikz_only:(t = true) render output =
  Texcanvas.tikz_only := t ;
  Tex.output render output

let to_svg = Svg.output

let to_obj = Obj.output

let show render =
  let open Rendering in
  try in_gtk_canvas render
  with BackendError s1 -> (
    try in_graphics_canvas render
    with BackendError s2 ->
      Format.eprintf "Picasso warning:\n  %s\n  %s\n" s1 s2 ;
      to_svg render
        Filename.(
          temp_file ~temp_dir:current_dir_name
            (Option.value render.window.title ~default:"picasso")
            ".svg") )

(* Build animations *)
let in_gtk_animated = Canvas.build_animate

let in_graphics_animated _state _step _to_render = ()

let to_svg_animated nb state step to_render name =
  let rec loop cur s =
    if cur < nb then (
      Format.printf "iteration %i\n%!" cur ;
      Format.printf "outputing svg ...%!" ;
      to_svg (to_render s) (Format.asprintf "%s_%i" name nb) ;
      Format.printf "done\n%!" ;
      loop (cur + 1) (step s) )
  in
  loop 0 state

let show_animated ?max_step:(max = 100) state step to_render =
  try in_gtk_animated state step to_render
  with BackendError s1 -> (
    try in_graphics_animated state step to_render
    with BackendError s2 ->
      Format.eprintf "Picasso warning:\n  %s\n  %s\n" s1 s2 ;
      to_svg_animated max state step to_render
        Filename.(
          temp_file ~temp_dir:current_dir_name
            (Option.value (to_render state).window.title ~default:"picasso")
            ".svg") )
