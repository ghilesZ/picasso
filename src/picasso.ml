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
let in_gtk_animated step render = Canvas.build ~step render

let in_graphics_animated _step _render = ()

let to_svg_animated nb step render name =
  let rec loop cur r =
    if cur < nb then (
      to_svg r (Format.asprintf "%s_%i" name nb) ;
      loop (cur + 1) (step r) )
  in
  loop 0 render

let show_animated ?max_step:(max = 10) step render =
  try in_gtk_animated step render
  with BackendError s1 -> (
    try in_graphics_animated step render
    with BackendError s2 ->
      Format.eprintf "Picasso warning:\n  %s\n  %s\n" s1 s2 ;
      to_svg_animated max step render
        Filename.(
          temp_file ~temp_dir:current_dir_name
            (Option.value render.window.title ~default:"picasso")
            ".svg") )
