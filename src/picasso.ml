module Colors = Colors
module Rendering = Rendering
module Rendering3d = Rendering3d
module Drawable = Drawable

let in_gtk_canvas render = Canvas.build render

let to_latex ?tikz_only:(t = true) render output =
  Texcanvas.tikz_only := t ;
  Tex.output render output

let to_svg render output = Svg.output render output

let to_obj render output = Obj.output render output
