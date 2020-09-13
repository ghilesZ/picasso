module Rendering = Rendering
module Rendering3d = Rendering3d

module Drawable = Drawable

(** Displays a Rendering.t within a scrollable, zoomable gtk canvas *)
let in_gtk_canvas render =
  GtkMain.Main.init () |> ignore;
  let window = Canvas.build render in
  window#show ();
  GMain.Main.main ()

(* let in_js_canvas _render = () *)

(** Builds a tikz figure corresponding to a Rendering.t  *)
let to_latex render output = Tex.output render output

(** Builds an obj file rorresponding to a Rendering3D context *)
let to_obj render output = Obj.output render output
