module Rendering = Rendering

module Drawable = Drawable

let in_gtk_canvas render =
  GtkMain.Main.init () |> ignore;
  let window = Canvas.build render in
  window#show ();
  GMain.Main.main ()

let in_js_canvas _render = ()

let to_latex =
  let module Draw = Drawer.Make(Tex) in
  fun render output ->
  Tex.set_output output;
  Draw.draw render

let to_obj _render = ()
