include Drawer.Make (Svgcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Svgcanvas.output := Some fmt

let output ?filename render =
  let open Rendering in
  let fn = Tools.spawn_filename filename render.window.title "picasso" ".svg" in
  Svgcanvas.init render.window.sx render.window.sy fn ;
  fill_poly white (screen ()) ;
  draw render ;
  Svgcanvas.ending ()
