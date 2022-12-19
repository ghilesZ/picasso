include Drawer.Make (Texcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Texcanvas.output := Some fmt

let output ?filename render =
  let open Rendering in
  let fn = Tools.spawn_filename filename render.window.title "picasso" "tex" in
  Texcanvas.init fn ;
  List.iter
    (fun (c, _) -> ignore (Texcanvas.define_color c))
    Rendering.(render.elems) ;
  draw render ;
  Texcanvas.ending ()
