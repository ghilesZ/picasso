include Drawer.Make(Svgcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Svgcanvas.output := Some fmt

let output ?filename render =
  let open Rendering in
  let fn = Filename.(temp_file ~temp_dir:current_dir_name (match filename with
        | Some s -> s
        | None -> match render.window.title with None -> "picasso" | Some s -> s )
      ".svg") in
  Svgcanvas.init render.window.sx render.window.sy fn;
  fill_poly white (screen());
  draw render;
  Svgcanvas.ending()
