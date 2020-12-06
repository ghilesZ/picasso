include Drawer.Make(Svgcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Svgcanvas.output := Some fmt

let output render filename =
  Svgcanvas.init filename;
  draw render;
  Svgcanvas.ending()
