module Draw = Drawer.Make(Tex)

let show render output =
  Tex.set_output output;
  Draw.draw render
