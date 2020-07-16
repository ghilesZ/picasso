module Draw = Drawer.Make(Tex)

let show render =
  Draw.draw render
