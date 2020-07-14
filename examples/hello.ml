open Picasso

let _ =
  let r = Rendering.create ~title:"test" 400. 400. in
  Canvas.show r
