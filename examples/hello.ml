open Picasso

let _ =
  let r = Rendering.create ~title:"Test" 800. 800. in
  Canvas.show r
