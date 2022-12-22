open Picasso
open Colors
open Apronext

let env = Environmentext.make_s [||] [|"x1"; "x2"|]

let gens1 =
  [ Generatorext.of_float_point env [0.; 0.]
  ; Generatorext.of_float_point env [0.; 107.125]
  ; Generatorext.of_float_point env [150.666; 107.125]
  ; Generatorext.of_float_point env [150.666; 0.] ]

let p1 = Abox.of_generator_list gens1 |> Drawable.of_box

let _ =
  let r =
    Rendering.create ~abciss:"x1" ~ordinate:"x2" ~title:"Test" 800. 800.
  in
  let r = Rendering.add r (blue, p1) in
  show r
