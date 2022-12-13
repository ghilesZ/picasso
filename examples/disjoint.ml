open Picasso
open Colors
open Apronext

let env = Environmentext.make_s [||] [|"x1"; "x2"|]

let gens1 =
  [ Generatorext.of_float_point env [120.; 0.]
  ; Generatorext.of_float_point env [105.; 100.]
  ; Generatorext.of_float_point env [150.; 200.]
  ; Generatorext.of_float_point env [170.; 210.]
  ; Generatorext.of_float_point env [200.; 0.] ]

let gens2 =
  [ Generatorext.of_float_point env [20.; 20.]
  ; Generatorext.of_float_point env [5.; 0.]
  ; Generatorext.of_float_point env [50.; 100.]
  ; Generatorext.of_float_point env [100.; -100.] ]

let p1 = Apol.of_generator_list gens1 |> Drawable.of_pol

let p2 = Apol.of_generator_list gens2 |> Drawable.of_pol

let _ =
  let r =
    Rendering.create ~abciss:"x1" ~ordinate:"x2" ~title:"Test" 800. 800.
  in
  let r = Rendering.add_l r [(blue, p1); (blue, p2)] in
  show r
