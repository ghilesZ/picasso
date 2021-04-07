open Picasso
open Colors
open Apronext

let env = Environmentext.make_s [||] [|"x1"; "x2"; "x3"; "x4"|]

let gens =
  [ Generatorext.of_float_point env [120.; 0.; 100.; 88.]
  ; Generatorext.of_float_point env [105.; 100.; 150.; 55.]
  ; Generatorext.of_float_point env [150.; 200.; 220.; 11.]
  ; Generatorext.of_float_point env [200.; 0.; 250.; 55.] ]

let pol = Apol.of_generator_list gens

let () =
  let down, up = Apol.bound_variable_fs pol "x2" in
  let _step = (up -. down) /. 100. in
  let _cut _i = assert false in
  ()
