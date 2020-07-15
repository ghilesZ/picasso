open Picasso
open Apronext

let _ =
  let env = Environmentext.make_s [||] [|"x"; "y"|] in
  let g1 = Generatorext.of_float_point env [0.;0.] in
  let g2 = Generatorext.of_float_point env [0.;1.] in
  let g3 = Generatorext.of_float_point env [0.5;2.] in
  let g4 = Generatorext.of_float_point env [1.;0.] in
  let poly = Apronext.Apol.of_generator_list env [g1;g2;g3;g4] in
  let r = Rendering.create ~title:"Test" 800. 800. in
  let r = Rendering.add r ((150,150,150), Drawable.of_pol poly) in
  Canvas.show r
