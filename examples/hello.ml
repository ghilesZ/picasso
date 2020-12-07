open Picasso
open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"; "z"|]

let polyhedron =
  let g1 = Generatorext.of_float_point env [0.;0.;0.] in
  let g2 = Generatorext.of_float_point env [0.;100.;50.] in
  let g3 = Generatorext.of_float_point env [50.;200.;100.] in
  let g4 = Generatorext.of_float_point env [100.;0.;150.] in
  Apol.of_generator_list env [g1;g2;g3;g4] |> Drawable.of_pol

let octagon =
  let g1 = Generatorext.of_float_point env [100.;0.;0.] in
  let g2 = Generatorext.of_float_point env [100.;100.;50.] in
  let g3 = Generatorext.of_float_point env [150.;200.;100.] in
  let g4 = Generatorext.of_float_point env [200.;0.;150.] in
  Aoct.of_generator_list env [g1;g2;g3;g4] |> Drawable.of_oct

let _ =
  let r = Rendering.create ~abciss:"z" ~ordinate:"x" ~title:"Test" 800. 800. in
  let r = Rendering.add r ((150,150,150), polyhedron) in
  let r = Rendering.add r ((200,50,50), octagon) in
  to_svg r "file.svg";
  to_svg r "file.txt";
  to_latex ~tikz_only:false r "file.tex";
  let r3 = Rendering3d.create ~abciss:"x" ~ordinate:"y" ~height:"z" () in
  let r3 = Rendering3d.add r3 polyhedron in
  let r3 = Rendering3d.add r3 octagon in
  to_obj r3 "file.obj";
  in_gtk_canvas r
