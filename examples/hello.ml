open Picasso
open Apronext

let env = Environmentext.make_s [||] [|"x"; "y"; "z"|]

let gens =
  [Generatorext.of_float_point env [0.;0.;0.];
   Generatorext.of_float_point env [0.;100.;50.] ;
   Generatorext.of_float_point env [50.;200.;100.] ;
   Generatorext.of_float_point env [100.;0.;150.]]

let polyhedron = Apol.of_generator_list gens |> Drawable.of_pol
let octagon = Aoct.of_generator_list env gens |> Drawable.of_oct

let _ =
  let r = Rendering.create ~abciss:"z" ~ordinate:"x" ~title:"Test" 800. 800. in
  let r = Rendering.add_l r [(150,150,150), polyhedron; (200,50,50), octagon] in
  to_svg r "file.svg";
  to_latex ~tikz_only:false r "file.tex";
  let r3 = Rendering3d.create ~abciss:"x" ~ordinate:"y" ~height:"z" () in
  let r3 = Rendering3d.add_l r3 [polyhedron; octagon] in
  to_obj r3 "file.obj";
  in_gtk_canvas r
