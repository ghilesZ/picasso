open Picasso
open Apronext

let _ =
  let env = Environmentext.make_s [||] [|"x"; "y"|] in
  let g1 = Generatorext.of_float_point env [0.;0.] in
  let g2 = Generatorext.of_float_point env [0.;100.] in
  let g3 = Generatorext.of_float_point env [50.;200.] in
  let g4 = Generatorext.of_float_point env [100.;0.] in
  let poly = Apronext.Apol.of_generator_list env [g1;g2;g3;g4] in
  let r = Rendering.create ~abciss:"x" ~ordinate:"y" ~title:"Test" 800. 800. in
  let r = Rendering.add r ((150,150,150), Drawable.of_pol poly) in
  to_latex r "file.tex";
  Sys.command "pdflatex file.tex; rm -f file.log file.aux" |> ignore;
  in_gtk_canvas r
