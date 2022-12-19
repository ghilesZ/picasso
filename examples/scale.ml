open Picasso
open Colors
open Apronext

let env = Environmentext.make_s [||] [|"x1"; "x2"|]

let mk_gen l = Generatorext.of_float_point env l

let _ =
  let r =
    Rendering.create ~abciss:"x1" ~ordinate:"x2" ~title:"Test" 800. 800.
  in
  let gens =
    [[120.; 0.]; [105.; 100.]; [150.; 200.]; [170.; 210.]; [200.; 0.]]
  in
  let translate d =
    List.map (fun pt -> List.map (( +. ) d) pt |> mk_gen) gens
    |> Apol.of_generator_list |> Drawable.of_pol
  in
  let rec aux acc cpt =
    if cpt = 0 then acc
    else aux (Rendering.add acc (blue, translate (float cpt *. 100.))) (cpt - 1)
  in
  show (aux r 100)
