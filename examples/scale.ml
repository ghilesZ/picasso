open Picasso
open Colors
open Apronext

let env = Environmentext.make_s [||] [|"x1"; "x2"|]

let mk_gen l = Generatorext.of_float_point env l

let translate_gen dx dy = function
  | [x; y] -> [x +. dx; y +. dy]
  | _ -> invalid_arg "was a expecting a 2d generator"

let _ =
  let r =
    Rendering.create ~abciss:"x1" ~ordinate:"x2" ~title:"Test" 800. 800.
  in
  let gens =
    [[120.; 0.]; [105.; 100.]; [150.; 200.]; [170.; 210.]; [200.; 0.]]
  in
  let translate dx dy =
    List.map (fun pt -> mk_gen @@ translate_gen dx dy pt) gens
    |> Apol.of_generator_list |> Drawable.of_pol
  in
  let rec aux acc cpt =
    if cpt = 0 then acc
    else
      let dx = cpt / 10 in
      let dy = cpt mod 10 in
      aux
        (Rendering.add acc
           (blue, translate (float dx *. 100.) (float dy *. 300.)) )
        (cpt - 1)
  in
  show (aux r 500)
