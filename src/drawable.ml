open Apronext

module L = Linconsext
module G = Generatorext
module E = Environmentext

type t = Polka.strict Polka.t Apronext.Apol.A.t list
and point = float list
and range = float * float

let of_box b = [Abox.to_poly b]

let of_oct o = [Aoct.to_poly o]

let of_pol p = [p]

let of_gens gl =[Apol.of_generator_list (List.hd gl |> G.get_env) gl]

let of_lcons cl = [Apol.of_lincons_list (List.hd cl |> L.get_env) cl]

let of_hull vars pts =
  let env = E.make_s [||] (Array.of_list vars) in
  [Apol.of_generator_list env (List.rev_map (G.of_float_point env) pts)]

let of_ranges vars ranges =
  let vars = Array.of_list vars in
  let env = E.make_s [||] vars in
  let itvf = Array.of_list ranges in
  let itv = Array.map (fun (l,u) -> Apron.Interval.of_float l u) itvf in
  [Abox.of_box env (Array.map Apron.Var.of_string vars) itv
   |> Abox.to_poly]

let join : t -> t -> t = List.rev_append
