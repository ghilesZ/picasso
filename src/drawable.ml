open Apronext
module L = Linconsext
module G = Generatorext
module E = Environmentext

type t = Apol.t list

and point = float list

and range = float * float

and var = string

let of_box b = [Abox.to_poly b]

let of_oct o = [Aoct.to_poly o]

let of_pol p = [p]

let of_gens gl = [Apol.of_generator_list gl]

let of_lcons cl = [Apol.of_lincons_list (List.hd cl |> L.get_env) cl]

let of_hull vars pts =
  let env = E.make_s [||] (Array.of_list vars) in
  [Apol.of_generator_list (List.rev_map (G.of_float_point env) pts)]

let of_ranges vars ranges =
  let vars = Array.of_list vars in
  let env = E.make_s [||] vars in
  let itvf = Array.of_list ranges in
  let itv = Array.map (fun (l, u) -> Intervalext.of_float l u) itvf in
  [Abox.of_box env (Array.map Apron.Var.of_string vars) itv |> Abox.to_poly]

let union : t -> t -> t = List.rev_append

let product x y =
  List.fold_left
    (fun acc x ->
      List.fold_left
        (fun acc y ->
          let m = Apol.meet x y in
          if Apol.is_bottom m then acc else m :: acc )
        acc y )
    [] x

let bounds v1 v2 : t -> Intervalext.t * Intervalext.t = function
  | [] -> invalid_arg "Drawable.bounds: should be non bottom"
  | h :: tl ->
      let i1 = Apol.bound_variable_s h v1 in
      let i2 = Apol.bound_variable_s h v2 in
      List.fold_left
        (fun (i1, i2) p ->
          let i1' = Apol.bound_variable_s p v1 in
          let i2' = Apol.bound_variable_s p v2 in
          (Intervalext.join i1 i1', Intervalext.join i2 i2') )
        (i1, i2) tl

let print fmt (t : t) =
  Format.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:Tools.newline_sep Apol.print)
    t

(* use box printing instead of polyhedra if possible *)
let pp_print_elem fmt p =
  let b = Apol.to_box p in
  let p' = Abox.to_poly b in
  if Apol.is_leq p' p then Format.fprintf fmt "%a" Abox.pp_print b
  else Format.fprintf fmt "%a" Apol.pp_print p

let pp_print fmt (p : t) =
  Format.fprintf fmt "%a"
    (Format.pp_print_list ~pp_sep:Tools.newline_sep pp_print_elem)
    p
