open Apronext

module L = Linconsext
module G = Generatorext
module E = Environmentext

type t =
  | Box         of Abox.t
  | Oct         of Aoct.t
  | Pol         of Apol.t
  | Generators  of G.t list
  | Constraints of L.t list
  | Convex      of string list * point list
  | Hcube       of (string * range) list

and point = float list
and range = float * float

let of_box x = Box x
let of_oct x = Oct x
let of_pol x = Pol x
let of_gens x = Generators x
let of_lcons x = Constraints x
let of_hull vars vertices = Convex (vars,vertices)
let of_ranges vars bounds = Hcube (List.combine vars bounds)

let to_poly = function
  | Generators  gl    -> Apol.of_generator_list (List.hd gl |> G.get_env) gl
  | Constraints cl    -> Apol.of_lincons_list (List.hd cl |> L.get_env) cl
  | Box         b     -> Abox.to_poly b
  | Oct         o     -> Aoct.to_poly o
  | Pol         p     -> p
  | Convex (vars,pts) ->
     let env = E.make_s [||] (Array.of_list vars) in
     Apol.of_generator_list env (List.map (G.of_float_point env) pts)
  | Hcube      ranges ->
     let vars,ranges = List.split ranges in
     let vars = Array.of_list vars in
     let env = E.make_s [||] vars in
     let itvf = Array.of_list ranges in
     let itv = Array.map (fun (l,u) -> Apron.Interval.of_float l u) itvf in
     Abox.of_box env (Array.map Apron.Var.of_string vars) itv
     |> Abox.to_poly

(* if no variable is specified, then the two first of the environment
   are picked *)
let fit2d ?x ?y abs =
  let p_abs = to_poly abs in
  let get_v =
    let nb = ref 0 in
    fun () -> let r = E.var_of_dim p_abs.env !nb in incr nb; r
  in
  let x =
    try Option.get x |> Apron.Var.of_string
    with Invalid_argument _ -> get_v ()
  in
  let y =
    try Option.get y |> Apron.Var.of_string
    with Invalid_argument _ -> get_v ()
  in Apol.proj2D p_abs x y
