module L = Linconsext
module G = Generatorext

type t =
  | Box         of Abox.t
  | Oct         of Aoct.t
  | Pol         of Apol.t
  | Generators  of Generatorext.t list
  | Constraints of Linconsext.t list

let of_box x = Box x
let of_oct x = Oct x
let of_pol x = Pol x
let of_gens x = Generators x
let of_lcons x = Constraints x

let to_poly = function
  | Generators  gl -> Apol.of_generator_list (List.hd gl |> G.get_env) gl
  | Constraints cl -> Apol.of_lincons_list (List.hd cl |> L.get_env) cl
  | Box         b  -> Abox.to_poly b
  | Oct         o  -> Aoct.to_poly o
  | Pol         p  -> p

(* if no variable is specified, then the two first of the environment
   are picked *)
let fit2d ?x ?y abs =
  let p_abs = to_poly abs in
  let get_v =
    let nb = ref 0 in
    fun () -> let r = Environmentext.var_of_dim p_abs.env !nb in incr nb; r
  in
  let x =
    try Option.get x |> Apron.Var.of_string
    with Invalid_argument _ -> get_v ()
  in
  let y =
    try Option.get y |> Apron.Var.of_string
    with Invalid_argument _ -> get_v ()
  in Apol.proj2D p_abs x y
