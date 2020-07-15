(******************************************************************************)
(* This file is an extension for the Generator1 module from the apron Library *)
(******************************************************************************)
open Apron
open Apron_utils

(* It only adds function, nothing is removed *)
include Apron.Generator1
include Array_maker.GeneratorExt

(* (\* Converts a Generator1 into a float array array. *\)
 * let to_float_array gens size =
 *   (\* Converts a Generator0 into an float array. *\)
 *   let to_float_array gen size =
 *     let tab = Array.make size 0. in
 *     let gen_lin = gen.Generator0.linexpr0 in
 *     for i=0 to (size-1) do
 *       let coeff = Linexpr0.get_coeff gen_lin i in
 *       tab.(i) <- coeff_to_float coeff
 *     done;
 *     tab
 *   in
 *   let gen_tab = gens.Generator1.generator0_array in
 *   let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
 *   for i=0 to ((Array.length gen_tab)-1) do
 *     tab.(i) <- to_float_array gen_tab.(i) size
 *   done;
 *   tab *)

(* Converts a Generator0 into an array of floats. *)
let to_float_array gen size =
  let tab = Array.make size 0. in
  let gen_lin = gen.Generator0.linexpr0 in
  for i=0 to (size-1) do
    let coeff = Linexpr0.get_coeff gen_lin i in
    tab.(i) <- coeff_to_float coeff
  done;
  tab

(* Converts a Generator1 into an array of array of floats. *)
let to_float_array gens =
  let size = Environmentext.size (array_get gens 0).env in
  let gen_tab = gens.Generator1.generator0_array in
  let tab = Array.make (Array.length gen_tab) (Array.make size 0.) in
  for i=0 to ((Array.length gen_tab)-1) do
    tab.(i) <- to_float_array gen_tab.(i) size
  done;
  tab

(* constructs a new generator in opposite direction *)
let neg (d:Generator1.t) : Generator1.t =
  let d = Generator1.copy d in
  Generator1.iter (fun c v -> Generator1.set_coeff d v (Coeff.neg c)) d;
  d

(*returns a generator corresponding to a float point*)
let of_float_point env coeffs =
  let l = Linexpr1.make env in
  let coeffs = List.mapi (fun i e ->
                   (Coeff.s_of_float e), Environmentext.var_of_dim env i)
                         coeffs
  in
  Linexpr1.set_list l coeffs None;
  make l VERTEX

let to_vertices2D (g:t) v1 v2 =
  let a_v1 = Apron.Var.of_string v1
  and a_v2 = Apron.Var.of_string v2 in
  let l = get_linexpr1 g in
  Apron.Linexpr1.(coeff_to_float (get_coeff l a_v1), coeff_to_float (get_coeff l a_v2))
