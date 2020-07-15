open Apron
open Scalar

let scalar_to_float s =
  let res = match s with
    | Mpqf x -> Mpqf.to_float x
    | Float x -> x
    | Mpfrf x -> Mpfrf.to_float ~round:Mpfr.Near x
  in res

let coeff_to_float = function
  | Coeff.Scalar x -> scalar_to_float x
  | Coeff.Interval _ -> failwith "cant convert a coeff.interval to float"
