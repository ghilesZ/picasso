let foi = float_of_int

let iof = int_of_float

(** Reverses a string. eg, "kayak" becomes "kayak" *)
let string_rev s =
  let len = String.length s in
  String.init len (fun i -> s.[len - 1 - i])

(** removes beginning zeros of a string *)
let trail_beginning_zeros str =
  let cpt = ref 0 in
  ( try String.iter (function '0' -> incr cpt | _ -> raise Exit) str
    with Exit -> () ) ;
  String.sub str !cpt (String.length str - !cpt)

(** removes ending zeros of a string *)
let trail_ending_zeros str =
  str |> string_rev |> trail_beginning_zeros |> string_rev

let pp_float fmt f =
  let i_c = iof (ceil f) in
  let i_f = iof (floor f) in
  let i =
    if abs_float (float i_c -. f) < abs_float (float i_f -. f) then i_c
    else i_f
  in
  if abs_float (foi i -. f) < 0.001 then Format.fprintf fmt "%i" i
  else Format.fprintf fmt "%s" (trail_ending_zeros (Format.asprintf "%f" f))

let iterate f x0 next until =
  let rec loop () cur = if not (until cur) then loop (f cur) (next cur) in
  loop () x0

(* helper : project from a value n from [a;b] to [c;d] *)
let projection (a, b) (c, d) n =
  let perc (x, y) r = x +. (r *. (y -. x))
  and to_perc (x, y) r =
    if x < 0. then (r -. x) /. (y -. x) else (r -. x) /. (y -. x)
  in
  if b = a then c else perc (c, d) (to_perc (a, b) n)
