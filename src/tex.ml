(* this module handles the latex generation *)

type internal = string (* output file *)

let output = ref None

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  output := Some fmt

let get_output () =
  try Option.get !output
  with Failure _ -> failwith "tex output should be set before tex generation"

let emit s = Format.fprintf (get_output ()) s

let name = "tempcolor"

let init out =
  set_output out;
  emit "\\documentclass{article}\n\\usepackage{fullpage}\n\\usepackage{tikz}\n\n";
  emit "\\begin{document}\n\n\\centering\n\n";
  emit "\\begin{figure}[t]\n\\centering\n\\begin{tikzpicture}\n";
  emit "\\definecolor{%s}{RGB}{%i,%i,%i}\n" name 0 0 0

let ending () =
  emit "\\end{tikzpicture}\n\\end{figure}\n%!";
  emit "\\end{document}\n%!"

(* helper : project from a value n from [a;b] to [c;d] *)
let projection (a,b) (c,d) n =
  let perc (x,y) r = x +. (r *. (y-.x))
  and to_perc (x,y) r =
    if x < 0. then (r-.x) /. (y-.x)
    else (r-.x) /. (y-.x)
  in
  if b = a then c else perc (c,d) (to_perc (a,b) n)

let normalize (x_min,x_max) (y_min,y_max) (x,y) =
  let x = projection (x_min,x_max) (0.,18.) x in
  let y = projection (y_min,y_max) (0.,18.) y in
  x,y

let width () = 0
let height () = 0

type color = int * int * int
let rgb r g b : color = (r,g,b)

(* outputs the tikz definition of a color
on the std outpout and returns the color name as a string *)
let define_color =
  let old_one = ref (0,0,0) in
  fun ((r,g,b) as col :color) ->
  if !old_one <> col then begin
      old_one := col;
      emit "\\definecolor{%s}{RGB}{%i,%i,%i};\n" name r g b
    end;
  name

let draw_text col pos (x,y) text =
  let text = String.escaped text in
  let pos = match pos with
    | `Center -> "center"
    | `Left | `Right -> failwith "draw text left right not implemented"
  in
  let col = define_color col in
  emit "\\node[align=%s,text=%s,font=\\tiny] at (%f,%f) { %s };\n" pos col x y text

let draw_line col (x1,y1) (x2,y2) =
  let col = define_color col in
  emit "\\draw[%s] (%f,%f) -- (%f,%f);\n" col x1 y1 x2 y2

let circle filldraw (col:color) (x,y) rad =
  let col = define_color col in
  emit "\\%s[%s] (%f,%f) circle (%f);\n" filldraw col x y rad

let fill_circle = circle "fill"

let draw_circle = circle "draw"

let poly filldraw col vertices =
  let col = define_color col in
  emit "\\%s[%s] " filldraw col;
	List.iter (fun (x,y) -> emit "(%f, %f) -- " x y) vertices;
  emit "cycle;@."

let fill_poly = poly "fill"

let draw_poly = poly "draw"
