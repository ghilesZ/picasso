module Colors = Colors
module Rendering = Rendering
module Rendering3d = Rendering3d
module Drawable = Drawable

(*TODO: remove this when next release of apron is done (i.e > v0.9.13) *)
let () =
  let open Apron.Manager in
  Printexc.register_printer (function
    | Error e -> Some (Format.asprintf "%a" print_exclog e)
    | _ -> None )

exception BackendError = Manager.BackendError

let in_gtk_canvas = Canvas.build

let in_graphics_canvas = GraphX.build

let to_latex ?filename ?tikz_only:(t = true) render =
  Texcanvas.tikz_only := t ;
  Tex.output ?filename render

let to_svg = Svg.output

let to_obj = Obj.output

let backends : (Rendering.t -> unit) list =
  [in_gtk_canvas; in_graphics_canvas; Svg.output]

let show render =
  try
    List.iter
      (fun f ->
        try f render ; raise Exit
        with BackendError s -> Format.eprintf "Picasso warning:\n%s" s )
      backends
  with Exit -> ()

(* let in_gtk_animated = Canvas.build_animate
 *
 * let in_graphics_animated _state _step _to_render = ()
 *
 * let to_svg_animated nb state step to_render name =
 *   let rec loop cur s =
 *     if cur < nb then (
 *       to_svg (to_render s) (Format.asprintf "%s_%i" name nb) ;
 *       Format.printf "done\n%!" ;
 *       loop (cur + 1) (step s) )
 *   in
 *   loop 0 state
 *
 * let show_animated ?max_step:(max = 100) state step to_render =
 *   try in_gtk_animated state step to_render
 *   with BackendError s1 -> (
 *     try in_graphics_animated state step to_render
 *     with BackendError s2 ->
 *       Format.eprintf "Picasso warning:\n  %s\n  %s\n" s1 s2 ;
 *       to_svg_animated max state step to_render
 *         Filename.(
 *           temp_file ~temp_dir:current_dir_name
 *             (Option.value (to_render state).window.title ~default:"picasso")
 *             ".svg") ) *)
