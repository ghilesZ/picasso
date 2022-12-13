include Drawer.Make (Texcanvas)

let set_output out =
  let oc = open_out out in
  let fmt = Format.formatter_of_out_channel oc in
  Texcanvas.output := Some fmt

let output ?filename render =
  let open Rendering in
  let fn =
    match filename with
    | Some s -> s
    | None ->
        Filename.(
          temp_file ~temp_dir:current_dir_name
            (match render.window.title with None -> "picasso" | Some s -> s)
            ".tex" )
  in
  Texcanvas.init fn ;
  List.iter
    (fun (c, _) -> ignore (Texcanvas.define_color c))
    Rendering.(render.elems) ;
  draw render ;
  Texcanvas.ending ()
