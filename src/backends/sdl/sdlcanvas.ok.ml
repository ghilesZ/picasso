open Tools

module D = struct
  let renderer : Sdltype.renderer option ref = ref None

  let get_r () =
    match !renderer with
    | None -> failwith "sdl renderer not set"
    | Some r -> r

  type color = int * int * int

  let ending () = ()

  let width () = 1.

  let height () = 1.

  let normalize _ _ = Fun.id

  let rgb r g b = (r, g, b)

  let draw_text rgb _pos (_x, _y) _str =
    let r = get_r () in
    Sdlrender.set_draw_color r ~rgb ~a:255 ;
    ()

  let draw_line ~dashed rgb (x1, y1) (x2, y2) =
    let r = get_r () in
    Sdlrender.set_draw_color r ~rgb ~a:255 ;
    ignore dashed ;
    Sdlrender.draw_line r ((iof x1, iof y1), (iof x2, iof y2))

  let draw_circle _col (_x, _y) _r = ()

  let fill_circle _col (_x, _y) _r = ()

  let draw_poly _col _pts = ()

  let fill_poly _col _pts = ()
end

module Draw = Drawer.Make (D)

let build render =
  let open Rendering in
  let width = render.window.sx |> iof in
  let height = render.window.sy |> iof in
  let _window, renderer =
    Sdlrender.create_window_and_renderer ~width ~height ~flags:[]
  in
  D.renderer := Some renderer ;
  Sdl.init [`VIDEO] ;
  Sdlrender.set_draw_color renderer ~rgb:(255, 255, 255) ~a:255 ;
  Sdlrender.clear renderer ;
  let rec aux () =
    Draw.draw render ;
    Sdlrender.render_present renderer ;
    Sdltimer.delay ~ms:10 ;
    match Sdlevent.poll_event () with
    | Some (Sdlevent.Quit _) -> Sdl.quit ()
    | _ -> aux ()
  in
  aux ()
