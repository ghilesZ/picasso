module Make (D : Manager.T) = struct
  (* Initialization and Backend setting *)

  let x_min = ref 0.

  let x_max = ref 0.

  let y_min = ref 0.

  let y_max = ref 0.

  let nb_graduation =
    10. (* target number of graduation, the actual will differ slightly *)

  let square ((a, b) as bl) ((c, d) as tr) = [bl; (c, b); tr; (a, d)]

  (* screen as a polygon *)
  let screen () = square (0., 0.) (D.width (), D.height ())

  let to_backend_coord (x, y) =
    D.normalize (!x_min, !x_max) (!y_min, !y_max) (x, y)

  (* few constants colors *)
  let black = D.rgb 0 0 0

  let red = D.rgb 255 0 0

  let white = D.rgb 255 255 255

  let darkgray = D.rgb 64 64 64

  let lightgray = D.rgb 230 230 230

  let gray = D.rgb 128 128 128

  (* redefining drawing utilities *)

  let clear () = D.fill_poly white (screen ())

  let draw_line ~dashed col p1 p2 =
    let p1 = to_backend_coord p1 and p2 = to_backend_coord p2 in
    D.draw_line ~dashed col p1 p2

  let draw_text col pos p text =
    let p = to_backend_coord p in
    D.draw_text col pos p text

  let fill_circle col ((cx, cy) as center) rad =
    let ((px, _) as p) = to_backend_coord center in
    let px', _ = to_backend_coord (cx +. rad, cy) in
    let rad = px' -. px in
    D.fill_circle col p rad

  let poly f col vertices =
    match vertices with
    | [] -> ()
    | [x] -> fill_circle col x 2.
    | [(xa, ya); (xb, yb)] -> draw_line ~dashed:false col (xa, ya) (xb, yb)
    | _ ->
        let vertices = List.rev_map to_backend_coord vertices in
        f col vertices

  let draw_poly = poly D.draw_poly

  let fill_poly = poly D.fill_poly

  (* Filled, black-outlined polygon *)
  let polygon col vertices = fill_poly col vertices ; draw_poly black vertices

  let xline r cur =
    let open Rendering in
    let up = r.scene.y_max and down = r.scene.y_min in
    let p1 = normalize r (cur, down) and p2 = normalize r (cur, up) in
    draw_line ~dashed:true lightgray p1 p2

  let yline r cur =
    let open Rendering in
    let left = r.scene.x_min and right = r.scene.x_max in
    let p1 = normalize r (left, cur) and p2 = normalize r (right, cur) in
    draw_line ~dashed:true lightgray p1 p2

  let closest_power_of_10 x =
    let xl10 = log10 x in
    10. ** Float.round xl10

  let closest_half_power_of_10 x =
    let xl10 = log10 x in
    let up = 10. ** Float.ceil xl10 in
    let down = 10. ** Float.floor xl10 in
    let mid = up /. 2. in
    let dif_down = abs_float (down -. x) in
    let dif_up = abs_float (up -. x) in
    let dif_mid = abs_float (mid -. x) in
    if dif_mid < dif_down then if dif_mid < dif_up then mid else up
    else if dif_down < dif_up then down
    else up

  let draw_grid render =
    let open Rendering in
    D.comment "grid start" ;
    let sx = render.scene.x_max -. render.scene.x_min in
    let sy = render.scene.y_max -. render.scene.y_min in
    let step_x = closest_power_of_10 sx /. 10. in
    let step_y = closest_power_of_10 sy /. 10. in
    let fst_x = (render.scene.x_min /. step_x |> floor) *. step_x in
    let fst_y = (render.scene.y_min /. step_y |> floor) *. step_y in
    Tools.iterate (xline render) fst_x (( +. ) step_x)
      (( < ) render.scene.x_max) ;
    Tools.iterate (yline render) fst_y (( +. ) step_y)
      (( < ) render.scene.y_max) ;
    D.comment "grid end"

  let graduation fx fy render =
    let open Rendering in
    let sx = render.scene.x_max -. render.scene.x_min in
    let sy = render.scene.y_max -. render.scene.y_min in
    let step_x = closest_half_power_of_10 sx /. 2. in
    let step_y = closest_half_power_of_10 sy /. 2. in
    let fst_x = (render.scene.x_min /. step_x |> floor) *. step_x in
    let fst_y = (render.scene.y_min /. step_y |> floor) *. step_y in
    Tools.iterate fx fst_x
      (fun x -> x +. step_x)
      (( < ) (render.scene.x_max +. step_x)) ;
    Tools.iterate fy fst_y
      (fun x -> x +. step_x)
      (( < ) (render.scene.y_max +. step_y))

  let nb_digits n = if n > 1. then 0 else int_of_float (ceil ~-.(log10 n))

  let draw_axes r =
    D.comment "axes start" ;
    let open Rendering in
    let x0, y0 = (10., 10.) in
    let left = 0. and right = r.window.sx in
    let up = r.window.sy and down = 0. in
    let hx, hy = (left, y0) and hx', hy' = (right, y0) in
    let th = 3. in
    let thick_line =
      [(hx, hy +. th); (hx, hy -. th); (hx', hy' -. th); (hx', hy' +. th)]
    in
    fill_poly gray thick_line ;
    let vx, vy = (x0, down) and vx', vy' = (x0, up) in
    let thick_line =
      [(vx +. th, vy); (vx -. th, vy); (vx' -. th, vy'); (vx' +. th, vy')]
    in
    fill_poly gray thick_line ;
    let mb_size = 6. in
    let sx = r.scene.x_max -. r.scene.x_min in
    let sy = r.scene.y_max -. r.scene.y_min in
    let step_x = closest_half_power_of_10 sx /. 2. in
    let step_y = closest_half_power_of_10 sy /. 2. in
    (* horizontal minibars and coordinates *)
    let fx cur =
      let text =
        Format.asprintf "%a"
          (Tools.pp_float ~max_decimals:(nb_digits step_x))
          cur
      in
      let x, _ = normalize r (cur, down) in
      draw_line ~dashed:false gray (x, hy -. mb_size) (x, hy +. mb_size) ;
      draw_text darkgray `Center (x, hy +. 20.) text
    in
    (* vertical minibar coordinates *)
    let fy cur =
      let text =
        Format.asprintf "%a"
          (Tools.pp_float ~max_decimals:(nb_digits step_y))
          cur
      in
      let _, y = normalize r (left, cur) in
      draw_line ~dashed:false gray (vx -. mb_size, y) (vx +. mb_size, y) ;
      draw_text darkgray `Center (vx, y) text
    in
    graduation fx fy r ;
    draw_text black `Center (r.window.sx /. 2., 0.) r.abciss ;
    draw_text black `Center (0., r.window.sy /. 2.) r.ordinate ;
    D.comment "axes end"

  (* main drawing function *)
  let draw r =
    let open Rendering in
    x_min := r.scene.x_min ;
    x_max := r.scene.x_max ;
    y_min := r.scene.y_min ;
    y_max := r.scene.y_max ;
    r |> to_vertices
    |> List.iter (fun ((r, g, b), e) ->
           let c = D.rgb r g b in
           polygon c e ) ;
    let highlight = highlight_to_vertices r in
    List.iter
      (fun ((r, g, b), e) ->
        let c = D.rgb (r / 2) (g / 2) (b / 2) in
        polygon c e )
      highlight ;
    if r.grid then draw_grid r ;
    if r.axis then draw_axes r
end
