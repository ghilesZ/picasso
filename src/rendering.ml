open Tools
module E = Environmentext

(* Speed *)
let sx = 1000.
and sy = 1000.
let zo = 2.

type color        =  int * int * int

type t = {
    window      : window_settings;
    scene       : scene_settings;
    (* graphical options *)
    grid        : bool;
    axes        : bool;
    (* content *)
    elems       : (color * Drawable.t) list;
    abciss      : string option;
    ordinate    : string option;
  }

and window_settings = {
    padding : float;
    sx : float;
    sy : float;
    title : string option
  }

and scene_settings = {
    (* the bounds of the scene *)
    x_min  : float;
    x_max  : float;
    y_min  : float;
    y_max  : float;
  }

let empty_scene = {
    x_min  = -1000.;
    x_max  = 1000.;
    y_min  = -1000.;
    y_max  = 1000.;
  }

let create ?title ?padding:(pad=60.) ?abciss ?ordinate sx sy = {
    window      = {padding = pad; sx; sy; title};
    scene       = empty_scene;
    axes        = true;
    grid        = true;
    elems       = [];
    abciss;
    ordinate;
  }

let toggle_grid r = {r with grid = not r.grid}
let toggle_axes r = {r with axes = not r.axes}

(* set new bounds for a scene *)
let set_bounds x_min x_max y_min y_max =
  {x_min; x_max; y_min; y_max}

let translate_scene (x,y) a =
  let x = x /. (a.window.sx) in
  let y = y /. (a.window.sy) in
  let lx =  (a.scene.x_max -. a.scene.x_min) *. x in
  let ly =  (a.scene.y_max -. a.scene.y_min) *. y in
  {a with scene = {
      x_min = a.scene.x_min -. lx;
      x_max = a.scene.x_max -. lx;
      y_min = a.scene.y_min -. ly;
      y_max = a.scene.y_max -. ly;
    }
  }

let scale_scene a alpha =
  let center_x = 0.5 *. (a.scene.x_max +. a.scene.x_min) in
  let center_y = 0.5 *. (a.scene.y_max +. a.scene.y_min) in
  let x_min = center_x +. (a.scene.x_min -. center_x) *. alpha in
  let y_min = center_y +. (a.scene.y_min -. center_y) *. alpha in
  let x_max = center_x +. (a.scene.x_max -. center_x) *. alpha in
  let y_max = center_y +. (a.scene.y_max -. center_y) *. alpha in
  {a with scene = {x_min; x_max; y_min; y_max}}

let zoom_scene a = scale_scene a zo

let unzoom_scene a = scale_scene a (1./.zo)

let change_size_x x a = {a with window = {a.window with sx = x}}

let change_size_y y a= {a with window = {a.window with sy = y}}

let change_size x y a = {a with window = {a.window with sx = x; sy = y}}

let add r x = {r with elems = x::r.elems}

(* given a window and a scene, returns a function that maps an
   abstract coordinate to a point of the scene to the window *)
let normalize u =
  let s,w = u.scene,u.window in
  let to_coord (min_x,max_x) (min_y,max_y) (a,b) =
    let a = projection (min_x,max_x) (w.padding, (w.sx-.w.padding)) a
    and b = projection (min_y,max_y) (w.padding, (w.sy-.w.padding)) b
    in a, b
  in
  to_coord (s.x_min,s.x_max) (s.y_min,s.y_max)

(* given a window and a scene, returns a function that maps an
 *     a point of the window to the abstract coordinate of the scene *)
let denormalize u =
  let s,w = u.scene,u.window in
  let to_coord (min_x,max_x) (min_y,max_y) (a,b) =
    let a = projection (w.padding, (w.sx-.w.padding)) (min_x,max_x) a
    and b = projection (w.padding, (w.sy-.w.padding)) (min_y,max_y) b
    in (a, b)
  in
  to_coord (s.x_min,s.x_max) (s.y_min,s.y_max)

let abstract_screen u x y =
  let scenv = E.make [||] [|x; y|] in
  let to_gens (x,y) = Generatorext.of_float_point scenv [x;y] in
  [(0.,0.);(u.window.sx,0.);(u.window.sx,u.window.sy);(0.,u.window.sy)]
  |> List.map (denormalize u)
  |> List.rev_map to_gens
  |> Apol.of_generator_list scenv

let to_vertices render =
  let norm = normalize render in
  match render.elems with
  | (_,h)::_ ->
     let h2d = Drawable.fit2d ?x:render.abciss ?y:render.ordinate h in
     let x,y = E.((var_of_dim h2d.env 0),(var_of_dim h2d.env 1)) in
     let polyscreen = abstract_screen render x y in
     let to_vert (c,d) =
       let p = Drawable.fit2d ?x:render.abciss ?y:render.ordinate d in
       (c,Apol.(to_vertices2D (meet p polyscreen) x y |> List.rev_map norm))
     in
     List.rev_map to_vert render.elems
  | [] -> []
