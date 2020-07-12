open Tools

(* Speed *)
let sx = 1000.
and sy = 1000.
let zo = 2.

type color        =  int * int * int

type t = {
    window      : window_settings;
    scene       : scene_settings;
    dim_handler : dh;
    (* graphical options *)
    grid        : bool;
    axes        : bool;
  }

and window_settings = {
    padding : float;
    sx : float;
    sy : float;
  }

and scene_settings = {
    (* the bounds of the scene *)
    x_min  : float;
    x_max  : float;
    y_min  : float;
    y_max  : float;
  }

and dh = {
    x_var : string;
    y_var : string;
    cuts  : (string * float) list;
  }

let empty_scene = {
    x_min  = -1000.;
    x_max  = 1000.;
    y_min  = -1000.;
    y_max  = 1000.;
  }

let initital_dh = {
    x_var = "x";
    y_var = "y";
    cuts  = [];
  }

let create ?padding:(pad=60.) sx sy = {
    window      = {padding = pad; sx; sy};
    scene       = empty_scene;
    dim_handler = initital_dh;
    axes        = true;
    grid        = true;
  }

let get_x render = render.dim_handler.x_var

let get_y render = render.dim_handler.y_var

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
  let nxmin = center_x +. (a.scene.x_min -. center_x) *. alpha in
  let nymin = center_y +. (a.scene.y_min -. center_y) *. alpha in
  let nxmax = center_x +. (a.scene.x_max -. center_x) *. alpha in
  let nymax = center_y +. (a.scene.y_max -. center_y) *. alpha in
  {a with scene = {x_min = nxmin;
                   x_max = nxmax;
                   y_min = nymin;
                   y_max = nymax;}}

let zoom_scene a = scale_scene a zo

let unzoom_scene a = scale_scene a (1./.zo)

let change_var_x x a = {a with dim_handler = {a.dim_handler with x_var = x}}

let change_var_y y a = {a with dim_handler = {a.dim_handler with y_var = y}}

let change_size_x x a = {a with window = {a.window with sx = x}}

let change_size_y y a= {a with window = {a.window with sy = y}}

let change_size x y a = {a with window = {a.window with sx = x; sy = y}}

(* (\**************************************************************************\)
 * (\* string manipulation of some properties : useful to communicate with gui*\)
 * (\**************************************************************************\)
 *
 * let set_property r key value =
 *   match key with
 *   | "X axis" -> change_var_x value r
 *   | "Y axis" -> change_var_y value r
 *   | "x_min"  -> {r with scene = {r.scene with x_min = int_of_string value |> foi }}
 *   | "x_max"  -> {r with scene = {r.scene with x_max = int_of_string value |> foi }}
 *   | "y_min"  -> {r with scene = {r.scene with y_min = int_of_string value |> foi }}
 *   | "y_max"  -> {r with scene = {r.scene with y_max = int_of_string value |> foi }}
 *   | _ -> failwith ("unknown property"^key)
 *
 * let properties r = [
 *     ("X axis", r.dim_handler.x_var);
 *     ("Y axis", r.dim_handler.y_var);
 *     "x_min", (string_of_int (iof r.scene.x_min));
 *     "x_max", (string_of_int (iof r.scene.x_max));
 *     "y_min", (string_of_int (iof r.scene.y_min));
 *     "y_max", (string_of_int (iof r.scene.y_max))
 *   ] *)

(****************************)
(* abstract element edition *)
(****************************)

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
