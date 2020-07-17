open Tools
open Apronext
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
    axis        : bool;
    (* content *)
    elems       : (color * Apol.t) list;
    (* projection variables *)
    abciss      : string option;
    ordinate    : string option;
    (* elems projected on the projection variables. We differentiate
       the bounded ones from the unbounded ones for efficiency *)
    bounded     : (color * Geometry.hull) list;
    unbounded   : (color * Apol.t) list;
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

let create ?title ?padding:(pad=60.) ?abciss ?ordinate ?grid ?axis sx sy = {
    window      = {padding = pad; sx; sy; title};
    scene       = empty_scene;
    axis        = Option.value axis ~default:true;
    grid        = Option.value grid ~default:true;
    elems       = [];
    abciss;
    ordinate;
    bounded = [];
    unbounded = [];
  }

let toggle_grid r = {r with grid = not r.grid}
let toggle_axes r = {r with axis = not r.axis}

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

let add r ((c,x): color*Drawable.t) =
  {r with elems = (c,Drawable.to_poly x)::r.elems}

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

(* TODO: recompute screen only when the window changes size and when
   projection variables are changed *)
let abstract_screen r =
  let x = Option.get r.abciss and y = Option.get r.ordinate in
  let scenv = E.make_s [||] [|x; y|] in
  let to_gens (x,y) = Generatorext.of_float_point scenv [x;y] in
  [(0.,0.);(r.window.sx,0.);(r.window.sx,r.window.sy);(0.,r.window.sy)]
  |> List.rev_map (denormalize r)
  |> List.rev_map to_gens
  |> Apol.of_generator_list scenv

let to_vertice r e =
  let gen' = Apol.to_generator_list e in
  List.rev_map (fun g ->
      Generatorext.to_vertices2D_s g
        (Option.get r.abciss) (Option.get r.ordinate)) gen'
  |> Geometry.hull

(* changes the projection variables. if those are different from the
   previous ones we:
 - compute the hull for bounded elements
-  project the unbounded ones on the specified variables *)
let set_proj_vars r v1 v2 =
  match r.abciss,r.ordinate with
  | Some x, Some y when x=v1 && y = v2 -> r
  | _ ->
     let bounded,unbounded =
       List.fold_left (fun (b,u) (c,pol) ->
           let p2d = Apol.proj2D_s pol v1 v2 in
           if Apol.is_bounded p2d then (c,to_vertice r p2d)::b,u
           else b,(c,p2d)::u
         ) ([],[]) r.elems
     in
     {r with abciss = Some v1; ordinate = Some v2; bounded; unbounded;}

(* we setup the two variables for drawing. if None, we pick the two
   first from the first abstract element *)
let setup_vars r =
  let get_v =
    let nb = ref 0 in
    fun env -> let r = E.var_of_dim env !nb in incr nb; r
  in let x =
    match r.abciss with
    | None -> Apron.Var.to_string (get_v (List.hd r.elems |> snd).env)
    | Some x -> x
  in let y =
    match r.ordinate with
    | None -> Apron.Var.to_string (get_v (List.hd r.elems |> snd).env)
    | Some y ->  y
  in set_proj_vars r x y

let to_vertices r =
  let norm = normalize r in
  let screen = abstract_screen r in
  List.fold_left (fun acc (c,e) ->
      let interscreen = Apol.meet e screen in
      if Apol.is_bottom interscreen then acc
      else (c,to_vertice r interscreen)::acc
    ) r.bounded r.unbounded
  |> List.rev_map (fun (c,h) -> c,List.rev_map norm h)
