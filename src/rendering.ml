(* this module handles the link between a physical window (size, padding, etc)
   an abstract scene (zoom, "camera angle"), and the content to be rendered
   (abstract elements) *)

open Tools
open Apronext
module E = Environmentext
module G = Generatorext

(* drag sensitivity *)
let sx = 1000.

and sy = 1000.

(* zoom sensitivity *)
let zo = 1.1

(* elements augmented with a cache, to avoid recomputing projections and convex
   hulls. If the element projected on a pair of variable is bounded, then we
   also put in cache its convex hull *)
type elem =
  { pol: Apol.t
  ; col: Colors.t
  ; proj_cache: (string * string, Apol.t * Geometry.hull option) Hashtbl.t }

type t =
  { window: window_settings
  ; scene: scene_settings
  ; (* graphical options *)
    grid: bool
  ; axis: bool
  ; (* projection variables *)
    abciss: string
  ; ordinate: string
  ; abstract_screen: Apol.t
  ; (* content *)
    elems: (Colors.t * Apol.t) list
  ; interscreen:
      (Colors.t * Apol.t) list (* the elements that are visible in the screen *)
  ; env2d: E.t (* 2d apron environment *)
  ; highlighted: (Colors.t * Apol.t) list (* elems under cursor *)
  ; (* elems projected on the projection variables. We differentiate the bounded
       ones from the unbounded ones for efficiency *)
    bounded: (Colors.t * Geometry.hull) list
  ; unbounded: (Colors.t * Apol.t) list }

and window_settings =
  {padding: float; sx: float; sy: float; title: string option}

(* drawing scenes are bounded *)
and scene_settings = {x_min: float; x_max: float; y_min: float; y_max: float}

let empty_scene =
  {x_min= infinity; x_max= neg_infinity; y_min= infinity; y_max= neg_infinity}

let scene_size {x_min; x_max; y_min; y_max} = (x_max -. x_min, y_max -. y_min)

(* given a window and a scene, returns a function that maps an abstract
   coordinate to a point of the scene to the window *)
let normalize s w =
  let to_coord (min_x, max_x) (min_y, max_y) (a, b) =
    let a = projection (min_x, max_x) (w.padding, w.sx -. w.padding) a
    and b = projection (min_y, max_y) (w.padding, w.sy -. w.padding) b in
    (a, b)
  in
  to_coord (s.x_min, s.x_max) (s.y_min, s.y_max)

(* given a scene and a window, returns a function that maps a point of the
   window to the abstract coordinate of the scene *)
let denormalize s w =
  let to_coord (min_x, max_x) (min_y, max_y) (a, b) =
    let a = projection (w.padding, w.sx -. w.padding) (min_x, max_x) a
    and b = projection (w.padding, w.sy -. w.padding) (min_y, max_y) b in
    (a, b)
  in
  to_coord (s.x_min, s.x_max) (s.y_min, s.y_max)

(* helper that computes an abstract screen from a scene and a window *)
let update_screen s w env2d =
  let to_gens (x, y) = G.of_float_point env2d [x; y] in
  [(0., 0.); (w.sx, 0.); (w.sx, w.sy); (0., w.sy)]
  |> List.rev_map (fun pt -> to_gens (denormalize s w pt))
  |> Apol.of_generator_list

(* helper that computes the list of abstract elements that are visible *)
let update_interscreen elems r =
  List.fold_left
    (fun acc (c, e) ->
      let e = Apol.change_environment e r.env2d in
      let e_screen = Apol.meet e r.abstract_screen in
      if Apol.is_bottom e_screen then acc else (c, e_screen) :: acc )
    [] elems

(* initialization of an empty rendering object *)
let create ?title ?padding:(pad = 60.) ?grid ?axis ~abciss ~ordinate sx sy =
  let window = {padding= pad; sx; sy; title} in
  let scene = empty_scene in
  let env2d = E.make_s [||] [|abciss; ordinate|] in
  let abstract_screen = Apol.bottom env2d in
  { window
  ; scene
  ; axis= Option.value axis ~default:true
  ; grid= Option.value grid ~default:true
  ; elems= []
  ; interscreen= []
  ; abciss
  ; ordinate
  ; env2d
  ; abstract_screen
  ; bounded= []
  ; unbounded= []
  ; highlighted= [] }

let toggle_grid r = {r with grid= not r.grid}

let toggle_axes r = {r with axis= not r.axis}

(* set new bounds for a scene *)
let set_scene s x_min x_max y_min y_max =
  { x_min= min x_min s.x_min
  ; x_max= max x_max s.x_max
  ; y_min= min y_min s.y_min
  ; y_max= max y_max s.y_max }

(* translation of the scene *)
let translate (x, y) r =
  let x = x /. r.window.sx in
  let y = y /. r.window.sy in
  let lx = (r.scene.x_max -. r.scene.x_min) *. x in
  let ly = (r.scene.y_max -. r.scene.y_min) *. y in
  let scene =
    { x_min= r.scene.x_min -. lx
    ; x_max= r.scene.x_max -. lx
    ; y_min= r.scene.y_min -. ly
    ; y_max= r.scene.y_max -. ly }
  in
  let abstract_screen = update_screen scene r.window r.env2d in
  let interscreen = update_interscreen r.elems r in
  {r with scene; abstract_screen; interscreen}

let scale r alpha =
  let center_x = 0.5 *. (r.scene.x_max +. r.scene.x_min) in
  let center_y = 0.5 *. (r.scene.y_max +. r.scene.y_min) in
  let x_min = center_x +. ((r.scene.x_min -. center_x) *. alpha) in
  let y_min = center_y +. ((r.scene.y_min -. center_y) *. alpha) in
  let x_max = center_x +. ((r.scene.x_max -. center_x) *. alpha) in
  let y_max = center_y +. ((r.scene.y_max -. center_y) *. alpha) in
  let scene = {x_min; x_max; y_min; y_max} in
  let abstract_screen = update_screen scene r.window r.env2d in
  let interscreen = update_interscreen r.elems r in
  {r with scene; abstract_screen; interscreen}

let zoom r = scale r zo

let unzoom r = scale r (1. /. zo)

let change_size_x x r =
  let window = {r.window with sx= x} in
  let abstract_screen = update_screen r.scene window r.env2d in
  {r with window; abstract_screen}

let change_size_y y r =
  let window = {r.window with sy= y} in
  let abstract_screen = update_screen r.scene window r.env2d in
  {r with window; abstract_screen}

let change_size x y r =
  let window = {r.window with sx= x; sy= y} in
  let abstract_screen = update_screen r.scene window r.env2d in
  {r with window; abstract_screen}

let add ?autofit:(auto = true) r ((c, x) : Colors.t * Drawable.t) =
  let r =
    {r with elems= List.fold_left (fun acc e -> (c, e) :: acc) r.elems x}
  in
  let r =
    if auto then
      let i1, i2 = Drawable.bounds r.abciss r.ordinate x in
      let (l1, u1), (l2, u2) = Intervalext.(to_float i1, to_float i2) in
      let scene = set_scene r.scene l1 u1 l2 u2 in
      let abstract_screen = update_screen scene r.window r.env2d in
      {r with scene; abstract_screen}
    else r
  in
  let interscreen =
    List.fold_left
      (fun acc e ->
        let e_screen =
          Apol.proj2D_s e r.abciss r.ordinate |> Apol.meet r.abstract_screen
        in
        if Apol.is_bottom e_screen then acc else (c, e_screen) :: acc )
      r.interscreen x
  in
  {r with interscreen}

let add_l ?autofit:(auto = true) r drawables =
  List.fold_left (add ~autofit:auto) r drawables

(* set the bounds of the abstract scene so that it encompasses all abstract
   elements *)
let focus r =
  let open Intervalext in
  let bounds v =
    List.fold_left
      (fun acc (_, e) ->
        try Apol.bound_variable_s e v |> join acc with Failure _ -> acc )
      bottom r.elems
    |> to_float
  in
  let x_min, x_max = bounds r.abciss and y_min, y_max = bounds r.ordinate in
  let scene = {x_min; x_max; y_min; y_max} in
  let abstract_screen = update_screen scene r.window r.env2d in
  let interscreen = update_interscreen r.elems r in
  {r with scene; abstract_screen; interscreen}

let normalize r =
  let s, w = (r.scene, r.window) in
  normalize s w

let denormalize r =
  let s, w = (r.scene, r.window) in
  denormalize s w

(* convex hull computation *)
let to_vertice abciss ordinate e =
  let gl = Apol.to_generator_list e in
  if abciss = ordinate then
    List.rev_map
      (fun g ->
        let f =
          G.get_coeff g (Apron.Var.of_string abciss) |> Coeffext.to_float
        in
        (f, f) )
      gl
  else
    List.rev_map (fun g -> G.to_vertices2D_s g abciss ordinate) gl
    |> Geometry.hull

let proj_elem elem ((x, y) as key) =
  match Hashtbl.find_opt elem.proj_cache key with
  | Some (e2d, hull) -> (e2d, hull)
  | None ->
      let p2d = Apol.proj2D_s elem.pol x y in
      let res =
        if Apol.is_bounded p2d then (p2d, Some (to_vertice x y p2d))
        else (p2d, None)
      in
      Hashtbl.add elem.proj_cache key res ;
      res

(* computes the union of environments of all variables *)
let get_vars r =
  List.fold_left
    (fun acc (_, elm) -> E.join acc (Apol.get_environment elm))
    E.empty r.elems

(* computes the union of environments of all variables as an array *)
let array_var render =
  let e = get_vars render in
  let a = Array.make (E.size e) "" in
  let i = ref 0 in
  E.iter
    (fun v ->
      a.(!i) <- Apron.Var.to_string v ;
      incr i )
    e ;
  a

(* Changes the projection variables. if those are different from the previous
   ones we: 1) compute the hull for bounded elements 2) project the unbounded
   ones on the specified variables *)
let set_proj_vars r v1 v2 =
  let env2d = E.make_s [||] [|v1; v2|] in
  let r = {r with abciss= v1; ordinate= v2; env2d} in
  let bounded, unbounded =
    List.fold_left
      (fun (b, u) (c, pol) ->
        let p2d = Apol.proj2D_s pol v1 v2 in
        if Apol.is_bounded p2d then ((c, to_vertice v1 v2 p2d) :: b, u)
        else (b, (c, p2d) :: u) )
      ([], []) r.elems
  in
  let abstract_screen = update_screen r.scene r.window env2d in
  focus {r with bounded; unbounded; abstract_screen}

(* computes the list of abstract elements that are under a concrete
   coordinate *)
let hover (pt : Geometry.point) (r : t) : t * bool =
  let mx, my = (denormalize r) pt in
  let genpt = G.of_float_point r.env2d [mx; my] in
  let abspt = Apol.of_generator_list [genpt] in
  let highlighted =
    List.fold_left
      (fun acc (col, e) ->
        let e = Apol.change_environment e r.env2d in
        let constr = Apol.to_lincons_list e in
        if List.for_all (Apol.sat_lincons abspt) constr then (col, e) :: acc
        else acc )
      [] r.interscreen
  in
  if highlighted <> r.highlighted then ({r with highlighted}, true)
  else (r, false)

let highlight_to_vertices r =
  let norm = normalize r in
  let r = set_proj_vars r r.abciss r.ordinate in
  List.fold_left
    (fun acc (c, e) ->
      let interscreen = Apol.meet e r.abstract_screen in
      if Apol.is_bottom interscreen then acc
      else (c, to_vertice r.abciss r.ordinate interscreen) :: acc )
    [] r.highlighted
  |> List.rev_map (fun (c, e) -> (c, List.rev_map norm e))

(* bounded elements will always be drawn, potentially "outside the window" and
   will not be seen, unbounded elements are artificially bounded and only their
   "visible" part is rendered *)
let to_vertices r =
  let norm = normalize r in
  let r = set_proj_vars r r.abciss r.ordinate in
  List.fold_left
    (fun acc (c, e) ->
      let interscreen = Apol.meet e r.abstract_screen in
      if Apol.is_bottom interscreen then acc
      else (c, to_vertice r.abciss r.ordinate interscreen) :: acc )
    r.bounded r.unbounded
  |> List.rev_map (fun (c, h) -> (c, List.rev_map norm h))
