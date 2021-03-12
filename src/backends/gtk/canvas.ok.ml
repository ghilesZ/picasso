open Tools

(* This module is a drawing area wrapper that handles mouse events and expose
   event *)
open Geometry
open GButton

let array_var render =
  let open Apronext in
  let env = Rendering.get_vars render in
  let arr = Array.make (Environmentext.size env) "" in
  let cpt = ref 0 in
  Environmentext.iter
    (fun v ->
      arr.(!cpt) <- Apron.Var.to_string v ;
      incr cpt )
    env ;
  arr

let find_index arr v =
  let exception Found of int in
  try
    Array.iteri (fun i v' -> if v' = v then raise (Found i)) arr ;
    None
  with Found i -> Some i

let next_v vars i j =
  let i = (i + 1) mod Array.length vars in
  if i = j then vars.((i + 1) mod Array.length vars) else vars.(i)

let prev_v vars i j =
  let i = (Array.length vars + i - 1) mod Array.length vars in
  if i = j then vars.((Array.length vars + i - 1) mod Array.length vars)
  else vars.(i)

class toolbar ~width ~hpack ~vpack () =
  let tb_h = GPack.hbox ~height:40 ~width:(width - 40) ~packing:hpack () in
  let prev_var_h = button ~packing:tb_h#add () in
  let _lab = GMisc.label ~packing:tb_h#add () in
  let _lab = GMisc.label ~packing:tb_h#add ~text:"X axis: " () in
  let cur_h = GMisc.label ~packing:tb_h#add () in
  let _lab = GMisc.label ~packing:tb_h#add () in
  let next_var_h = button ~packing:tb_h#add () in
  let tb_v = GPack.vbox ~packing:vpack ~width:40 () in
  let prev_var_v = button ~packing:tb_v#add () in
  let _lab = GMisc.label ~packing:tb_v#add () in
  let _lab = GMisc.label ~packing:tb_v#add ~text:"Y axis: " () in
  let cur_v = GMisc.label ~packing:tb_v#add () in
  let _lab = GMisc.label ~packing:tb_v#add () in
  let next_var_v = button ~packing:tb_v#add () in
  let _lab = GMisc.label ~packing:tb_v#add ~height:40 () in
  object (self)
    val mutable abc = 0

    val mutable ord = 0

    val mutable vars = [||]

    val mutable refresh = fun () -> ()

    method set_vars () =
      next_var_h#set_label (next_v vars abc ord) ;
      prev_var_h#set_label (prev_v vars abc ord) ;
      cur_h#set_label vars.(abc) ;
      next_var_v#set_label (next_v vars ord abc) ;
      prev_var_v#set_label (prev_v vars ord abc) ;
      cur_v#set_label vars.(ord)

    method set_refresh f = refresh <- (fun () -> self#set_vars () ; f ())

    method set_render (render : Rendering.t ref) =
      vars <- array_var !render ;
      abc <- find_index vars !render.abciss |> Option.get ;
      ord <- find_index vars !render.ordinate |> Option.get ;
      self#set_vars () ;
      ignore
        (prev_var_h#connect#clicked ~callback:(fun () ->
             abc <- (Array.length vars + abc - 1) mod Array.length vars ;
             if abc = ord then
               abc <- (Array.length vars + abc - 1) mod Array.length vars ;
             render := Rendering.set_proj_vars !render vars.(abc) vars.(ord) ;
             refresh () ) ) ;
      ignore
        (next_var_h#connect#clicked ~callback:(fun () ->
             abc <- (abc + 1) mod Array.length vars ;
             if abc = ord then abc <- (abc + 1) mod Array.length vars ;
             render := Rendering.set_proj_vars !render vars.(abc) vars.(ord) ;
             refresh () ) ) ;
      ignore
        (prev_var_v#connect#clicked ~callback:(fun () ->
             ord <- (Array.length vars + ord - 1) mod Array.length vars ;
             if abc = ord then
               ord <- (Array.length vars + ord - 1) mod Array.length vars ;
             render := Rendering.set_proj_vars !render vars.(abc) vars.(ord) ;
             refresh () ) ) ;
      ignore
        (next_var_v#connect#clicked ~callback:(fun () ->
             ord <- (ord + 1) mod Array.length vars ;
             if abc = ord then ord <- (ord + 1) mod Array.length vars ;
             render := Rendering.set_proj_vars !render vars.(abc) vars.(ord) ;
             refresh () ) ) ;
      ()
  end

let toolbar ~width ~hpack = new toolbar ~width ~hpack ()

class clickable ~packing ~width ~height () =
  (* Create the containing vbox. *)
  let da = GMisc.drawing_area ~width ~height ~packing () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in
  let mc = Gdk.Cursor.create `ARROW in
  let oc = Gdk.Cursor.create `FLEUR in
  object (self)
    initializer
    ignore
      ( da#event#add
          [`BUTTON_PRESS; `BUTTON_RELEASE; `POINTER_MOTION; `SCROLL] ;
        () )

    val mutable tolerance = 900.

    val mutable old_right = None

    val mutable old_left = None

    method private set_moving_left (x, y) = old_left <- Some (x, y)

    method private unset_moving_left () = old_left <- None

    method get_drawable () =
      try Lazy.force drawable
      with Gpointer.Null -> failwith "drawable null"

    method private get_coord (x, y) =
      (* in gtk y axis is inversed *)
      let _, b = (self#get_drawable ())#size in
      (x, float b -. y)

    method private expose (f : int * int -> unit) =
      ignore
        (da#event#connect#expose ~callback:(fun _ ->
             f (self#get_drawable ())#size ;
             false ) )

    method private press f_left =
      ignore
        (da#event#connect#button_press ~callback:(fun c ->
             let x, y = (GdkEvent.Button.x c, GdkEvent.Button.y c) in
             let w = da#misc#window in
             ( match GdkEvent.Button.button c with
             | 1 ->
                 self#set_moving_left (x, y) ;
                 Gdk.Window.set_cursor w oc ;
                 f_left (self#get_coord (x, y))
             | _ -> () ) ;
             false ) )

    method private click f_left =
      ignore
        (da#event#connect#button_press ~callback:(fun c ->
             let x, y = (GdkEvent.Button.x c, GdkEvent.Button.y c) in
             ( match GdkEvent.Button.button c with
             | 1 -> f_left (self#get_coord (x, y))
             | _ -> () ) ;
             false ) )

    method private release f_left =
      ignore
        (da#event#connect#button_release ~callback:(fun c ->
             let x, y = (GdkEvent.Button.x c, GdkEvent.Button.y c) in
             let w = da#misc#window in
             ( match GdkEvent.Button.button c with
             | 1 ->
                 self#unset_moving_left () ;
                 Gdk.Window.set_cursor w mc ;
                 f_left (self#get_coord (x, y))
             | _ -> () ) ;
             false ) )

    method private drag (f_left : point -> point -> unit) =
      ignore
        (da#event#connect#motion_notify ~callback:(fun c ->
             let ((x, y) as p1) =
               (GdkEvent.Motion.x c, GdkEvent.Motion.y c)
             in
             ( match old_left with
             | None -> ()
             | Some ((a, b) as p2) ->
                 if sq_dist p1 p2 >= tolerance then (
                   f_left (self#get_coord (a, b)) (self#get_coord (x, y)) ;
                   self#set_moving_left (x, y) ) ) ;
             false ) )

    method private scrollwheel (fscroll : Gdk.Tags.scroll_direction -> unit)
        =
      ignore
        (da#event#connect#scroll ~callback:(fun c ->
             fscroll (GdkEvent.Scroll.direction c) ;
             false ) )

    method mouse_set ?expose:(exp = (fun _ -> () : int * int -> unit))
        ?press:(pl = fun _ -> ()) ?release:(rl = fun _ -> ())
        ?click:(cl = fun _ -> ()) ?drag:(dl = fun _ _ -> ())
        ?scrollwheel:(sw = fun _ -> ()) () =
      self#expose exp ;
      self#press pl ;
      self#release rl ;
      self#click cl ;
      self#drag dl ;
      self#scrollwheel sw
  end

module Gtkcanvas = struct
  type internal = GDraw.drawable

  let drawable = ref (None : GDraw.drawable option)

  let font = ref None

  (* text font loading *)
  let load_font () =
    let font_list =
      [ "-misc-fixed-medium-r-*-*-*-130-*-*-*-*-iso8859-1"
      ; "-schumacher-clean-medium-r-normal--6-140-75-75-c-60-iso646.1991-irv"
      ; "-bitstream-courier 10 \
         pitch-bold-r-normal--0-0-0-0-m-0-adobe-standard"
      ; "rk24"
      ; "10x20" ]
    in
    let fontname = ref "" in
    let exception FontFound of Gdk.font in
    try
      List.iter
        (fun fn ->
          match Gdk.Font.load fn with
          | f ->
              fontname := fn ;
              raise (FontFound f)
          | exception Gpointer.Null -> () )
        font_list
    with FontFound f -> font := Some f

  let set_drawable d =
    load_font () ;
    drawable := Some d

  let get_drawable () =
    try Option.get !drawable
    with Invalid_argument _ ->
      failwith "drawable should be set before canvas operations"

  let ending () = ()

  let width () =
    let d = get_drawable () in
    fst d#size |> float

  let height () =
    let d = get_drawable () in
    snd d#size |> float

  let normalize _ _ (x, y) = (x, height () -. y)

  type color = GDraw.color

  let rgb r g b : color = `RGB (256 * r, 256 * g, 256 * b)

  (* Draw text left, centre or right justified at point. (x,y) point is
     either top left, top middle or top right of text. *)
  let draw_text col position p text =
    let x, y = to_int_point p in
    match !font with
    | Some font -> (
        let drawable = get_drawable () in
        drawable#set_foreground col ;
        let w = Gdk.Font.string_width font text in
        let h = Gdk.Font.string_height font text in
        match position with
        | `Left -> drawable#string text ~font ~x ~y:(y + h)
        | `Center -> drawable#string text ~font ~x:(x - (w / 2)) ~y:(y + h)
        | `Right -> drawable#string text ~font ~x:(x - w) ~y:(y + h) )
    | None ->
        Format.eprintf "Picasso: no font found\n" ;
        ()

  let draw_line ~dashed col a b =
    let ax, ay = to_int_point a in
    let bx, by = to_int_point b in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    if dashed then drawable#set_line_attributes ~style:`ON_OFF_DASH ()
    else drawable#set_line_attributes ~style:`SOLID () ;
    drawable#line ~x:ax ~y:ay ~x:bx ~y:by

  let circle fill col (x, y) rad =
    let x, y = to_int_point (x -. (rad /. 2.), y -. (rad /. 2.)) in
    let rad = int_of_float rad in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    drawable#arc ~x ~y ~height:rad ~width:rad ~filled:fill ~angle:360. ()

  let fill_circle = circle true

  let draw_circle = circle false

  (* polygons *)
  let poly fill col vertices =
    let vertices = List.rev_map to_int_point vertices in
    let drawable = get_drawable () in
    drawable#set_foreground col ;
    drawable#polygon vertices ~filled:fill

  let fill_poly = poly true

  let draw_poly = poly false
end

module Draw = Drawer.Make (Gtkcanvas)

class canvas ~packing ~width ~height () =
  let hbox = GPack.hbox ~width ~height ~packing () in
  object (self)
    inherit clickable ~width ~height ~packing:hbox#add ()

    val mutable rend = None

    method get_render () =
      match rend with None -> failwith "render not set" | Some r -> !r

    method set_render (render : Rendering.t ref) =
      rend <- Some render ;
      self#mouse_set
        ~expose:(fun (a, b) ->
          render := !render |> Rendering.change_size (float a) (float b) ;
          self#repaint () )
        ~drag:(fun (a, b) (x, y) ->
          render := Rendering.translate (x -. a, y -. b) !render ;
          self#repaint () )
        ~scrollwheel:(fun direction ->
          (render :=
             match direction with
             | `DOWN -> Rendering.zoom !render
             | `UP -> Rendering.unzoom !render
             | _ -> !render ) ;
          self#repaint () )
        ()

    (* Repaint the widget. *)
    method repaint () =
      let drawable = self#get_drawable () in
      Gtkcanvas.set_drawable drawable ;
      Draw.clear () ;
      Draw.draw (self#get_render ()) ;
      ()
  end

(* constructor *)
let create_canvas ~packing ~height ~width =
  new canvas ~packing ~height ~width ()

(* building the main view *)
let build render =
  let open Rendering in
  GtkMain.Main.init () |> ignore ;
  let width = render.window.sx |> iof in
  let height = render.window.sy |> iof in
  let title = Option.value render.window.title ~default:"Picasso" in
  let window = GWindow.window ~width ~height ~title () in
  window#connect#destroy ~callback:GMain.Main.quit |> ignore ;
  window#event#add [`ALL_EVENTS] ;
  let hbox = GPack.hbox ~width ~packing:window#add () in
  let vbox = GPack.vbox ~width:(width - 40) ~packing:hbox#add () in
  let c = create_canvas ~packing:vbox#add ~height:(height - 30) ~width in
  let tb = toolbar ~width ~hpack:vbox#add ~vpack:hbox#add in
  let render = ref render in
  c#set_render render ;
  tb#set_render render ;
  tb#set_refresh (fun () -> c#repaint ()) ;
  window#show () ;
  GMain.Main.main ()
