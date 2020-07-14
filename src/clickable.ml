(* This module is a drawing area wrapper that handles mouse events and expose event *)

type mouse_fun = Geometry.point -> unit
type mouse_move_fun = Geometry.point -> Geometry.point -> unit

class clickable ~packing ~width ~height () =
  (* Create the containing vbox. *)
  let vbox = GPack.vbox ~width ~height ~packing () in
  let da = GMisc.drawing_area ~width ~height ~packing:vbox#add () in
  let drawable = lazy (new GDraw.drawable da#misc#window) in

  let mc = Gdk.Cursor.create `ARROW in
  let cc = Gdk.Cursor.create `CROSS in
  let oc = Gdk.Cursor.create `FLEUR in

  object(self)
        initializer
          (ignore (da#event#add ([`BUTTON_PRESS;`BUTTON_RELEASE;`POINTER_MOTION;`SCROLL]);
                   ()))

    val mutable moving_right = false
    val mutable moving_left = false
    val mutable old_right = None
    val mutable old_left = None

    method private set_moving_left (x,y) =
      old_left <- Some (x,y);
      moving_left <- true

    method private set_moving_right (x,y) =
      old_right <- Some (x,y);
      moving_right <- true

    method private unset_moving_left () =
      old_left <- None;
      moving_left <- false

    method private unset_moving_right () =
      old_right <- None;
      moving_right <- false

    method get_drawable () =
      try Lazy.force drawable
      with Gpointer.Null -> failwith "drawable null"

    method private get_coord (x,y) =
      (* in gtk y axis is inversed *)
      let _,b = (self#get_drawable())#size in
      x,((float b) -. y)

    method private expose (f : int*int -> unit)=
      ignore(da#event#connect#expose
        ~callback:(fun _ -> f (self#get_drawable())#size; false))

    method private press f_left f_right =
      ignore (da#event#connect#button_press ~callback:(fun c ->
          let x,y = (GdkEvent.Button.x c),(GdkEvent.Button.y c) in
          let w = da#misc#window in
          (match GdkEvent.Button.button c with
           | 1 ->
              self#set_moving_left(x,y);
              Gdk.Window.set_cursor w cc;
              f_left (self#get_coord (x,y))
           | 3 ->
              self#set_moving_right(x,y);
              Gdk.Window.set_cursor w oc;
              f_right (self#get_coord (x,y))
           | _ -> ());
          false
         ))

    method private click f_left f_right =
      ignore (da#event#connect#button_press ~callback:(fun c ->
          let x,y = (GdkEvent.Button.x c),(GdkEvent.Button.y c) in
          (match GdkEvent.Button.button c with
           | 1 -> f_left (self#get_coord (x,y))
           | 3 -> f_right (self#get_coord (x,y))
           | _ -> ());
          false
         ))

    method private release f_left f_right =
      ignore (da#event#connect#button_release ~callback:(fun c ->
           let x,y = (GdkEvent.Button.x c),(GdkEvent.Button.y c) in
           let w = da#misc#window in
           (match GdkEvent.Button.button c with
            | 1 ->
               self#unset_moving_left ();
               Gdk.Window.set_cursor w mc;
               f_left (self#get_coord (x,y))
            | 3 ->
               self#unset_moving_right ();
               Gdk.Window.set_cursor w mc;
               f_right (self#get_coord (x,y))
            | _ -> ()
           );
           false
         ))

    method private drag (f_left : float * float -> float * float -> unit)
                (f_right : float * float -> float * float -> unit) =
      ignore (da#event#connect#motion_notify ~callback:(fun c ->
          let x,y = GdkEvent.Motion.x c,GdkEvent.Motion.y c in
          (match old_left with
           | None -> ()
           | Some(a,b) ->
              f_left (self#get_coord (a,b)) (self#get_coord (x,y));
              self#set_moving_left (x,y);
          );
          (match old_right with
           | None -> ()
           | Some(a,b) ->
              f_right (self#get_coord (a,b)) (self#get_coord (x,y));
              self#set_moving_right (x,y);
          );
          false
         ))

    method private scrollwheel (fscroll:Gdk.Tags.scroll_direction ->unit) =
      ignore (da#event#connect#scroll ~callback:(fun c ->
           fscroll (GdkEvent.Scroll.direction c);
           false
         ))

    method mouse_set
             ?expose:(exp=((fun _ ->()):int*int->unit))
             ?press:(pl,pr=((fun _ ->()):mouse_fun),((fun _ ->()):mouse_fun))
             ?release:(rl,rr=((fun _ ->()):mouse_fun),((fun _ ->()):mouse_fun))
             ?click:(cl,cr=((fun _ ->()):mouse_fun),((fun _ ->()):mouse_fun))
             ?drag:(dl,dr=((fun _ _ ->()):mouse_move_fun),((fun _ _ ->()):mouse_move_fun))
             ?scrollwheel:(sw=((fun _ ->()):Gdk.Tags.scroll_direction -> unit))
             ()
      =
      self#expose exp;
      self#press pl pr;
      self#release rl rr;
      self#click cl cr;
      self#drag dl dr;
      self#scrollwheel sw
end
