class canvas ?packing ?width ?height () =

  (* Create the drawing area. *)
  let hbox = GPack.hbox ?width ?height ?packing () in
  object (self)
    inherit Clickable.clickable ~width:500
                                ~height:500
                                ~packing:hbox#add
                                ()
    val mutable rend = None

    method get_render () =
      match rend with
      | None -> failwith "render not set"
      | Some r -> !r

    method set_render (render:Rendering.t ref) =
      rend <- Some render;
      self#mouse_set
        ~expose:
        (fun (a,b) ->
          render := !render |> Rendering.change_size (float a) (float b);
          self#repaint())
        ~drag:
        ((fun _ _ -> ()),
         (fun (a,b) (x,y) ->
           render := Rendering.translate_scene (x -. a, y -. b) (!render);
           self#repaint ();
        ))
        ~scrollwheel:(fun direction ->
          render :=
            (match direction with
             | `DOWN ->  Rendering.zoom_scene (!render)
             | `UP ->  Rendering.unzoom_scene (!render)
             | _ -> !render);
          self#repaint ())
      ()

    (* Repaint the widget. *)
    method repaint () =
      let module Draw =
        Drawer.Make(
            struct
              let internal = self#get_drawable()
              include Gtkcanvas
            end)
      in
      Draw.clear();
      (* Draw the elements *)
      Draw.draw (self#get_render());
      ()

  end

(* constructor *)
let create_canvas ~packing ~height ~width =
  new canvas ~packing ~height ~width ()
