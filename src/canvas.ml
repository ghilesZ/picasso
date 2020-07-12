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
          let newrender = !render |> Rendering.change_size (float a) (float b) in
          render := newrender;
          self#repaint())
        ~drag:
        ((fun _ _ -> ()),
         (fun (a,b) (x,y) ->
           let difx = x -. a in
           let dify = y -. b in
           let newrender = Rendering.translate_scene (difx,dify) (!render) in
           render := newrender;
           self#repaint ();
        ))
        ~scrollwheel:(fun direction ->
          let newrender =
            (match direction with
             | `DOWN ->  Rendering.zoom_scene (!render)
             | `UP ->  Rendering.unzoom_scene (!render)
             | _ -> !render)
          in
          render := newrender;
          self#repaint ())
      ()

    (* Repaint the widget. *)
    method repaint () =
      let module Draw =
        Backend.Make(
            struct
              let internal = self#get_drawable()
              include Gtkcanvas
            end)
      in
      Draw.clear();
      (* Draw the elements *)
      Draw.draw (self#get_render());
      Draw.ending();
      ()

  end

(* constructor *)
let create_canvas ~packing ~height ~width =
  new canvas ~packing ~height ~width ()
