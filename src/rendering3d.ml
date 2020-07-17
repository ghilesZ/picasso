open Apronext

type t = {
    (* content *)
    elems3     :  Apol.t list;
    (* projection variables *)
    abciss3    : string option;
    ordinate3  : string option;
    height3    : string option;
    (* elems projected on the projection variables.*)
    bounded3   : vertex3d list;
  }

and vertex3d = point3d list
and point3d = float * float * float

let create ?abciss3 ?ordinate3 ?height3 () = {
    elems3 = []; abciss3;  ordinate3; height3; bounded3 = [];
  }

let add r (x: Drawable.t) =
  {r with elems3 = (Drawable.to_poly x)::r.elems3}

(* changes the projection variables. if those are different from the
   previous ones we convert bounded elements to 3d points list *)
let set_proj_vars r v1 v2 v3 =
  match r.abciss3, r.ordinate3, r.height3 with
  | Some x, Some y, Some z when x=v1 && y = v2 && z=v3 -> r
  | _ ->
     let bounded3 =
       List.fold_left (fun acc pol ->
           let p3d = Apol.proj3D_s pol v1 v2 v3 in
           if Apol.is_bounded p3d then
             let gen' = Apol.to_generator_list p3d in
             let pts = List.rev_map (fun g -> Generatorext.to_vertices3D_s g v1 v2 v3) gen' in
             pts::acc
           else failwith "unbounded element for obj generation"
         ) [] r.elems3
     in
     {r with abciss3 = Some v1; ordinate3 = Some v2; height3=Some v3;
             bounded3}
