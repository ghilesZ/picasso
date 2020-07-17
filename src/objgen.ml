(* This module handles the obj file format generation *)
type point3d = float * float * float
type triangle = point3d * point3d * point3d
type cube = point3d * point3d * point3d * point3d
            * point3d * point3d * point3d * point3d
type id = int
type face = id * id * id

(* Vertex printing *)
let print_vertex oc ((x,y,z):point3d) =
  output_string oc
    ("v "^(string_of_float x)^" "^(string_of_float y)^" "^(string_of_float z)^"\n")

(* Face printing *)
let print_face oc ((id1,id2,id3):face)  =
  output_string oc
    ("f "^(string_of_int id1)^" "^(string_of_int id2)^" "^(string_of_int id3)^"\n")

(* vertex id generator *)
let gen_vert_id : unit -> id =
  let v_id = ref 0 in
  fun () ->
  incr v_id;
  !v_id

(* prints a vertex and return the associated ID *)
let print_vertex : out_channel -> point3d -> id =
  (* cache to avoid the multiple printing of the same vertice *)
  let h_v = Hashtbl.create 10000 in
  fun oc v ->
  try Hashtbl.find h_v v
  with Not_found -> (
    let id = gen_vert_id() in
    Hashtbl.add h_v v id;
    print_vertex oc v;
    id
  )

(* This function is expanded for efficiency purposes *)
(* It prints 8 points and then the faces between those points *)
let handle_c fmt (p0,p1,p2,p3,p4,p5,p6,p7) =
  let id0 = print_vertex fmt p0 in
  let id1 = print_vertex fmt p1 in
  let id2 = print_vertex fmt p2 in
  let id3 = print_vertex fmt p3 in
  let id4 = print_vertex fmt p4 in
  let id5 = print_vertex fmt p5 in
  let id6 = print_vertex fmt p6 in
  let id7 = print_vertex fmt p7 in
  let t1,t2 = (id0,id1,id2),(id0,id2,id3) in
  let t3,t4 = (id4,id5,id6),(id4,id6,id7) in
  let t5,t6 = (id0,id1,id5),(id0,id5,id4) in
  let t7,t8 = (id2,id3,id7),(id2,id7,id6) in
  let t9,t10 = (id0,id3,id7),(id0,id7,id4) in
  let t11,t12 = (id1,id2,id6),(id1,id6,id5) in
  print_face fmt t1;
  print_face fmt t2;
  print_face fmt t3;
  print_face fmt t4;
  print_face fmt t5;
  print_face fmt t6;
  print_face fmt t7;
  print_face fmt t8;
  print_face fmt t9;
  print_face fmt t10;
  print_face fmt t11;
  print_face fmt t12

(* Main function of the module. Prints the cube list *)
let print_to_file fmt (cubes: cube list) =
  List.iter (handle_c fmt) cubes
