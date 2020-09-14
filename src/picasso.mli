(** Picasso is an Abstract element drawing library. It handles most of
   the boilerplate you usually write to draw abstract elements and
   allows you to view those.  *)

(** module of drawable abstractions *)
module Drawable : sig

  (** {1 Types} *)

  (** the type of drawable abstractions  *)
  type t

  type var = string
  type point = float list
  type range = float * float

  (** {1 Constructors} *)

  (** drawable from an element of the Boxes abstract domain *)
  val of_box : Apronext.Abox.t -> t

  (** drawable from an element of the Octagon abstract domain *)
  val of_oct : Apronext.Aoct.t -> t

  (** drawable from an element of the Polyhedra abstract domain *)
  val of_pol : Apronext.Apol.t -> t

  (** Builds the drawable space corresponding to the polyhedra defined
   by the list of generators. *)
  val of_gens : Apron.Generator1.t list -> t

  (** Builds the drawable space corresponding to the polyhedra defined
   by the conjunction of a list of constraints. *)
  val of_lcons : Apron.Lincons1.t list -> t

  (** Same as of_gens, but build a convex hull from a list of a
   variables defining an environment and a list of points. Raises an
   error if one points or more do not have as many dimension as the
   number of variables*)
  val of_hull : string list -> point list -> t

  (** builds a drawable hypercube from a list of variable and a list of
   ranges. Raises an error if the range list and the variable list do
   not have the same length *)
  val of_ranges : string list -> range list -> t

  (** {1 Operations }*)

  (** merges two drawable into one drawable*)
  val join : t -> t -> t

  (** Computes the bounds of a variable within the elements of a drawable*)
  val bounds : var -> var -> t -> Apron.Interval.t * Apron.Interval.t
end

(** module for 2d drawing of abstract elements, handles the 'camera'
   settings, 2D projection, and some graphical options *)
module Rendering : sig
  (** type of 2D scenes *)
  type t

  (** colors are defined in RGB format *)
  type color = int * int * int

  (** initalizes an empty 2d scenes. *)
  val create: ?title:string -> ?padding:float -> ?grid:bool -> ?axis:bool ->
              abciss:string -> ordinate:string -> float -> float -> t

  (** registers an abstract element, associated to a color, into a scene *)
  val add : ?autofit:bool -> t -> color * Drawable.t -> t

  (** camera settings *)
  val translate : float * float -> t -> t
  val scale : t -> float -> t
end

(** module for 3D model generation of abstract elements *)
module Rendering3d : sig
   (** type of 3D scenes *)
  type t

  (** initalizes an empty 3D scenes. *)
  val create : abciss:string -> ordinate:string -> height:string -> unit -> t

  (** registers an abstract element into a scene *)
  val add : t -> Drawable.t -> t
end

(** Displays a Rendering.t within a scrollable, zoomable gtk canvas *)
val in_gtk_canvas : Rendering.t -> unit

(** Outputs a tex file with a tikz figure corresponding to a
   Rendering.t. If the tikz_only option is set to false (default is
   true), it outputs the full tex document and not only the tikz
   figure *)
val to_latex : ?tikz_only:bool -> Rendering.t -> string -> unit

(** Builds an obj file rorresponding to a Rendering3D context *)
val to_obj : Rendering3d.t -> string -> unit
