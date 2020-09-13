(** the type of drawable abstractions  *)
type t = Apronext.Apol.t list

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

(** {2 Operations }*)

(** merges two drawable into one drawable*)
val join : t -> t -> t

(** Computes the bounds of a variable within the elements of a drawable*)
val bounds : var -> var -> t -> Apron.Interval.t * Apron.Interval.t
