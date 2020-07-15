open Geometry
open Apronext

type abs =
  | Box of Abox.t
  | Oct of Aoct.t
  | Pol of Apol.t

type t =
  | Polygon     of point list
  | Generators  of Generatorext.t list
  | Constraints of Linconsext.t list
  | Abstract    of abs
