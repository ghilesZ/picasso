open Tools

module type T = sig
  type internal
  type color
  val internal : internal (* backend specific settings *)
  val init : internal -> unit
  val ending : unit -> unit

  val width : unit -> int
  val height : unit -> int

  val normalize : range -> range -> point -> point

  val rgb : int -> int -> int -> color

  val draw_text : color -> [<`Center|`Left |`Right ] -> point -> string -> unit

  val draw_line : color -> point -> point -> unit

  val draw_circle : color -> point -> float -> unit
  val fill_circle : color -> point -> float -> unit

  val draw_poly : color -> point list -> unit
  val fill_poly : color -> point list -> unit
end
