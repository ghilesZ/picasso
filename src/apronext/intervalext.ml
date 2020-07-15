open Apron

include Interval

let join a b = {
    inf = if Scalar.cmp a.inf b.inf < 0 then a.inf else b.inf;
    sup = if Scalar.cmp a.sup b.sup > 0 then a.sup else b.sup;
  }

let to_float a = (Apron_utils.scalar_to_float a.inf),(Apron_utils.scalar_to_float a.sup)
