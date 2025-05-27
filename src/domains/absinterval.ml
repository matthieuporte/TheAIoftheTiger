module Absint = struct
  include Interval
  include Interval_filter
end

module Absstring = Conststring
module Absarray = Squasharray.Make (Absint)

let safe_divisor (itv : Absint.t) =
  Absint.ne itv (Absint.of_int 0) |> Absint.truth = True

let safe_array_access (itv : Absint.t) (arr : 'a Absarray.t) =
  Absint.lt itv arr.size |> Absint.truth = True
  && Absint.ge itv (Absint.of_int 0) |> Absint.truth = True
