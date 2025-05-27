module Absint = Parity
module Absstring = Conststring
module Absarray = Squasharray.Make (Absint)

let safe_divisor (p : Absint.t) =
  (* if p is odd, it can be 0 *)
  p = Parity.Odd

let safe_array_access (_p : Absint.t) (_arr : 'a Absarray.t) = false
