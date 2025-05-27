module Absint = Constint
module Absstring = Conststring
module Absarray = Squasharray.Make (Absint)

let safe_divisor (p : Absint.t) = match p with Known i -> i <> 0 | _ -> false

let safe_array_access (p : Absint.t) (arr : 'a Absarray.t) =
  match (arr, p) with
  | { size = Known size; _ }, Known i -> i < size && i >= 0
  | _ -> false
