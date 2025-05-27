type itv = int * int

let print_itv fmt (low, sup) =
  if low = sup then Format.fprintf fmt "%i" sup
  else Format.fprintf fmt "[%i;%i]" low sup

type t =
  | Range of itv
  | Minf of int  (** [Minf(up)] is the set of values [x | x <= up] *)
  | Inf of int  (** [Inf(low)] is the set of values [x | x >= low] *)
  | Top

let print fmt itv =
  match itv with
  | Top -> Format.fprintf fmt "]-oo;+oo["
  | Minf u -> Format.fprintf fmt "]-oo;%i]" u
  | Inf l -> Format.fprintf fmt "[%i;+oo[" l
  | Range itv -> print_itv fmt itv

(* join *)
(* Step 4 *)
let join _ _ = Top
let widen _ _ = Top

let subset a b =
  match (a, b) with
  | _, Top -> true
  | Range (l1, h1), Range (l2, h2) -> l2 <= l1 && h1 <= h2
  | Range (_, u), Minf u' | Minf u, Minf u' -> u <= u'
  | Range (l, _), Inf l' | Inf l, Inf l' -> l >= l'
  | _ -> false

(* arith*)

(** negation of an interval *)
let neg (i : t) : t =
  match i with
  | Top -> Top
  | Inf x -> Minf (-x)
  | Minf x -> Inf (-x)
  | Range (l, u) -> Range (-u, -l)



(* Step 4 *)
let add _i1 _i2 = Top
let sub _i1 _i2 = Top
let mul _i1 _i2 = Top
let div _i1 _i2 = Top

(* truth handling *)
let false_ = Range (0, 0)
let true_ = Range (1, 1)
let maybe_ = Range (0, 1)

let truth = function
  | Range (0, 0) -> Domain.False
  | Range (1, 1) -> Domain.True
  | _ -> Domain.Unknown

(* boolean logic *)
(* Tiger Boolean operators normalize their result to 0 or 1 *)
let logical_and a b =
  if subset a false_ || subset b false_ then false_ else maybe_

let logical_or a b =
  if subset a false_ && subset b false_ then false_ else maybe_

(* comparisons *)
(* Step 4 *)
let eq _ _ = maybe_
let ne _ _ = maybe_
let gt _ _ = maybe_
let ge _ _ = maybe_
let lt _ _ = maybe_
let le _ _ = maybe_

(* constructors *)
let of_int x = Range (x, x)
let range l h = join l h
