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
let join i1 i2 =
  match (i1, i2) with
  | Range (low1, high1), Range (low2, high2) ->
      let low = if low1 < low2 then low1 else low2
      and high = if high1 > high2 then high1 else high2 in
      Range (low, high)
  | Range (low1, high1), Minf up | Minf up, Range (low1, high1) ->
      let high = if high1 > up then high1 else up in
      Minf high
  | Range (low1, high1), Inf low2 | Inf low2, Range (low1, high1) ->
      let low = if low1 < low2 then low1 else low2 in
      Inf low
  | _ -> Top

let widen i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (l2, h2) -> (
      let low = if l1 = l2 then 1 else 0 and high = if h1 = h2 then 1 else 0 in
      match (low, high) with
      | 1, 1 -> Range (l1, h1)
      | 1, 0 -> Inf l1
      | 0, 1 -> Minf h1
      | _, _ -> Top)
  | Range (l1, h1), Inf low | Inf low, Range (l1, h1) ->
      if l1 = low then Inf low else Top
  | Range (l1, h1), Minf up | Minf up, Range (l1, h1) ->
      if h1 = up then Minf up else Top
  | _, _ -> Top

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
let add (_i1 : t) (_i2 : t) : t =
  match _i1, _i2 with
  | Top, _ -> Top
  | _, Top -> Top
  | Range (low1, high1), Range (low2, high2) -> Range (low1 + low2, high1 + high2)
  | Range (_, high), Minf i -> Minf (i + high)
  | Range (low, _), Inf i -> Inf (i + low)
  | Inf i, Range (low, _) -> Inf (i + low)
  | Inf _, _ -> Top
  | Minf i, Range (_, high) -> Minf (i + high)
  | Minf _, _ -> Top

(* Can be imporved with more precision on Inf and Minf *)
let sub (_i1 : t) (_i2 : t) : t =
  match _i1, _i2 with
  | Top, _ -> Top
  | _, Top -> Top
  | Range (low1, high1), Range (low2, high2) -> Range (low1 - high2, high1 - low2)
  | Range (low, _), Minf i -> Inf (low - i)
  | Range (_, high), Inf i -> Minf (high - i)
  | Inf i, Range (_, high) -> Inf (high - i)
  | Inf _, _ -> Top
  | Minf i, Range (low, _) -> Inf (low - i)
  | Minf _, _ -> Top
  

let mul_range (l1 : int) (h1 : int) (l2 : int) (h2 : int) : int * int =
  let m1 = l1 * l2 in
  let m2 = l1 * h2 in
  let m3 = h1 * l2 in
  let m4 = h1 * h2 in
  min (min m1 m2) (min m3 m4), max (max m1 m2) (max m3 m4)

let div_range (l1 : int) (h1 : int) (l2 : int) (h2 : int) : int * int =
  let m1 = l1 / l2 in
  let m2 = l1 / h2 in
  let m3 = h1 / l2 in
  let m4 = h1 / h2 in
  min (min m1 m2) (min m3 m4), max (max m1 m2) (max m3 m4)

(* Need to be had logic handling Inf and Minf *)
let mul (_i1 : t) (_i2 : t) : t =
  match _i1, _i2 with
  | Range (low1, high1), Range (low2, high2) -> Range (mul_range low1 high1 low2 high2)
  | _ -> Top

let div (_i1 : t) (_i2 : t) : t =
  match _i1, _i2 with
  (* Division by 0 *)
  | _, Range (low, high) when low <= 0 && high >= 0 -> Top
  | _, Inf i when i <= 0 -> Top
  | _, Minf i when i >= 0 -> Top
  (* Left is range *)
  | Range (low1, high1), Range (low2, high2) -> Range (div_range low1 high1 low2 high2)
  | Range (low, high), Inf i when low > 0 && high > 0 -> Range (0, high / i)
  | Range (low, high), Inf i when low < 0 && high < 0 -> Range (low / i, 0)
  | Range (low, high), Minf i when low > 0 && high > 0 -> Range (high/i, 0)
  | Range (low, high), Minf i when low < 0 && high < 0 -> Range (0, low / i)
  |_ -> Top

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
let reverse b =
  let t = truth b in
  match t with Domain.True -> false_ | Domain.False -> true_ | _ -> maybe_

(* Step 4 *)
let eq i1 i2 =
  match (i1, i2) with
  | Range (low1, high1), Range (low2, high2) ->
      if low1 = low2 && low1 = high1 && low2 = high2 then true_
      else if low1 > high2 || low2 > high1 then false_
      else maybe_
  | Inf low, Minf up | Minf up, Inf low -> if low > up then false_ else maybe_
  | _, _ -> maybe_

let ne i1 i2 = reverse (eq i1 i2)

(* i1 > i2 *)
let gt i1 i2 =
  match i1 with
  | Range (low1, high1) -> (
      match i2 with
      | Range (low2, high2) ->
          if low1 > high2 then true_
          else if low2 >= high1 then false_
          else maybe_
      | Minf up -> if low1 > up then true_ else maybe_
      | Inf low -> if high1 < low then false_ else maybe_
      | _ -> maybe_)
  | Minf up -> (
      match i2 with
      | Range (low2, high2) -> if up <= low2 then false_ else maybe_
      | Inf low -> if up <= low then false_ else maybe_
      | _ -> maybe_)
  | Inf low -> (
      match i2 with
      | Range (low2, high2) -> if low > high2 then true_ else maybe_
      | Minf up -> if up < low then true_ else maybe_
      | _ -> maybe_)
  | _ -> maybe_

let ge i1 i2 =
  match i1 with
  | Range (low1, high1) -> (
      match i2 with
      | Range (low2, high2) ->
          if low1 >= high2 then true_
          else if low2 > high1 then false_
          else maybe_
      | Minf up -> if low1 >= up then true_ else maybe_
      | Inf low -> if high1 <= low then false_ else maybe_
      | _ -> maybe_)
  | Minf up -> (
      match i2 with
      | Range (low2, high2) -> if up < low2 then false_ else maybe_
      | Inf low -> if up < low then false_ else maybe_
      | _ -> maybe_)
  | Inf low -> (
      match i2 with
      | Range (low2, high2) -> if low >= high2 then true_ else maybe_
      | Minf up -> if up <= low then true_ else maybe_
      | _ -> maybe_)
  | _ -> maybe_

let lt i1 i2 = gt i2 i1
let le i1 i2 = ge i2 i1

(* constructors *)
let of_int x = Range (x, x)
let range l h = join l h
