open Interval

(* Ensure the interval is non-empty.  If the interval is invalid
   (upper bound less than lower bound), raise Domain.Bot_found to signal
   inconsistency. *)
let validate = function
  | Range (l, h) when h < l -> raise Domain.Bot_found
  | itv -> itv

(* comparisons *)

(* Interval refinement functions for relational constraints.
   Given two intervals, these functions compute a refined interval for the first operand
   that satisfies the given comparison against the second operand.
   - Each function returns a possibly narrowed interval, or raises Domain.Bot_found if the result is empty.
   - Intervals should be validated to ensure they are non-empty after refinement.

   hint: try first to implement the easy case when comparing two
   ranges and default to returning i1 in the other cases as shown
   here:
*)
let filter_eq i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (l2, h2) ->
      let l = max l1 l2 in
      let h = min h1 h2 in
      Range (l, h) |> validate
  | _ -> i1

let filter_ne i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (c, c2) when c = c2 ->
      if l1 = c then Range (l1 + 1, h1)
      else if h1 = c then Range (l1, h1 - 1)
      else i1
  | _ -> i1

let filter_gt i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (l2, h2) -> Range (max l1 l2 + 1, h1)
  | _ -> i1

let filter_ge i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (l2, h2) -> Range (max l1 l2, h1)
  | _ -> i1

let filter_lt i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (l2, h2) -> Range (l1, min h1 (h2 - 1))
  | _ -> i1

let filter_le i1 i2 =
  match (i1, i2) with
  | Range (l1, h1), Range (l2, h2) -> Range (l1, min h1 h2)
  | _ -> i1
