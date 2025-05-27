open Domain

module Make (D : D) = struct
  open D

  type t =
    | Void
    | Int of Absint.t
    | String of Absstring.t
    | Array of t Absarray.t
    | Unreachable (* special value to denote unreachable code *)

  let rec print fmt r =
    match r with
    | Void -> Format.fprintf fmt "void"
    | Int i -> Format.fprintf fmt "%a" Absint.print i
    | String s -> Format.fprintf fmt "%a" Absstring.print s
    | Array a -> Format.fprintf fmt "%a" (Absarray.print print) a
    | Unreachable -> Format.fprintf fmt "unreachable"

  let fail_incompatible kind r1 r2 =
    Format.asprintf "heterogenous %s on %a %a" kind print r1 print r2
    |> failwith

  let rec binary_op kind int_op string_op array_op record_op r1 r2 =
    match (r1, r2) with
    | Int d1, Int d2 -> Int (int_op d1 d2)
    | String s1, String s2 -> String (string_op s1 s2)
    | Array a1, Array a2 ->
        Array
          (array_op (binary_op kind int_op string_op array_op record_op) a1 a2)
    | Void, Void -> Void
    | Unreachable, x | x, Unreachable -> x
    | _, _ -> fail_incompatible kind r1 r2

  let rec join r1 r2 =
    binary_op "join" Absint.join Absstring.join Absarray.join join r1 r2

  let rec widen r1 r2 =
    binary_op "widen" Absint.widen Absstring.widen Absarray.widen widen r1 r2

  let rec subset r1 r2 =
    match (r1, r2) with
    | Void, Void -> true
    | Int d1, Int d2 -> Absint.subset d1 d2
    | Array a1, Array a2 -> Absarray.subset subset a1 a2
    | String s1, String s2 -> Absstring.subset s1 s2
    | Unreachable, _ -> true
    | _, Unreachable -> false
    | _, _ ->
        Format.asprintf "heterogenous subset on %a %a" print r1 print r2
        |> failwith

  let cast_int loc = function
    | Int i -> i
    | r ->
        Format.asprintf "%a\n\nWas expecting int but got %a\n%!" Errors.from_loc
          loc print r
        |> Errors.fail

  let cast_array loc = function
    | Array a -> a
    | r ->
        Format.asprintf "%a a\n\nWas expecting array but got %a\n%!"
          Errors.from_loc loc print r
        |> Errors.fail

  let cast_bool loc = function
    | Int i -> i |> Absint.truth
    | r ->
        Format.asprintf "%a\n\nWas expecting int (bool) but got %a\n%!"
          Errors.from_loc loc print r
        |> Errors.fail

  (* wrappers arround array manipulation *)
  let array_make (size : Absint.t) (v : t) : t = Array (Absarray.make size v)

  let array_set (array : t Absarray.t) (i : Absint.t) (v : t) : t =
    Array (Absarray.set join array i v)

  let array_get (array : t Absarray.t) (i : Absint.t) : t = Absarray.get array i
end
