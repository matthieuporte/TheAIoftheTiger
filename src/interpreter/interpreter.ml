open Ast

(* utilities to convert binary operators to an actual function *)
let binop_to_fun (op : binop) : int -> int -> int =
  match op with Add -> ( + ) | Sub -> ( - ) | Mul -> ( * ) | Div -> ( / )

let relop_to_fun (op : relop) (v1 : Value.t) (v2 : Value.t) =
  let open Value in
  match (op, v1, v2) with
  | Eq, _, _ -> if v1 = v2 then 1 else 0
  | Ne, _, _ -> if v1 <> v2 then 1 else 0
  | Lt, Int r1, Int r2 -> if r1 < r2 then 1 else 0
  | Le, Int r1, Int r2 -> if r1 <= r2 then 1 else 0
  | Gt, Int r1, Int r2 -> if r1 > r2 then 1 else 0
  | Ge, Int r1, Int r2 -> if r1 >= r2 then 1 else 0
  | Lt, String r1, String r2 -> if r1 < r2 then 1 else 0
  | Le, String r1, String r2 -> if r1 <= r2 then 1 else 0
  | Gt, String r1, String r2 -> if r1 > r2 then 1 else 0
  | Ge, String r1, String r2 -> if r1 >= r2 then 1 else 0
  | _, _, _ -> failwith "invalid comparison"

(* Evaluates an expression in a given state.  Returns the result and
   possibly updated state. *)
let rec eval_expr (state : State.t) (e : expr) : Value.t * State.t =
  match e.payload with
  | Const i -> (Int i, state)
  (* evaluation from left to right *)
  | Funcall (name, args) ->
      let state, args =
        List.fold_left
          (fun (s, acc) a ->
            let r, s' = eval_expr s a in
            (s', acc @ [ r ]))
          (state, []) args
      in
      let func = State.find_fun name state in
      (func args, state)
  | String s -> (String s, state)
  | Let (chunks, body) ->
     let new_state = eval_chunks state chunks in 
     (* this is to put back the precedent state after
        the end of the let in end *)
     let v, _ = eval_seq new_state body in
     v, state
  | Lval lv -> read_lvalue state lv
  | Binop (e1, op, e2) -> eval_binop state e1 op e2
  | Seq body -> eval_seq state body
  | Assign (lv, e) ->
     let v,s = eval_expr state e in
     v, write_lvalue s lv v
    
     (* complete the function and keep this wildcard card until it becomes redundant *)
  | _ -> Format.asprintf "(%s)" __FUNCTION__ |> Utils.niy

(* Writes a value to the location referred to by the given lvalue,
   returning the updated state.  This may involve evaluating
   subexpressions with side effects (e.g. array indices), and in the
   case of nested lvalues (such as array elements), recursively
   updates the structure.

   hint: Use Value.array_set
*)
and write_lvalue (state : State.t) (lv : lvalue) (value : Value.t) : State.t =
  match lv.payload with
  | Var id -> State.update_value id value state
     (* complete the function and keep this wildcard card until it becomes redundant *)
     | _ -> Format.asprintf "%a (%s)" Ast.print_lvalue lv __FUNCTION__ |> Utils.niy

(* Resolves an lvalue to the value it refers to, returning the value
   and the updated state.  This may involve evaluating subexpressions
   with side effects, such as index expressions.
   hint: Use Value.array_get
*)
and read_lvalue (state : State.t) (lv : lvalue) : Value.t * State.t =
  match lv.payload with
  | Var id -> (State.find_value id state, state)
     (* complete the function and keep this wildcard card until it becomes redundant *)
     | _ -> Format.asprintf "(%s)" __FUNCTION__ |> Utils.niy

and eval_seq (state: State.t) (seq_expr: expr list) : Value.t * State.t =
  match seq_expr with
   | [] -> failwith "eval_seq: empty list"
   | [ e ] -> eval_expr state e
   | e :: tail ->
      (* let _ = Printf.printf "e\n" in *)
      let _, new_state = eval_expr state e in
      eval_seq new_state tail
  (* List.fold_left (fun (_, s) e  -> eval_expr s e) (Void, state) seq_expr  *)

and eval_chunks (state : State.t) (chunks : chunk list) : State.t =
  List.fold_left eval_chunk state chunks

and eval_chunk (state : State.t) (c : chunk) : State.t =
  match c.payload with
  | Exp e ->
      (* we evaluate the expression so that it's side effects are taken
         into account, but the result is dicarded *)
      let _, state = eval_expr state e in
      state
  (* | Typedec name t -> failwith "Not implemented" *)
  | Vardec (id, t, e) ->
     (* let _ = Printf.printf "\nHello\n" in *)
     let v, s = eval_expr state e in
     let new_state = State.add_value id v s in
     new_state

     (* let new_state = Env.add id value state.values in *)
     (* new_state  *)

  (* complete the function and keep this wildcard card until it becomes redundant *)
  | _ -> Format.asprintf "%a (%s)" Ast.print_chunk c __FUNCTION__ |> Utils.niy

and eval_binop (state : State.t) (e1 : expr) (op : binop) (e2 : expr) =
     let v1, s1 = eval_expr state e1 in
     let v2, s2 = eval_expr s1 e2 in
     let int1 = Value.cast_int e1.loc v1
     and int2 = Value.cast_int e2.loc v2 in
     (match op with
     | Add -> (Int (int1 + int2), s2)
     | Sub -> (Int (int1 - int2), s2)
     | Mul -> (Int (int1 * int2), s2)
      (* Maybe div by 0 problems *)
     | Div -> (Int (int1 / int2), s2)
     )

open Value

let print_int out = function
  | [ Int x ] ->
      Format.fprintf out "%i%!" x;
      Void
  | _ -> failwith "print_int: type error"

let print out = function
  | [ String x ] ->
      Format.fprintf out "%s%!" x;
      Void
  | _ -> failwith "print: type error"

let concat = function
     (* Concatenates two strings *)
  | [ String a; String b ] ->
    String (a ^ b)
  | _ -> failwith "concat: type error"

let range = function
   (* returns a value between low and high *)
  | [ Int low; Int high ] ->
     Random.self_init();
     (* +1 because upper value is not included *)
     let r = Random.int (high - low + 1) in
     Int (r + low)
  | _ -> failwith "range: type error"

(* Evaluates a Tiger program with an optional output formatter.
   Initializes the runtime environment with built-in functions and
   evaluates the program from the initial state. *)
let eval_program ?oc (p : program) : State.t =
  let out = match oc with None -> Format.std_formatter | Some o -> o in
  let runtime =
    [
      ("print_int", print_int out);
      ("print", print out);
      ("concat", concat);
      ("range", range);
    ]
  in
  let start = State.init runtime in
  eval_chunks start p
