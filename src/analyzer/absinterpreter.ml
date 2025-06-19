open Domain
open Ast

module Make (D : D) = struct
  open D
  module Value = Absvalue.Make (D)
  module State = Absstate.Make (D)
  module Guard = Guard.Make (D)

  let binop_to_fun (op : binop) =
    match op with
    | Add -> Absint.add
    | Sub -> Absint.sub
    | Mul -> Absint.mul
    | Div -> Absint.div

  let boolop_to_fun (op : boolop) =
    match op with And -> Absint.logical_and | Or -> Absint.logical_or

  let num_of_truth = function
    | True -> Absint.of_int 1
    | False -> Absint.of_int 0
    | Unknown -> Absint.join (Absint.of_int 1) (Absint.of_int 0)

  let relop_to_fun (op : relop) (r1 : Value.t) (r2 : Value.t) =
    match (op, r1, r2) with
    | Eq, Int r1, Int r2 -> Absint.eq r1 r2
    | Eq, String s1, String s2 -> Absstring.eq s1 s2 |> num_of_truth
    | Ne, Int r1, Int r2 -> Absint.ne r1 r2
    | Ne, _, _ -> num_of_truth Unknown
    | Lt, Int r1, Int r2 -> Absint.lt r1 r2
    | Le, Int r1, Int r2 -> Absint.le r1 r2
    | Gt, Int r1, Int r2 -> Absint.gt r1 r2
    | Ge, Int r1, Int r2 -> Absint.ge r1 r2
    | _, _, _ ->
        Format.asprintf "invalid comparison : %a %s %a" Value.print r1
          (relop_to_string op) Value.print r2
        |> failwith

  (* Computes a fixed point of function [f] over abstract state [x] with widening.
       Continues applying [f] until the state no longer grows (i.e., reaches stability).
     Hint: use me at step 6 *)
  let rec fix f state =
    let fx = f state in
    if State.subset fx state then fx else fix f (State.widen state fx)

  (* [filter] attempts to refine [state] based on the boolean [cond].
     - If [cond] is definitely true or false it returns the original [state] or [Bot] depending on [r].
     - If [cond] is unknown, it tries [Guard.filter_annot]; on failure, it falls back to [state].
     Hint: use me at step 5 *)
  let filter (state : State.t) (cond : (State.t, Value.t) Annotast.expr)
      (r : bool) : State.t =
    match cond.e_value |> Value.cast_bool cond.e_loc with
    | True -> if r then state else Bot
    | False -> if r then Bot else state
    | Unknown -> (
        try Guard.filter_annot state cond r with Guard.Unfiltrable -> state)

  (* Entry point for analyzing an expression.  If the abstract state
     is empty (i.e., unreachable code), annotate the expression
     recursively with an unreachable value.  Otherwise, Analyze an
     expression node and return its annotated version.  The analysis
     may modify the abstract state, which is propagated through
     recursively.
  *)
  let rec analyze_expr (state : State.t) (e : expr) :
      (State.t, Value.t) Annotast.expr =
    (* local function that operates on non-empty states *)
    let analyze_non_bot_expr (state : State.t) (e : expr) :
        (State.t, Value.t) Annotast.expr =
      let open Annotast in
      match e.payload with
      | Const i ->
          let absi = Value.Int (Absint.of_int i) in
          build_expr e.loc (AConst i) state absi
      | String s ->
          let abss = Value.String (Absstring.of_string s) in
          build_expr e.loc (AString s) state abss
      | Binop (l, op, r) -> analyze_binop e.loc state l r op
      | Boolop (l, op, r) -> analyze_boolop e.loc state l r op
      | Relop (l, op, r) -> analyze_relop e.loc state l r op
      | Funcall (name, args) -> analyze_funcall state e.loc name args
      | Assign (left, right) -> analyze_assign e.loc state left right
      | ArrayInit (id, size, content) ->
          analyze_array_init e.loc state id size content
      | Lval lv ->
          let lv' = read_lvalue state lv in
          build_expr e.loc (ALval lv') lv'.l_state lv'.l_value
      | Seq l ->
          let l_annot, s, res = analyze_seq state l in
          build_expr e.loc (ASeq l_annot) s res
      | Let (chunks, body) -> analyze_let e.loc state chunks body
      | IfThenElse (cond, true_branch, false_branch) ->
          analyze_if state e.loc cond true_branch false_branch
      | While (cond, body) -> analyze_while state e.loc cond body
    in

    (* If the abstract state is empty then the code is unreachable and
       we annotate the expression and the whole sub-ast with the
       'Unreachable' value. Otherwise, analyze the expression normally
       assuming it is reachable. *)
    if State.is_empty state then Annotast.fill_expr e state Value.Unreachable
    else analyze_non_bot_expr state e

  (* Step 2: Analyze a binary operation *)
  and analyze_binop (loc : location) (state : State.t) (left : Ast.expr)
      (right : Ast.expr) (op : Ast.binop) =
    (* annotate recursively left and right children *)
    let left_annot = analyze_expr state left in
    let right_annot = analyze_expr left_annot.e_state right in
    (* Cast to Absint *)
    let left_int = Value.cast_int loc left_annot.e_value
    and right_int = Value.cast_int loc right_annot.e_value in
    (* Computes the result and get an Absint *)
    let value = Value.Int ((binop_to_fun op) left_int right_int) in
    (* Build the annotated node of the AST *)
    let node = Annotast.ABinop (left_annot, op, right_annot) in
    Annotast.build_expr loc node right_annot.e_state value

  (* Step 2: Analyze a comparison *)
  and analyze_relop (loc : location) (state : State.t) (left : Ast.expr)
      (right : Ast.expr) (op : Ast.relop) =
    let open Annotast in
    let left_annot = analyze_expr state left in
    let right_annot = analyze_expr left_annot.e_state right in
    let value = (relop_to_fun op) left_annot.e_value right_annot.e_value in
    let node = ARelop (left_annot, op, right_annot) in
    build_expr loc node right_annot.e_state (Value.Int value)

  (* Step 2: Analyzes a sequence of expressions in order, threading state
     through each one.  Returns the list of annotated expressions, the
     final state, and the value of the last expression. *)
  and analyze_seq (state : State.t) (exprs : Ast.expr list) :
      (State.t, Value.t) Annotast.expr list * State.t * Value.t =
    match exprs with
    | [] -> failwith "eval_seq: empty list"
    | [ e ] ->
        let new_annot = analyze_expr state e in
        ([ new_annot ], new_annot.e_state, new_annot.e_value)
    | e :: tail ->
        let new_annot = analyze_expr state e in
        let new_list, s, v = analyze_seq new_annot.e_state tail in
        ([ new_annot ] @ new_list, s, v)

  (* Step 2: Analyze an assignment.
     Hint: use write_value *)
  and analyze_assign loc (state : State.t) (left : Ast.lvalue)
      (right : Ast.expr) : (State.t, Value.t) Annotast.expr =
    let open Annotast in
    let right_annot = analyze_expr state right in
    let left_value =
      write_lvalue right_annot.e_state left right_annot.e_value
    in
    let node = AAssign (left_value, right_annot) in
    build_expr loc node left_value.l_state right_annot.e_value

  (* Step 2: Analyze an array initialization by evaluating the size and
     content expressions. Returns the annotated expression and result. *)
  and analyze_array_init loc (state : State.t) (id : string) (size : expr)
      (content : expr) : (State.t, Value.t) Annotast.expr =
    let size_annot = analyze_expr state size in
    let content_annot = analyze_expr size_annot.e_state content in
    let (size_int : Absint.t) =
      Value.cast_int size_annot.e_loc size_annot.e_value
    in
    let abs_arr = Value.array_make size_int content_annot.e_value in
    let node = Annotast.AArrayInit (id, size_annot, content_annot) in
    Annotast.build_expr loc node content_annot.e_state abs_arr

  and annot_list_to_value_list (a : (State.t, Value.t) Annotast.expr list) :
      Value.t list =
    match a with
    | [] -> failwith "eval_seq: empty list"
    | [ e ] -> [ e.e_value ]
    | e :: tail ->
        let new_list = annot_list_to_value_list tail in
        [ e.e_value ] @ new_list

  (* Step 2: Analyze a function call. Arguments are evaluated from left
     to right *)
  and analyze_funcall (state : State.t) loc (name : string) (args : expr list) :
      (State.t, Value.t) Annotast.expr =
    let open Annotast in
    let annotated_expr_list, (last_state : State.t), (value : Value.t) =
      analyze_seq state args
    in
    let value_list = annot_list_to_value_list annotated_expr_list in
    (* name and annotated_expr list *)
    let func = State.find_fun name last_state in
    let return_value = func value_list in
    let node = AFuncall (name, annotated_expr_list) in
    build_expr loc node last_state return_value
  (* Format.asprintf "%s" __FUNCTION__ |> Utils.niy *)

  (* Step 2: Analyze a let-binding *)
  and analyze_let loc state chunks body =
    let open Annotast in
    let chunks_annot, new_state = analyze_chunks state chunks in
    let body_annot, s, v = analyze_seq new_state body in
    let node = ALet (chunks_annot, body_annot) in
    build_expr loc node s v
  (* Format.asprintf "%s" __FUNCTION__ |> Utils.niy *)

  (* Step 2: Analyze a boolean operation
     - Hint : use State.join *)
  and analyze_boolop (loc : location) (state : State.t) (left : Ast.expr)
      (right : Ast.expr) (op : Ast.boolop) =
    let open Annotast in
    let left_annot = analyze_expr state left in
    let left_int = Value.cast_int loc left_annot.e_value in
    let right_annot = analyze_expr left_annot.e_state right in
    let right_int = Value.cast_int loc right_annot.e_value in
    let value = Value.Int ((boolop_to_fun op) left_int right_int) in
    let node = ABoolop (left_annot, op, right_annot) in
    (* lazy evaluation, _ represents False or Unknown *)
    match (op, Absint.truth left_int) with
    | And, True -> build_expr loc node right_annot.e_state value
    | And, _ -> build_expr loc node left_annot.e_state value
    | Or, True -> build_expr loc node left_annot.e_state value
    | Or, _ -> build_expr loc node right_annot.e_state value

  (* Step 3: Analyze an if-expression by evaluating the condition and both
     branches. Joins the resulting states and values.
     - Hint : use State.join, Value.join

     Step 5: Filters the abstract state based on the condition's truth
     value, analyzes each branch under the corresponding filtered
     state.
     - Hint: use filter *)
  and analyze_if (state : State.t) (loc : location) (cond : expr) (tbr : expr)
      (fbr : expr option) : (State.t, Value.t) Annotast.expr =
    let open Annotast in
    let cond_annot = analyze_expr state cond in
    let truth = Absint.truth (Value.cast_int loc cond_annot.e_value) in
    let tbr_annot = analyze_expr cond_annot.e_state tbr in
    (* Return None if none, an Annotast expr otherwise *)
    let (fbr_annot : (State.t, Value.t) Annotast.expr option) =
      (* we use the state from cond_annot because we cannot go in both branhces *)
      (Option.map (fun e -> analyze_expr cond_annot.e_state e)) fbr
    in
    let node = AIfThenElse (cond_annot, tbr_annot, fbr_annot) in
    let joined_state, joined_value =
      match (fbr_annot, truth) with
      | None, False -> (cond_annot.e_state, cond_annot.e_value)
      | None, _ -> (tbr_annot.e_state, tbr_annot.e_value)
      | Some e, True -> (tbr_annot.e_state, tbr_annot.e_value)
      | Some e, False -> (e.e_state, e.e_value)
      | Some e, Unknown ->
          ( State.join tbr_annot.e_state e.e_state,
            Value.join tbr_annot.e_value e.e_value )
    in
    build_expr loc node joined_state joined_value

  (* [accumulate cond body initial] simulates one additional iteration of a while-loop:
    - It analyzes the loop condition [cond] under the current abstract state [initial].
    - filters this state to keep only the executions where the condition *may be* true
        (i.e., where the loop would proceed).
    - The loop body [body] is analyzed under this filtered state.
    - joins the resulting state with the original [initial] to accumulate
        the effect of one more potential loop iteration.
    *)
  and accumulate (cond : expr) (body : expr) (initial : State.t) =
    let cond_annot = analyze_expr initial cond in
    let s' = cond_annot.e_state in
    let s' = filter s' cond_annot true in
    let ab = analyze_expr s' body in
    State.join initial ab.e_state

  (* Step 3: Analyze a while loop by evaluating its condition and body
     repeatedly until the condition can be proven to be statically
     false. Raises Not_Implement_Yet if the number of iteration
     exceeds Utils.max_iter. This is a first naive approach, we'll do
     better at step 6.

     Step 6: Analyze of a while-loop by computing a fixed point over the loop body.
     Repeatedly analyzes the condition and body under the filtered true state,
     joins intermediate states to approximate the loop effect, and filters the
     final state with the condition being false to model loop exit. *)
  and hat_while (count : int) (state : State.t) (loc : location) (cond : expr)
      (body : expr) : (State.t, Value.t) Annotast.expr =
    let open Annotast in
    match count with
    | 5 ->
        let accumulated_state = fix (accumulate cond body) state in
        let cond_annot = analyze_expr accumulated_state cond in
        let body_annot = analyze_expr cond_annot.e_state body in
        let node = AWhile (cond_annot, body_annot) in
        build_expr loc node body_annot.e_state body_annot.e_value
    (* | 5 -> Utils.niy "Max iter reached" *)
    (* | Utils.max_iter -> Utils.niy "Max iter reached" *)
    | _ -> (
        let cond_annot = analyze_expr state cond in
        let body_annot = analyze_expr cond_annot.e_state body in
        let truth = Absint.truth (Value.cast_int loc cond_annot.e_value) in
        match truth with
        | False ->
            let node = AWhile (cond_annot, body_annot) in
            build_expr loc node body_annot.e_state body_annot.e_value
        | _ -> hat_while (count + 1) body_annot.e_state loc cond body)

  and analyze_while (state : State.t) (loc : location) (cond : expr)
      (body : expr) : (State.t, Value.t) Annotast.expr =
    hat_while 0 state loc cond body
  (* fix (accumulate expr body state) state *)

  (* Evaluates an lvalue to read its value.
     - If the lvalue is a variable, retrieves its value from the current state.
     - If it's an array access, recursively reads the base lvalue, evaluates the
       index expression and retrieves the corresponding element from the array. *)
  and read_lvalue state (lv : lvalue) : (State.t, Value.t) Annotast.lvalue =
    let open Annotast in
    match lv.payload with
    | Var id ->
        let value = State.find_value id state in
        build_lval lv.loc (AVar id) state value
    | Array (lv', idx) ->
        let annot_lvalue = read_lvalue state lv' in
        let arr = Value.cast_array lv'.loc annot_lvalue.l_value in
        let index_annot = analyze_expr state idx in
        let index = Value.cast_int lv'.loc index_annot.e_value in
        let result_value = Value.array_get arr index in
        let node = AArray (annot_lvalue, index_annot) in
        build_lval lv'.loc node index_annot.e_state result_value

  (* Evaluates an lvalue to perform a write operation.
     - If the lvalue is a variable, updates its value in the current state.
     - If it's an array access, recursively reads the base lvalue, evaluates
       the index expression, updates the corresponding element in the array,
       and recursively writes the modified array back. *)
  and write_lvalue state (lv : lvalue) (v : Value.t) :
      (State.t, Value.t) Annotast.lvalue =
    let open Annotast in
    match lv.payload with
    | Var id ->
        let state = State.update_value id v state in
        build_lval lv.loc (AVar id) state Value.Void
    | Array (lv', idx) ->
        let annot_lvalue = read_lvalue state lv' in
        let arr = Value.cast_array lv'.loc annot_lvalue.l_value in
        let index_annot = analyze_expr annot_lvalue.l_state idx in
        let index = Value.cast_int lv'.loc index_annot.e_value in
        let result_value = Value.array_set arr index v in
        write_lvalue index_annot.e_state lv' result_value

  (* Analyze and annotate each chunk in the chunk list. Analyzing a
     chunk may modify the state, so the updated state must be
     propagated to the next chunk.  Returns the list of annotated
     chunks and the final state. *)
  and analyze_chunks (state : State.t) (chunks : Ast.chunk list) :
      (State.t, Value.t) Annotast.chunk list * State.t =
    List.fold_left
      (fun (acc, s) c ->
        let c_annot = analyze_chunk s c in
        (acc @ [ c_annot ], c_annot.c_state))
      ([], state) chunks

  and analyze_chunk state (c : chunk) : (State.t, Value.t) Annotast.chunk =
    let open Annotast in
    match c.payload with
    | Typedec (id, typ) -> build_chunk c.loc (ATypedec (id, typ)) state
    | Exp e ->
        let e_annot = analyze_expr state e in
        build_chunk c.loc (AExp e_annot) e_annot.e_state
    | Vardec (id, typ_opt, e) ->
        let e_annot = analyze_expr state e in
        let state = State.add_value id e_annot.e_value e_annot.e_state in
        build_chunk c.loc (AVardec (id, typ_opt, e_annot)) state

  (** entry point of the analyzer *)
  let analyze_program ?(show_annotast = false) ?(pdf = false) (p : Ast.program)
      : (State.t, Value.t) Annotast.program =
    let runtime =
      [
        ( "range",
          function
          | [ Value.Int l; Value.Int h ] -> Value.Int (Absint.range l h)
          | _ -> failwith "type error" );
        ( "print_int",
          function [ Value.Int _x ] -> Value.Void | _ -> failwith "type error"
        );
        ( "concat",
          function
          | [ Value.String s1; Value.String s2 ] ->
              Value.String (Absstring.concat s1 s2)
          | _ -> failwith "type error" );
        ("print", function [ String _x ] -> Void | _ -> failwith "type error");
      ]
    in
    let state = State.init runtime in
    let annotd, _ = analyze_chunks state p in
    let pp_state s = Format.asprintf "%a" State.print s in
    let pp_res r = Format.asprintf "%a" Value.print r in
    (if show_annotast then
       let pp_state fmt (s : State.t) =
         match s with
         | Bot -> Format.fprintf fmt "⊥"
         | Nonbot { values; _ } ->
             if Env.is_empty values then Format.fprintf fmt "∅"
             else State.print fmt s
       in
       Annotast.print_program_and_state pp_state Format.std_formatter annotd);
    if pdf then
      Pdfprinter.write_to_file
        ~color_state:(function State.Bot -> "gray" | _ -> "red")
        ~color_expr:(function
          | Value.Int _ -> "green"
          | Value.String _ -> "orange"
          | Value.Unreachable -> "gray"
          | _ -> "blue")
        (Filename.chop_extension !Utils.file ^ ".tex")
        pp_state pp_res annotd;
    annotd
end
