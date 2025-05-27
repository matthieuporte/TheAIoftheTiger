open Ast

(* Annotated expressions parameterized by abstract state ('a) and abstract value ('b).
   Each expression node carries:
   - its payload (the structure of the expression),
   - the abstract state *after* analyzing this node,
   - the abstract value computed for this node,
   - and its source location. *)
type ('a, 'b) expr = {
  e_payload : ('a, 'b) expr_payload;
  e_state : 'a;
  e_value : 'b;
  e_loc : location;
}

and ('a, 'b) expr_payload =
  | AConst of int
  | AString of string
  | ALval of ('a, 'b) lvalue
  | ASeq of ('a, 'b) expr list
  | AAssign of ('a, 'b) lvalue * ('a, 'b) expr
  | ABinop of (('a, 'b) expr * binop * ('a, 'b) expr)
  | ARelop of (('a, 'b) expr * relop * ('a, 'b) expr)
  | ABoolop of (('a, 'b) expr * boolop * ('a, 'b) expr)
  | AIfThenElse of (('a, 'b) expr * ('a, 'b) expr * ('a, 'b) expr option)
  | AWhile of (('a, 'b) expr * ('a, 'b) expr)
  | AFuncall of (string * ('a, 'b) expr list)
  | ALet of ('a, 'b) chunk list * ('a, 'b) expr list
  | AArrayInit of string * ('a, 'b) expr * ('a, 'b) expr

and ('a, 'b) lvalue = {
  l_payload : ('a, 'b) lvalue_payload;
  l_state : 'a;
  l_value : 'b;
  l_loc : location;
}

and ('a, 'b) lvalue_payload =
  | AVar of string
  | AArray of ('a, 'b) lvalue * ('a, 'b) expr
  | ARecordField of ('a, 'b) lvalue * string

and ('a, 'b) chunk = {
  c_payload : ('a, 'b) chunk_payload;
  c_state : 'a;
  c_loc : location;
}

and ('a, 'b) chunk_payload =
  | ATypedec of string * typ
  | AVardec of string * typ option * ('a, 'b) expr
  | AExp of ('a, 'b) expr

type ('a, 'b) program = ('a, 'b) chunk list

let rec print_expr pp_state pp_value fmt (e : ('a, 'b) expr) =
  Format.fprintf fmt "@[<v 0>%a@]@,@[<v 0>%a@]" pp_state e.e_state pp_value
    e.e_value;
  match e.e_payload with
  | AConst i -> Format.fprintf fmt "%i" i
  | AString s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | ASeq l ->
      Format.fprintf fmt "@[<v>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
           (print_expr pp_state pp_value))
        l
  | AAssign (left, right) ->
      Format.fprintf fmt "%a := %a"
        (print_lvalue pp_state pp_value)
        left
        (print_expr pp_state pp_value)
        right
  | ABinop (e1, b, e2) ->
      Format.fprintf fmt "%a %s %a"
        (print_expr pp_state pp_value)
        e1 (binop_to_string b)
        (print_expr pp_state pp_value)
        e2
  | ABoolop (e1, b, e2) ->
      Format.fprintf fmt "%a %s %a"
        (print_expr pp_state pp_value)
        e1 (boolop_to_string b)
        (print_expr pp_state pp_value)
        e2
  | ARelop (e1, r, e2) ->
      Format.fprintf fmt "%a %s %a"
        (print_expr pp_state pp_value)
        e1 (relop_to_string r)
        (print_expr pp_state pp_value)
        e2
  | AIfThenElse (cond, tbr, Some fbr) ->
      Format.fprintf fmt "@[<v 2>if %a then@, %a@]@,@[<v 2>else@, %a@]"
        (print_expr pp_state pp_value)
        cond
        (print_expr pp_state pp_value)
        tbr
        (print_expr pp_state pp_value)
        fbr
  | AIfThenElse (cond, tbr, None) ->
      Format.fprintf fmt "@[<v 2>if %a then@, %a@]"
        (print_expr pp_state pp_value)
        cond
        (print_expr pp_state pp_value)
        tbr
  | AWhile (cond, body) ->
      Format.fprintf fmt "@[<v 2>while %a do@,%a@]"
        (print_expr pp_state pp_value)
        cond
        (print_expr pp_state pp_value)
        body
  | ALval v -> Format.fprintf fmt "%a" (print_lvalue pp_state pp_value) v
  | AFuncall (name, args) ->
      Format.fprintf fmt "%s(%a)" name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           (print_expr pp_state pp_value))
        args
  | ALet (chunks, body) ->
      Format.fprintf fmt "@[<v 2>let@,%a@]@,@[<v 2>in@,%a@]@,end"
        (print_chunks pp_state pp_value)
        chunks
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
           (print_expr pp_state pp_value))
        body
  | AArrayInit (id, size, content) ->
      Format.fprintf fmt "%s[%a] of %a" id
        (print_expr pp_state pp_value)
        size
        (print_expr pp_state pp_value)
        content

and print_lvalue pp_state pp_value fmt (lv : ('a, 'b) lvalue) =
  Format.fprintf fmt "@[<v 0> %a@]@,@[<v 0> %a@]@," pp_state lv.l_state pp_value
    lv.l_value;
  match lv.l_payload with
  | AVar v -> Format.fprintf fmt "%s" v
  | ARecordField (lv, f) ->
      Format.fprintf fmt "%a.%s" (print_lvalue pp_state pp_value) lv f
  | AArray (lv, dim) ->
      Format.fprintf fmt "%a[%a]"
        (print_lvalue pp_state pp_value)
        lv
        (print_expr pp_state pp_value)
        dim

and print_chunks pp_state pp_value fmt c =
  Format.fprintf fmt "@[<v>%a@]"
    (Format.pp_print_list (print_chunk pp_state pp_value))
    c

and print_chunk pp_state pp_value fmt (chunk : ('a, 'b) chunk) =
  Format.fprintf fmt "@[<v 0>%a@]" pp_state chunk.c_state;
  match chunk.c_payload with
  | ATypedec (id, typ) -> Format.fprintf fmt "type %s = %a" id print_typ typ
  | AVardec (id, None, rvalue) ->
      Format.fprintf fmt "var %s := %a" id (print_expr pp_state pp_value) rvalue
  | AVardec (id, Some ty, rvalue) ->
      Format.fprintf fmt "var %s : %a := %a" id print_typ ty
        (print_expr pp_state pp_value)
        rvalue
  | AExp e -> (print_expr pp_state pp_value) fmt e

(* We use build_expr, build_lval, and build_chunk so that every
   construction of an annotated AST node passes through a single
   place. That way, later we can easily add invariants, logging,
   debugging hooks ... without changing too much code. *)
let build_expr e_loc e_payload e_state e_value =
  { e_payload; e_state; e_value; e_loc }

let build_lval l_loc l_payload l_state l_value =
  { l_payload; l_state; l_value; l_loc }

let build_chunk c_loc c_payload c_state = { c_payload; c_state; c_loc }

(* Recursively annotate an expression by copying its structure and
   filling each node with default abstract state and abstract
   value.  Used, for example, to annotate unreachable code. *)
let rec fill_expr (ast : Ast.expr) (state : 'a) value : ('a, 'b) expr =
  match ast.payload with
  | Ast.Const c -> build_expr ast.loc (AConst c) state value
  | Ast.String s -> build_expr ast.loc (AString s) state value
  | Ast.Lval lv ->
      build_expr ast.loc (ALval (fill_lvalue lv state value)) state value
  | Ast.Seq l ->
      build_expr ast.loc
        (ASeq (List.map (fun e -> fill_expr e state value) l))
        state value
  | Ast.Assign (lv, e) ->
      build_expr ast.loc
        (AAssign (fill_lvalue lv state value, fill_expr e state value))
        state value
  | Ast.Binop (e1, op, e2) ->
      build_expr ast.loc
        (ABinop (fill_expr e1 state value, op, fill_expr e2 state value))
        state value
  | Ast.Relop (e1, op, e2) ->
      build_expr ast.loc
        (ARelop (fill_expr e1 state value, op, fill_expr e2 state value))
        state value
  | Ast.Boolop (e1, op, e2) ->
      build_expr ast.loc
        (ABoolop (fill_expr e1 state value, op, fill_expr e2 state value))
        state value
  | Ast.IfThenElse (cond, tbr, fbr) ->
      build_expr ast.loc
        (AIfThenElse
           ( fill_expr cond state value,
             fill_expr tbr state value,
             Option.map (fun e -> fill_expr e state value) fbr ))
        state value
  | Ast.While (cond, body) ->
      build_expr ast.loc
        (AWhile (fill_expr cond state value, fill_expr body state value))
        state value
  | Ast.Funcall (name, args) ->
      build_expr ast.loc
        (AFuncall (name, List.map (fun e -> fill_expr e state value) args))
        state value
  | Ast.Let (chunks, body) ->
      build_expr ast.loc
        (ALet
           ( List.map (fun c -> fill_chunk c state value) chunks,
             List.map (fun e -> fill_expr e state value) body ))
        state value
  | Ast.ArrayInit (name, size, content) ->
      build_expr ast.loc
        (AArrayInit
           (name, fill_expr size state value, fill_expr content state value))
        state value

and fill_lvalue (lv : Ast.lvalue) (state : 'a) value : ('a, 'b) lvalue =
  match lv.payload with
  | Ast.Var id -> build_lval lv.loc (AVar id) state value
  | Ast.Array (lv, idx) ->
      build_lval lv.loc
        (AArray (fill_lvalue lv state value, fill_expr idx state value))
        state value

and fill_chunk (chunk : Ast.chunk) (state : 'a) value : ('a, 'b) chunk =
  match chunk.payload with
  | Ast.Typedec (id, typ) -> build_chunk chunk.loc (ATypedec (id, typ)) state
  | Ast.Vardec (id, ty_opt, expr) ->
      build_chunk chunk.loc
        (AVardec (id, ty_opt, fill_expr expr state value))
        state
  | Ast.Exp expr ->
      build_chunk chunk.loc (AExp (fill_expr expr state value)) state

let print_program_and_state state_print fmt cl =
  let rec print_chunks fmt cl =
    Format.fprintf fmt "@[<v>%a@]" (Format.pp_print_list print_chunk) cl
  and print_chunk fmt { c_payload; c_state; _ } =
    match c_payload with
    | ATypedec (id, typ) ->
        Format.fprintf fmt "@[<h>type %s = %a@]" id print_typ typ
    | AVardec (id, None, rvalue) ->
        Format.fprintf fmt "@[<h>var %s := %a\t%a@]" id print_expr rvalue
          state_print c_state
    | AVardec (id, Some ty, rvalue) ->
        Format.fprintf fmt "@[<h>var %s : %a := %a\t%a@]" id print_typ ty
          print_expr rvalue state_print c_state
    | AExp e -> Format.fprintf fmt "%a" print_expr e
  and print_lvalue fmt l =
    match l.l_payload with
    | AVar v -> Format.fprintf fmt "%s" v
    | AArray (lv, dim) ->
        Format.fprintf fmt "%a[%a]" print_lvalue lv print_expr dim
    | ARecordField _ -> assert false
  and print_expr fmt { e_payload; e_state; _ } =
    match e_payload with
    | AConst i -> Format.fprintf fmt "%i" i
    | ASeq l ->
        Format.fprintf fmt "@[<v 2>(@ %a@]@\n)"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt " ;@,")
             print_expr)
          l
    | AString s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
    | AAssign (left, right) ->
        Format.fprintf fmt "@[<h>%a := %a\t%a@]" print_lvalue left print_expr
          right state_print e_state
    | AFuncall (name, args) ->
        Format.fprintf fmt "@[<h>%s(%a)\t%a@]" name
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
             print_expr)
          args state_print e_state
    | ABinop (e1, b, e2) ->
        Format.fprintf fmt "(%a %s %a)" print_expr e1 (binop_to_string b)
          print_expr e2
    | ABoolop (e1, b, e2) ->
        Format.fprintf fmt "(%a %s %a)" print_expr e1 (boolop_to_string b)
          print_expr e2
    | ARelop (e1, r, e2) ->
        Format.fprintf fmt "(%a %s %a)" print_expr e1 (relop_to_string r)
          print_expr e2
    | AIfThenElse (cond, tbr, Some fbr) ->
        Format.fprintf fmt
          "@[<v 2>if %a then@, %a\t%a@]@,@[<v 2>else@, %a\t%a@]" print_expr cond
          print_expr tbr state_print tbr.e_state print_expr fbr state_print
          fbr.e_state
    | AIfThenElse (cond, tbr, None) ->
        Format.fprintf fmt "@[<v 2>if %a then@, %a@]" print_expr cond print_expr
          tbr
    | AWhile (cond, ({ e_payload = ASeq _; _ } as body)) ->
        Format.fprintf fmt "@[<v 2>while %a do@ %a@]" print_expr cond print_expr
          body;
        Format.fprintf fmt "\t%a" state_print e_state
    | AWhile (cond, body) ->
        Format.fprintf fmt "@[<v 2>while %a do (@ %a@]@\n)" print_expr cond
          print_expr body;
        Format.fprintf fmt "\t%a" state_print e_state
    | ALval v -> Format.fprintf fmt "%a" print_lvalue v
    | ALet (chunks, body) ->
        Format.fprintf fmt "@[<v 2>let@ %a@]@\n" print_chunks chunks;
        Format.fprintf fmt "@[<v 2>in @ %a@]@\nend"
          (Format.pp_print_list
             ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
             print_expr)
          body;
        Format.fprintf fmt "\t%a" state_print e_state
    | AArrayInit (id, size, content) ->
        Format.fprintf fmt "@[<h>%s[%a] of %a@]" id print_expr size print_expr
          content
  in
  let s = Format.asprintf "%a" print_chunks cl in
  let ll =
    String.split_on_char '\n' s
    |> List.map (String.split_on_char '\t')
    |> List.map (function
         | [] -> []
         | [ h ] -> [ h ]
         | h :: h' :: _ as l ->
             (* T®1cKs *)
             let l' = String.length h' in
             if h'.[l' - 1] = ';' then [ h ^ ";"; String.sub h' 0 (l' - 1) ]
             else l)
  in
  let needed_len =
    List.fold_left
      (fun acc -> function
        | [] | [ _ ] -> acc
        | s :: _ -> max acc (String.length s))
      0 ll
    + 2
  in
  let cyan = Printf.sprintf "\027[38;2;0;255;255m" in
  List.iter
    (function
      | [] -> Format.fprintf fmt "@\n"
      | [ ast ] -> Format.fprintf fmt "%s@\n" ast
      | ast :: l ->
          Format.fprintf fmt "%s" ast;
          let spacing = needed_len - String.length ast in
          Format.pp_print_string fmt (String.make spacing ' ');
          Format.fprintf fmt "\027[0m%s%s\027[0m@\n" cyan (List.hd (List.rev l)))
    ll
