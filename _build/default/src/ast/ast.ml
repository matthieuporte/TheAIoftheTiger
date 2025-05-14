(* ranges in source file *)
type location = Lexing.position * Lexing.position

(* Polymorphic located values. *)
type 'a located = { payload : 'a; loc : location }

(* Language *)
(************)

(* types *)
type typ = T_array of typ | T_id of string | T_record of (string * typ) list

(* operators *)
type binop = Add | Sub | Mul | Div
type relop = Eq | Ne | Lt | Le | Gt | Ge
type boolop = And | Or

(* expressions *)
type expr = expr_payload located

and expr_payload =
  | Const of int
  | String of string
  | Lval of lvalue
  | Seq of expr list
  | Assign of lvalue * expr
  | Binop of (expr * binop * expr)
  | Relop of (expr * relop * expr)
  | Boolop of (expr * boolop * expr)
  | IfThenElse of (expr * expr * expr option)
  | While of (expr * expr)
  | Funcall of (string * expr list)
  | Let of chunk list * expr list
  | ArrayInit of string * expr * expr

(* left values *)
and lvalue = lvalue_payload located
and lvalue_payload = Var of string | Array of lvalue * expr

(* chunks*)
and chunk = chunk_payload located

and chunk_payload =
  | Typedec of string * typ
  | Vardec of string * typ option * expr
  | Exp of expr

type program = chunk list

(* constructor *)
let build loc payload = { loc; payload }

(************)
(* Printing *)
(************)

let binop_to_string (b : binop) =
  match b with Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"

let relop_to_string (r : relop) =
  match r with
  | Eq -> "="
  | Ne -> "<>"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="

let boolop_to_string b = match b with And -> "&" | Or -> "|"

let rec print_typ fmt t =
  match t with
  | T_id s -> Format.fprintf fmt "%s" s
  | T_array t -> Format.fprintf fmt "array of %a" print_typ t
  | T_record fields ->
      Format.fprintf fmt "{%a}"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ",")
           (fun fmt (name, t) -> Format.fprintf fmt "%s : %a" name print_typ t))
        fields

let rec print_expr fmt e =
  match e.payload with
  | Const i -> Format.fprintf fmt "%i" i
  | Seq l ->
      Format.fprintf fmt "@[<v>(%a)@]"
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
           print_expr)
        l
  | String s -> Format.fprintf fmt "\"%s\"" (String.escaped s)
  | Assign (left, right) ->
      Format.fprintf fmt "%a := %a" print_lvalue left print_expr right
  | Funcall (name, args) ->
      Format.fprintf fmt "%s(%a)" name
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ")
           print_expr)
        args
  | Binop (e1, b, e2) ->
      Format.fprintf fmt "%a %s %a" print_expr e1 (binop_to_string b) print_expr
        e2
  | Boolop (e1, b, e2) ->
      Format.fprintf fmt "%a %s %a" print_expr e1 (boolop_to_string b)
        print_expr e2
  | Relop (e1, r, e2) ->
      Format.fprintf fmt "%a %s %a" print_expr e1 (relop_to_string r) print_expr
        e2
  | IfThenElse (cond, tbr, Some fbr) ->
      Format.fprintf fmt "@[<v 2>if %a then@, %a@]@,@[<v 2>else@, %a@]"
        print_expr cond print_expr tbr print_expr fbr
  | IfThenElse (cond, tbr, None) ->
      Format.fprintf fmt "@[<v 2>if %a then@, %a@]" print_expr cond print_expr
        tbr
  | While (cond, body) ->
      Format.fprintf fmt "@[<v 2>while %a do@,%a@]" print_expr cond print_expr
        body
  | Lval v -> Format.fprintf fmt "%a" print_lvalue v
  | Let (chunks, body) ->
      Format.fprintf fmt "@[<v 2>let@,%a@]@,@[<v 2>in@,%a@]@,end" print_chunks
        chunks
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt ";@,")
           print_expr)
        body
  | ArrayInit (id, size, content) ->
      Format.fprintf fmt "%s[%a] of %a" id print_expr size print_expr content

and print_lvalue fmt l =
  match l.payload with
  | Var v -> Format.fprintf fmt "%s" v
  | Array (lv, dim) ->
      Format.fprintf fmt "%a[%a]" print_lvalue lv print_expr dim

and print_chunks fmt c =
  Format.fprintf fmt "@[<v>%a@]" (Format.pp_print_list print_chunk) c

and print_chunk fmt c =
  match c.payload with
  | Typedec (id, typ) -> Format.fprintf fmt "type %s = %a" id print_typ typ
  | Vardec (id, None, rvalue) ->
      Format.fprintf fmt "var %s := %a" id print_expr rvalue
  | Vardec (id, Some ty, rvalue) ->
      Format.fprintf fmt "var %s : %a := %a" id print_typ ty print_expr rvalue
  | Exp e -> Format.fprintf fmt "%a" print_expr e

let print_location fmt (p1, p2) =
  let open Lexing in
  Format.fprintf fmt "Line %i, col %i to Line %i, col %i" p1.pos_lnum
    (p1.pos_cnum - p1.pos_bol) p2.pos_lnum (p2.pos_cnum - p2.pos_bol)

let print_program = print_chunks
