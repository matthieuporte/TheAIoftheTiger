let show_ast = ref false
let show_annotated_ast = ref false
let globaldescr = "Tiger_ai is a static analyzer for the tiger language"
let domain = ref "const"
let pdf = ref false
let report = ref false
let rewrite = ref false

let options =
  [
    ("--show-ast", Arg.Set show_ast, "Enables the printing of the AST");
    ( "--annot",
      Arg.Set show_annotated_ast,
      "Enables the printing of the AST along with their abstract state" );
    ( "--pdf",
      Arg.Set pdf,
      "Enables the printing of the annotated AST int a pdf file" );
    ("--report", Arg.Set report, "Enables the generation of a report");
    ("--domain", Arg.Set_string domain, "Changes the evaluation domain");
    ( "--rewrite",
      Arg.Set rewrite,
      "Rewrites the program with annotations on safe array accesses" );
  ]

let parse_args () =
  let set_prog s = Utils.file := s in
  try
    Arg.parse_argv Sys.argv options set_prog globaldescr;
    if !Utils.file = "" then (
      Format.printf "Please give me a .tig file\n";
      exit 0);
    if Sys.file_exists !Utils.file then !Utils.file
    else (
      Format.printf "%s : file not found\n" !Utils.file;
      exit 0)
  with Arg.Bad s | Arg.Help s ->
    Format.printf "%s" s;
    exit 0

(* const *)
module CI = Absinterpreter.Make (Absconst)
module SAC = Safeaccess.Make (Absconst)

(* parity *)
module PI = Absinterpreter.Make (Absparity)
module SAP = Safeaccess.Make (Absparity)

(* intervals *)
module II = Absinterpreter.Make (Absinterval)
module SAI = Safeaccess.Make (Absinterval)

let () =
  Random.self_init ();
  let file = parse_args () in
  let ast = Fileparser.parse file in
  if !show_ast then Format.printf "%a@,%!" Ast.print_program ast;
  match !domain with
  | "parity" ->
      let annot_ast =
        PI.analyze_program ~show_annotast:!show_annotated_ast ~pdf:!pdf ast
      in
      let safe, unsafe = SAP.validate_program ~report:!report annot_ast in
      Format.printf "%i/%i access proven safe\n" safe (safe + unsafe)
  | "interval" ->
      let annot_ast =
        II.analyze_program ~show_annotast:!show_annotated_ast ~pdf:!pdf ast
      in
      let safe, unsafe = SAI.validate_program ~report:!report annot_ast in
      Format.printf "%i/%i access proven safe\n" safe (safe + unsafe)
  | "const" ->
      let annot_ast =
        CI.analyze_program ~show_annotast:!show_annotated_ast ~pdf:!pdf ast
      in
      let safe, unsafe = SAC.validate_program ~report:!report annot_ast in
      Format.printf "%i/%i access proven safe\n" safe (safe + unsafe)
  | "" -> ()
  | s -> failwith (Format.asprintf "domain %s unknown" s)
