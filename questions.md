Comment matcher un custom type ?

```
  match c.payload with
  | Exp e -> failwith "Not implemented"
  | Typedec (name t) -> failwith "Not implemented"
  | Vardec (name, t, e) -> failwith "Not implemented"
```

pourquoi il y a des Exp dans `eval_chunk` ? 
La doc tiger :

```
(* === Chunks of declarations. === *)
chunks = { chunk } ;
chunk =
    { tydec }   (* tychunk *)
  | { fundec }  (* funchunk *)
  | vardec      (* varchunk *)

  (* Importing a set of declarations. *)
  | "import" string
  ;
```

Je ne comprends pas comment manipuler :

```
type expr = expr_payload located
```