Comment matcher un custom type ?

```
  match c.payload with
  | Exp e -> failwith "Not implemented"
  | Typedec (name t) -> failwith "Not implemented"
  | Vardec (name, t, e) -> failwith "Not implemented"
```

pourquoi il y a des Exp dans `eval_chunk` ? 