
# Module type `Sigs.FUNCTORIAL`

```ocaml
type internal
```
```ocaml
module type COLUMN = sig ... end
```
```ocaml
module Table
  (_ : sig ... end)
  (Key : COLUMN)
  (Value : COLUMN) : 
  TABLE with type key = Key.t and type value = Value.t
```
```ocaml
module Column : sig ... end
```