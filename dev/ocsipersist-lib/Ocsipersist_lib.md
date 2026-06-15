
# Module `Ocsipersist_lib`

This modules provides tools for creating more implementations of the `Ocsipersist` virtual module.

```ocaml
module Sigs : sig ... end
```
```ocaml
val is_valid_name_char : char -> bool
```
```ocaml
val validate_name : string -> unit
```
```ocaml
module Polymorphic (Functorial : Sigs.FUNCTORIAL) : Sigs.POLYMORPHIC
```
deriving polymorphic interface from the functorial one

```ocaml
module Variable (T : sig ... end) : sig ... end
```
```ocaml
module Ref (Store : Sigs.STORE) : sig ... end
```
```ocaml
module Store_json (Functorial : Sigs.FUNCTORIAL) : Sigs.STORE_JSON
```
```ocaml
module Ref_json (Functorial : Sigs.FUNCTORIAL) : Sigs.REF_JSON
```
Type-safe persistent references using `Deriving_Json` for serialisation.
