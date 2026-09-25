# Module `Ocsipersist_lib`

This modules provides tools for creating more implementations of the `Ocsipersist` virtual module.

```ocaml
module Sigs : sig ... end
```
```ocaml
exception Decoding_error of string
```
Raised by the JSON frontends when a value read from the backend cannot be deserialised by the codec it was opened with (the type changed, or the stored data is corrupted). The argument is the decoder's error message.

```ocaml
val decode_json : 'a Deriving_Json.t -> string -> 'a
```
Decode a JSON value stored in a backend, turning the decoder's `Failure` into [`Decoding_error`](./#exception-Decoding_error) so that callers can distinguish an unreadable value from a backend error.

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
