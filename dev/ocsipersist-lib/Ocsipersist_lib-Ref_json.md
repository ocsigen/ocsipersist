# Module `Ocsipersist_lib.Ref_json`

Type-safe persistent references using `Deriving_Json` for serialisation.

## Parameters

```ocaml
module Functorial : Sigs.FUNCTORIAL
```

## Signature

```ocaml
type 'a t
```
The type of (persistent or not) references

```ocaml
val ref : ?persistent:string -> 'a Deriving_Json.t -> 'a -> 'a t
```
`ref ?persistent json default` creates a reference. If optional parameter `?persistent` is absent, the reference will not be persistent (implemented using OCaml references). Otherwise, the value of `persistent` will be used as key for the value in the persistent reference table. If the reference already exists, the current value is kept.

`ref` does not access the database: the reference is looked up, and `default` written if it does not exist, at the first [`get`](./#val-get) or [`set`](./#val-set). So a persistent reference can be created before the backend is configured, for instance when a module is initialised. If that first access fails, for instance because the backend is not configured yet, the next one tries again.

```ocaml
val get : 'a t -> 'a Lwt.t
```
Get the value of a reference

```ocaml
val set : 'a t -> 'a -> unit Lwt.t
```
Set the value of a reference
