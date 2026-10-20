# Module `Ocsipersist_lib.Ref`

## Parameters

```ocaml
module Store : Sigs.STORE
```

## Signature

```ocaml
val store : Store.store Lazy_lwt.t
```
```ocaml
type 'a t = 
  | Ref of 'a Stdlib.ref
  | Per of 'a Store.t Lazy_lwt.t
```
```ocaml
val ref : ?persistent:string -> 'a -> 'a t
```
```ocaml
val get : 'a t -> 'a Lwt.t
```
```ocaml
val set : 'a t -> 'a -> unit Lwt.t
```
