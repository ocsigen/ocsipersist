
# Module `Ocsipersist_lib.Variable`


## Parameters

```ocaml
module T : sig ... end
```

## Signature

```ocaml
type t = {
  name : T.k;
  default : unit -> T.v Lwt.t;
}
```
```ocaml
val make_lazy_lwt : name:T.k -> default:(unit -> T.v Lwt.t) -> t
```
```ocaml
val make_lazy : name:T.k -> default:(unit -> T.v) -> t
```
```ocaml
val make : name:T.k -> default:T.v -> t
```
```ocaml
val get : t -> T.v Lwt.t
```
```ocaml
val set : t -> T.v -> unit Lwt.t
```