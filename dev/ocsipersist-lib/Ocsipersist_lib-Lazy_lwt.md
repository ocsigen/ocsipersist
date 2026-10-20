# Module `Ocsipersist_lib.Lazy_lwt`

Promises computed at first use. Unlike `Lwt.t Lazy.t`, a failure is not kept: the next use runs the computation again. Ocsipersist uses them to access the database only when a value is first used, not when it is created, and to retry an access that failed, for instance because the backend was not configured yet.

```ocaml
type 'a t
```
```ocaml
val make : (unit -> 'a Lwt.t) -> 'a t
```
`make f` is a promise computed by `f ()` at its first [`force`](./#val-force).

```ocaml
val force : 'a t -> 'a Lwt.t
```
`force l` runs `f ()`, where `f` is the function given to [`make`](./#val-make), at the first call, and returns the same result to later calls while it is pending or fulfilled: concurrent callers share one computation. If it is rejected, the next call runs `f ()` again, also when it is made from a failure handler of a caller. Cancelling the promise returned to one caller does not cancel the shared computation. `f` must not force the value it computes.
