
# Module `Ocsidbmtypes`

```ocaml
exception Ocsidbm_error
```
```ocaml
type query = 
  | Get of string * string
  | Remove of string * string
  | Replace of string * string * string
  | Replace_if_exists of string * string * string
  | Nextkey of string
  | Firstkey of string
  | Length of string
```
```ocaml
type answer = 
  | Ok
  | Dbm_not_found
  | Value of string
  | End
  | Key of string
  | Error of exn
```