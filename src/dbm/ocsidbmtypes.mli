(* Ocsigen
 * http://www.ocsigen.org
 * Copyright (C) 2007 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, with linking exception;
 * either version 2.1 of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)
exception Ocsidbm_error

type query =
  | Get of string * string
  | Remove of string * string
  | Replace of string * string * string
  | Replace_if_exists of string * string * string
  | Nextkey of string
  | Firstkey of string
  | Length of string
  | Replace_if_unchanged of string * string * string option * string option
  (** [Replace_if_unchanged (table, key, expected, value)] binds [key] to
      [value], or removes it if [value] is [None], only if its value is still
      [expected] ([None] if absent). The answer is [Ok], or [Changed current]
      with the current value, and nothing is written. *)

type answer =
  | Ok
  | Dbm_not_found
  | Value of string
  | End
  | Key of string
  | Error of exn
  | Changed of string option
