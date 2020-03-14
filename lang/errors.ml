open Exprs
open Types

(*
when adding an error:
- put it in pretty
*)

exception DivideByZero of sourcespan
exception InternalError of string
exception UnboundId of string * sourcespan
exception DuplicateId of string * sourcespan * sourcespan (* name, where duplicated, where originated *)
exception ArgumentError of string * sourcespan

exception TypeMismatch of sourcespan typ * sourcespan typ * sourcespan
exception RecursiveType of string * sourcespan typ * sourcespan