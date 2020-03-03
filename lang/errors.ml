open Exprs
open Types

exception DivideByZero of sourcespan
exception InternalError of string
exception UnboundId of string * sourcespan
exception ArgumentError of string * sourcespan

exception TypeMismatch of sourcespan typ * sourcespan typ * sourcespan