open Exprs
open Types

exception DivideByZero of sourcespan
exception TypeMismatch of sourcespan typ * sourcespan typ * sourcespan