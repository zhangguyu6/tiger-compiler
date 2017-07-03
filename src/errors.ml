open Batteries
open BatPrintf
module L =Location

type t =
  | SyntaxError 
  | TypeError
  | NameError
  | RuntimeError

exception Error of t * L.t *string

let raise_error err loc msg = 
  raise @@ Error (err,loc,msg)

let raise_SyntaxError = raise_error SyntaxError
let raise_TypeError = raise_error TypeError
let raise_NameError = raise_error NameError
let raise_RuntimeError = raise_error RuntimeError
