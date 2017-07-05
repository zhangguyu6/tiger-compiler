open Batteries.Printf
module L =Location

type e =
  | SyntaxError of string
  | TypeError of string
  | TypeCheckError of string
  | NameError of string
  | RuntimeError of string
and ex= e * L.t 

exception Error of (ex * string)

let string_of_ex (e:ex):string =
  match e with
  | SyntaxError s, loc ->(sprintf "SyntaxError %s at %s" s (L.string_of_loc loc))
  | TypeError s, loc -> (sprintf "TypeError %s at %s" s (L.string_of_loc loc))
  | TypeCheckError s,loc -> (sprintf "TypeCheckError %s at %s" s (L.string_of_loc loc))
  | NameError s, loc -> (sprintf "NameError %s at %s" s (L.string_of_loc loc))
  | RuntimeError s, loc -> (sprintf "RuntimeError %s at %s" s (L.string_of_loc loc))


let raise_error (e:e) (loc:L.t)= 
  let ex = e,loc in
  let error = ex,(string_of_ex ex) in
  raise (Error error)


let raise_SyntaxError (msg:string) (loc:L.t)= 
  raise_error (SyntaxError msg) loc 

let raise_TypeError (msg:string) (loc:L.t)= 
  raise_error (TypeError msg) loc 

let raise_TypeCheckError (msg:string) (loc:L.t)= 
  raise_error (TypeCheckError msg) loc 

let raise_NameError (msg:string) (loc:L.t)= 
  raise_error (NameError msg) loc 

let raise_RuntimeError (msg:string) (loc:L.t)= 
  raise_error (RuntimeError msg) loc 