(*符号表*)
open Batteries
(*key为string的map类型*)
type symbol = string * int
type 'a t = (symbol,'a) BatMap.t

let create _ = BatMap.empty


let get k m = 
  try Some (BatMap.find k m)
  with Not_found -> None

let set k v m = BatMap.add k v m

let to_string (idname,_)= idname