open Batteries

type t = (string * int)

let create _ = BatMap.empty

let get k m = 
  try Some (BatMap.find k m)
  with Not_found -> None

let set k v m = BatMap.add k v m

let to_string (idname,_)= idname