open Printf
type t =int 
let incridex = 
  let n = ref (-1) in function () -> incr n; !n

let to_string = sprintf "#%d"