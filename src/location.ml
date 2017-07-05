open Batteries.Printf
type t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
}

type 'a loc = {
  value : 'a;
  loc : t;
}

let extract_value {value;loc} = value  
let extract_loc {value;loc} = loc

(*打印pos*)
let string_of_loc (loc:t) :string =
  let startline = loc.startpos.pos_lnum in
  let startcol = loc.startpos.pos_bol - loc.startpos.pos_cnum in
  let endline = loc.endpos.pos_lnum in
  let endcol = loc.endpos.pos_bol - loc.endpos.pos_cnum in
  sprintf "start %d:%d end %d:%d \n " startline startcol endline endcol