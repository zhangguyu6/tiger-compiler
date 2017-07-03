type t = {
  startpos : Lexing.position;
  endpos : Lexing.position;
}

type 'a info = {
  value : 'a;
  loc : t;
}

let extract_value {value;loc} = value  