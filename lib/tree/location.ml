type t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

type 'a loc = {
  txt : 'a;
  loc : t;
}
