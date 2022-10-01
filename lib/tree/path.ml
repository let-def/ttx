type t =
    Pident of Ident.t
  | Pdot of t * string
  | Papply of t * t

type typath =
  | Regular of t
  | Ext of t * string
  | LocalExt of Ident.t
  | Cstr of t * string
