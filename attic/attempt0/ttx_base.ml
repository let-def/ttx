module type INDEXABLE = sig
  type t
  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int
end

module type PRINTABLE = sig
  include INDEXABLE
end

module Position : sig
  type t = Lexing.position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  include INDEXABLE with type t := t

  val filename : t -> string
  val line : t -> int
  val column : t -> int

  val offset : t -> int
  val line_offset : t -> int
end = struct
  type t = Lexing.position = {
    pos_fname : string;
    pos_lnum : int;
    pos_bol : int;
    pos_cnum : int;
  }

  let equal t1 t2 =
    t1 == t2 || (
      Int.equal t1.pos_cnum t2.pos_cnum &&
      String.equal t1.pos_fname t2.pos_fname &&
      Int.equal t1.pos_lnum t2.pos_lnum &&
      Int.equal t1.pos_bol t2.pos_bol
    )

  let compare t1 t2 =
    if t1 == t2 then 0 else
    let c = String.compare t1.pos_fname t2.pos_fname in
    if c <> 0 then c else
      let c = Int.compare t1.pos_cnum t2.pos_cnum in
      if c <> 0 then c else
        let c = Int.compare t1.pos_lnum t2.pos_lnum in
        if c <> 0 then c else
          Int.compare t1.pos_bol t2.pos_bol

  let hash = Hashtbl.hash

  let filename t = t.pos_fname
  let line t = t.pos_lnum
  let column t = t.pos_cnum - t.pos_bol

  let offset t = t.pos_cnum
  let line_offset t = t.pos_bol
end
type position = Position.t

module Compiler_location = Location
type compiler_location = Location.t
type 'a compiler_loc = 'a Location.loc

module Location : sig
  include INDEXABLE with type t = compiler_location

  val mk : ?ghost:bool -> position -> position -> t
  val startp : t -> position
  val endp : t -> position
  val is_ghost : t -> bool

  val none : t
  val is_none : t -> bool

  val of_compiler_loc : compiler_location -> t
  val to_compiler_loc : t -> compiler_location
end = struct
  type t = Location.t

  let mk ?ghost:(loc_ghost = false) loc_start loc_end =
    {Location. loc_ghost; loc_start; loc_end}

  let startp t = t.Location.loc_start
  let endp t = t.Location.loc_end
  let is_ghost t = t.Location.loc_ghost

  let equal (t1 : Location.t) (t2 : Location.t) =
    t1 == t2 || (
      t1.loc_ghost = t2.loc_ghost &&
      Position.equal t1.loc_start t2.loc_start &&
      Position.equal t1.loc_end t2.loc_end
    )

  let compare (t1 : Location.t) (t2 : Location.t) =
    if t1 == t2 then 0 else
      let c = Position.compare t1.loc_start t2.loc_start in
      if c <> 0 then c else
        let c = Position.compare t1.loc_end t2.loc_end in
        if c <> 0 then c else
          Bool.compare t1.loc_ghost t2.loc_ghost

  let hash = Hashtbl.hash

  let none = Location.none
  let is_none t = equal t none

  let of_compiler_loc x = x
  let to_compiler_loc x = x
end
type location = Location.t

module Located : sig
  type 'a t
  val mk : ?loc:location -> 'a -> 'a t
  val value : 'a t -> 'a
  val location : 'a t -> location
end = struct
  type 'a t = 'a compiler_loc

  let mk ?loc x =
    match loc with
    | None -> Compiler_location.mknoloc x
    | Some loc -> Compiler_location.mkloc x loc

  let value t = t.Compiler_location.txt
  let location t = t.Compiler_location.loc
end

module Ident : sig
  include INDEXABLE

  val mk : string -> t
  val name : t -> string
  val id : t -> int
end = struct
  type t = {
    id: int;
    name : string;
  }

  let gensym = ref 0

  let mk name = incr gensym; {name; id = !gensym}
  let name t = t.name
  let id t = t.id

  let equal t1 t2 =
    t1 == t2 || (
      Int.equal t1.id t2.id &&
      String.equal t2.name t2.name
    )

  let compare t1 t2 =
    if t1 == t2 then 0 else
      let c = Int.compare t1.id t2.id in
      if c <> 0 then c else
        String.compare t1.name t2.name

  let hash t = t.id
end

type ident = Ident.t

module Path : sig
  include INDEXABLE

  type view =
    | Pident of ident
    | Pdot of t * string

  val view : t -> view

  val mk_root : ident -> t
  val mk_dot : t -> string -> t
end = struct
  type t = view

  and view =
    | Pident of ident
    | Pdot of t * string

  let view x = x

  let mk_root ident = Pident ident
  let mk_dot path dot = Pdot (path, dot)

  let rec equal t1 t2 =
    t1 == t2 ||
    match t1, t2 with
    | Pident id1, Pident id2 ->
      Ident.equal id1 id2
    | Pdot (p1, s1), Pdot (p2, s2) ->
      String.equal s1 s2 && equal p1 p2
    | Pident _, Pdot _
    | Pdot _, Pident _ -> false

  let rec compare t1 t2 =
    if t1 == t2 then 0 else
      match t1, t2 with
      | Pident _, Pdot _ -> 1
      | Pdot _, Pident _ -> -1
      | Pident id1, Pident id2 -> Ident.compare id1 id2
      | Pdot (p1, s1), Pdot (p2, s2) ->
        let c = String.compare s1 s2 in
        if c <> 0 then c else
          compare p1 p2

  let hash = Hashtbl.hash
end
type path = Path.t
