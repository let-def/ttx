type location = Location.t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

let location_none = Location.none

type 'a loc = 'a Location.loc = {
  txt: 'a;
  loc: location;
}

type constant =
  | Const_int of int
  | Const_char of char
  | Const_string of string * Location.t * string option
  | Const_float of string
  | Const_int32 of int32
  | Const_int64 of int64
  | Const_nativeint of nativeint

type rec_flag = Nonrecursive | Recursive

type direction_flag = Upto | Downto

type private_flag = Private | Public

type mutable_flag = Immutable | Mutable

type virtual_flag = Virtual | Concrete

type override_flag = Override | Fresh

type closed_flag = Closed | Open

type arg_label =
  | Nolabel
  | Labelled of string (*  label:T -> ... *)
  | Optional of string (* ?label:T -> ... *)

type variance =
  | Covariant
  | Contravariant
  | NoVariance

type injectivity =
  | Injective
  | NoInjectivity

type module_presence =
  | Present
  | Absent

type partial = Partial | Total

type attribute = Parsetree.attribute
type attributes = attribute list

type longident =
  | Lident of string
  | Ldot of longident * string
  | Lapply of longident * longident

type ns_value = private Ns_value
type ns_type = private Ns_type
type ns_type_level = private Ns_type_level
type ns_module = private Ns_module
type ns_module_type = private Ns_module_type

module Vector : sig
  type 'a t
  val empty : 'a t
  val of_list : 'a list -> 'a t
  val to_list : 'a t -> 'a list
  val of_array : 'a array -> 'a t
  val to_array : 'a array -> 'a t
  val unsafe_of_array : 'a array -> 'a t
  val unsafe_to_array : 'a array -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
  val iter : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
end = struct
  type 'a t = 'a array
  let empty = [||]
  let of_list = Array.of_list
  let to_list = Array.to_list
  let of_array = Array.copy
  let to_array = Array.copy
  let unsafe_of_array x = x
  let unsafe_to_array x = x
  let length = Array.length
  let get = Array.get
  let iter = Array.iter
  let map = Array.map
  let equal f a1 a2 =
    Array.length a1 = Array.length a2 &&
    Array.for_all2 f a1 a2
end

type 'a vector = 'a Vector.t

module Namespace = struct
  type 'a t =
    | Value : ns_value t
    | Type : ns_type t
    | Type_level : ns_type_level t
    | Module : ns_module t
    | Module_type : ns_module_type t
  let order (type a b) (a : a t) (b : b t) : (a, b) Context.type_ordering =
    match a, b with
    | Value       , Value       -> Eq
    | Type        , Type        -> Eq
    | Type_level  , Type_level  -> Eq
    | Module      , Module      -> Eq
    | Module_type , Module_type -> Eq
    | (Value|Type|Type_level|Module|Module_type), _ ->
      let c = compare (Obj.repr a) (Obj.repr b) in
      if c < 0 then Lt else Gt
  let to_string : type a. a t -> string = function
    | Value       -> "value"
    | Type        -> "type"
    | Type_level  -> "type variables"
    | Module      -> "module"
    | Module_type -> "module type"
end

include Context.With_namespace(Namespace)

module Path = struct
  type 'a t =
    | Ident : 'a name -> 'a t
    | Dot   : ns_module t * string -> 'a t
    (*| Apply : { lhs : ns_module t; rhs: ns_module t } -> ns_module t*)
end

type 'a path = 'a Path.t

module Type_level : sig
  type t
  type variable
  val make : ns_type_level binder -> t
  val freeze : t -> unit

  val fresh : t -> string option -> variable
  val level : variable -> ns_type_level name
  val index : variable -> int

  val binder : t -> ns_type_level binder
  val count : t -> int
  val get_var : t -> int -> variable
  val get_name : t -> int -> string option
end = struct
  type desc =
    | Open of int * string option list
    | Frozen of string option array

  type t = {
    binder: ns_type_level binder;
    mutable desc: desc;
  }

  type variable = ns_type_level name * int

  let make binder = {binder; desc = Open (0, [])}
  let freeze t =
    match t.desc with
    | Frozen _ -> invalid_arg "Type_level.freeze: level is already frozen"
    | Open (n, vars) ->
      let vars = Array.of_list (List.rev vars) in
      assert (Array.length vars = n);
      t.desc <- Frozen vars

  let fresh t name =
    match t.desc with
    | Frozen _ -> invalid_arg "Type_level.fresh: level is frozen"
    | Open (n, vars) ->
      t.desc <- Open (n + 1, name :: vars);
      (get_name t.binder, n)

  let level : variable -> ns_type_level name = fst
  let index : variable -> int = snd

  let binder t = t.binder

  let count t = match t.desc with
    | Frozen vars -> Array.length vars
    | Open _ -> invalid_arg "Type_level.count: level is not yet frozen"

  let get_var t n = match t.desc with
    | Open _ -> invalid_arg "Type_level.get_var: level is not yet frozen"
    | Frozen vars ->
      let l = Array.length vars in
      if n < 0 || n >= l then
        invalid_arg "Type_level.get_var: index out of bounds";
      (get_name t.binder, n)

  let get_name t n = match t.desc with
    | Open _ -> invalid_arg "Type_level.get_name: level is not yet frozen"
    | Frozen vars ->
      let l = Array.length vars in
      if n < 0 || n >= l then
        invalid_arg "Type_level.get_name: index out of bounds";
      vars.(n)
end
type type_level = Type_level.t
