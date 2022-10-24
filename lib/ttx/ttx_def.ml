type location = Location.t = {
  loc_start: Lexing.position;
  loc_end: Lexing.position;
  loc_ghost: bool;
}

class virtual ['self] location_iter = object (_ : 'self)
  method visit_location (_ : 'env) (_ : location) = ()
end

class virtual ['self] location_map = object (_ : 'self)
  method visit_location (_ : 'env) (x : location) = x
end

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

class virtual ['self] attributes_iter = object (_ : 'self)
  method visit_attributes (_ : 'env) (_ : attributes) = ()
end

class virtual ['self] attributes_map = object (_ : 'self)
  method visit_attributes (_ : 'env) (x : attributes) = x
end

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
  val to_array : 'a t -> 'a array
  val unsafe_of_array : 'a array -> 'a t
  val unsafe_to_array : 'a t -> 'a array
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

  class virtual ['self] visitor = object(_ : 'self)
    method visit_ns_value = Value
    method visit_ns_type = Type
    method visit_ns_type_level = Type_level
    method visit_ns_module = Module
    method visit_ns_module_type = Module_type
  end
end

include Context.With_namespace(Namespace)

class virtual ['self] binder_iter = object (_ : 'self)
  method visit_binder : 'ns. 'ns namespace -> 'env -> 'ns binder -> unit =
    fun _ _ _ -> ()
end

class virtual ['self] binder_map = object (_ : 'self)
  method visit_binder : 'ns. 'ns namespace -> 'env -> 'ns binder -> 'ns binder =
    fun _ _ x -> x
end

module Path = struct
  type 'a t =
    | Ident : 'a name -> 'a t
    | Dot   : ns_module t * string -> 'a t
    (*| Apply : { lhs : ns_module t; rhs: ns_module t } -> ns_module t*)
end

type 'a path = 'a Path.t

class virtual ['self] path_iter = object (_ : 'self)
  method visit_path : 'ns. 'ns namespace -> 'env -> 'ns path -> unit =
    fun _ _ _ -> ()
end

class virtual ['self] path_map = object (_ : 'self)
  method visit_path : 'ns. 'ns namespace -> 'env -> 'ns path -> 'ns path =
    fun _ _ x -> x
end

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

class virtual ['self] type_level_iter = object(self: 'self)
  method virtual visit_binder : 'ns. 'ns namespace -> 'env -> 'ns binder -> unit

  method visit_type_level : 'env -> type_level -> unit =
    fun env tl ->
    self#visit_binder Namespace.Type_level env (Type_level.binder tl)
end

class virtual ['self] type_level_map = object(self: 'self)
  method virtual visit_binder : 'ns. 'ns namespace -> 'env -> 'ns binder -> 'ns binder

  method visit_type_level : 'env -> type_level -> type_level =
    fun env tl ->
    let binder = Type_level.binder tl in
    let binder' = self#visit_binder Namespace.Type_level env binder in
    assert (binder == binder');
    tl
end

module Binding (*: sig
  type ('def, 'body) t
  val names : _ t -> namegroup
  val def : ('d, 'b) t -> 'd
  val body : ('d, 'b) t -> 'b

  val make : namegroup -> 'd -> 'b -> ('d, 'b) t
end*) = struct
  type ('def, 'body) t = {names: namegroup; def: 'def; body: 'body}
  let names t = t.names
  let def   t = t.def
  let body  t = t.body

  let make names def body = {names; def; body}

  class virtual ['self] iter = object(self: 'self)
    method virtual extend : 'env -> namegroup -> 'env

    method visit_binding
      : 'def 'body. ('env -> 'def -> unit) -> ('env -> 'body -> unit) ->
        'env -> ('def, 'body) t -> unit
      = fun visit_def visit_body env binding ->
        let env = self#extend env (names binding) in
        visit_def env (def binding);
        visit_body env (body binding)
  end

  class virtual ['self] map = object(self: 'self)
    method virtual extend : 'env -> namegroup -> 'env

    method visit_binding
      : 'def 'body. ('env -> 'def -> 'def) -> ('env -> 'body -> 'body) ->
        'env -> ('def, 'body) t -> ('def, 'body) t
      = fun visit_def visit_body env binding ->
        let env = self#extend env (names binding) in
        make (names binding)
          (visit_def env (def binding))
          (visit_body env (body binding))
  end
end
type ('def, 'body) binding = ('def, 'body) Binding.t

