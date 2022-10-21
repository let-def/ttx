type ('a, 'b) eq = Refl : ('a, 'a) eq
type ('a, 'b) type_ordering = Lt | Eq : ('a, 'a) type_ordering | Gt

exception Error of string

module type TYPE = sig
  type 'a t
end

module type NAMESPACE = sig
  include TYPE
  val to_string : _ t -> string
  val order : 'a t -> 'b t -> ('a, 'b) type_ordering
end

module type CONTEXT = sig
  type 'a namespace
  type 'a binder
  type 'a name
  type 'a info

  type t
  val empty : t
  val enter : t -> 'a binder -> 'a info -> t
  val lookup : t -> 'a name -> 'a info
  val update : t -> 'a name -> 'a info -> t

  (* Creating new bindings *)

  val extend : t -> 'a namespace -> string -> 'a info -> t * 'a binder
  val reserve : t -> 'a namespace -> string -> t * 'a binder
end

module type S = sig
  type 'a namespace
  type 'a binder
  type 'a name

  val get_name : 'a binder -> 'a name
  val get_text : 'a name -> string
  val namespace : 'a name -> 'a namespace

  module Make_context(Info: TYPE) :
    CONTEXT with type 'a namespace := 'a namespace
             and type 'a binder := 'a binder
             and type 'a name := 'a name
             and type 'a info = 'a Info.t
end

module With_namespace(Namespace: NAMESPACE) :
  S with type 'a namespace = 'a Namespace.t
