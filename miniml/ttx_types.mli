open Ttx_def

type ns_value = private Ns_value
type ns_type = private Ns_type
type ns_type_level = private Ns_type_level
type ns_module = private Ns_module
type ns_module_type = private Ns_module_type

module Vector : sig
  type 'a t
  val of_array : 'a array -> 'a t
  val to_array : 'a array -> 'a t
  val unsafe_of_array : 'a array -> 'a t
  val unsafe_to_array : 'a array -> 'a t
  val length : 'a t -> int
  val get : 'a t -> int -> 'a
end
type 'a vector = 'a Vector.t

module Namespace : sig
  type 'a t =
    | Value : ns_value t
    | Type : ns_type t
    | Type_level : ns_type_level t
    | Module : ns_module t
    | Module_type : ns_module_type t
  val to_string : 'a t -> string
  val order : 'a t -> 'b t -> ('a, 'b) Context.type_ordering
end

include Context.S with type 'a namespace = 'a Namespace.t

module Path : sig
  type 'a t =
    | Ident : 'a name -> 'a t
    | Dot   : ns_module t * string -> 'a t
    (*| Apply : { lhs : ns_module t; rhs: ns_module t } -> ns_module t*)
end

type 'a path = 'a Path.t

type type_expr
type type_level
type type_scheme
type constructor
type label_decl
type type_decl
type value_desc
type module_type
type functor_parameter
type module_decl
type module_type_decl
type signature
type signature_item

module Type_level : sig
  type t = type_level
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
end

module Type_expr : sig
  type t = type_expr
  type desc =
    | Var of Type_level.variable
    | Arrow of { lhs: t; rhs: t; }
    | Tuple of t list
    | Const of t list * ns_type path

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val make : desc -> t
  val desc : t -> desc

  val make_undefined : unit -> t
  val define : t -> desc -> unit
  exception Already_defined
  exception Undefined
end

module Type_scheme : sig
  type t = type_scheme
  val make : type_level -> type_expr -> t
  val forall : t -> type_level
  val expr : t -> type_expr
end

module Value_desc : sig
  type t = value_desc
  type desc =
    | Regular
    | Primitive

  val name : t -> string
  val typ : t -> type_scheme
  val desc : t -> desc

  val make : string -> type_scheme -> desc -> t
end

module Constructor : sig
  type t = constructor

  type nonrec path = {
    typ: ns_type path;
    index: int;
    name: string;
  }

  type arguments =
    | Tuple of type_expr vector
    | Record of label_decl vector

  val make : path -> forall:type_level -> arguments -> type_expr -> t
  val name : t -> string
  val forall : t -> type_level
  val arguments : t -> arguments
  val result : t -> type_expr
end

module Label_decl : sig
  type t = label_decl

  type nonrec path =
    | Of_record of {
        typ: ns_type path;
        index: int;
        name: string;
      }
    | Of_inline_record of {
        cstr: Constructor.path;
        index: int;
        name: string;
      }

  val make : path -> mutable_flag -> forall:type_level -> record:type_expr -> field:type_expr -> t
  val name : t -> string
  val forall : t -> type_level
  val mutability : t -> mutable_flag
  val record : t -> type_expr
  val field : t -> type_expr
end

module Type_decl : sig
  type t = type_decl

  type desc =
    | Abstract
    | Record of label_decl list
    | Variant of constructor list
    | Open

  val make :
    string ->
    forall:type_level ->
    params:type_expr list ->
    manifest:type_expr option ->
    desc -> t

  val name : t -> string
  val forall : t -> type_level
  val params : t -> type_expr list
  val manifest : t -> type_expr option
  val desc : t -> desc
end

module Functor_parameter : sig
  type t = functor_parameter
  type desc =
    | Unit
    | Named of ns_module binder option * module_type

  val make : desc -> t
  val desc : t -> desc
end

module Module_type : sig
  type t = module_type
  type desc =
    | Ident of ns_module_type path
    | Signature of signature
    | Functor of functor_parameter * module_type
    | Alias of ns_module path

  val make : desc -> t
  val desc : t -> desc
end

module Module_decl : sig
  type t = module_decl
  val name : t -> string
  val make : string -> module_type -> t
  val typ : t -> module_type
end

module Module_type_decl : sig
  type t = module_type_decl
  val make : module_type option -> t
  val typ : t -> module_type option
end

module Signature_item : sig
  type t = signature_item

  type visibility =
    | Exported
    | Hidden

  type desc =
    | Value of ns_value binder * value_desc
    | Type of rec_flag * (ns_type binder * type_decl) list
    | Module of rec_flag * (ns_module binder * module_decl) list
    | Module_type of ns_module_type binder * module_type_decl

  val desc : t -> desc
  val visibility : t -> visibility
  val make : visibility -> desc -> t
end

module Signature : sig
  type t = signature
  val make : Signature_item.t list -> t
  val items : t -> Signature_item.t list
end

module Visitor : sig
  type 'a category =
    | Type_expr   : type_expr category
    | Type_level  : type_level category
    | Type_scheme : type_scheme category
    | Constructor : constructor category
    | Label_decl  : label_decl category
    | Type_decl   : type_decl category
    | Value_desc  : value_desc category
    | Module_type : module_type category
    | Functor_parameter : functor_parameter category
    | Module_decl : module_decl category
    | Module_type_decl : module_type_decl category
    | Signature   : signature category
    | Signature_item : signature_item category

  type 'a decl =
    | Value       : value_desc -> ns_value decl
    | Type        : type_decl -> ns_type decl
    | Type_level  : type_level -> ns_type_level decl
    | Module      : module_decl -> ns_module decl
    | Import      : string * Digest.t -> ns_module decl
    | Module_type : module_type_decl -> ns_module_type decl

  val namespace : 'a decl -> 'a namespace
  (*let namespace (type a) : a decl -> a namespace = function
    | Value  _ -> Value
    | Type   _ -> Type
    | Module _ -> Module
    | Import _ -> Module*)

  type 'env bind = { bind: 'a. 'a binder -> 'a decl -> 'env -> 'env }
  val enter : 'env bind -> 'a category -> 'a -> 'env -> 'env

  type 'env iter = { iter: 'a. 'env iter -> 'env -> 'a category -> 'a -> unit }
  val iter : 'env iter -> 'env -> 'a category -> 'a -> unit

  type 'env map = { map: 'a. 'env map -> 'env -> 'a category -> 'a -> 'a }
  val map : 'env map -> 'env -> 'a category -> 'a -> 'a
end
