type type_expr
type type_scheme
type value_desc
type constructor
type label
type type_decl
type functor_parameter
type module_type
type module_decl
type module_type_decl
type signature_item
type signature
open Ttx_def

module Type_expr : sig
  type t = type_expr

  type desc =
    | Var of Type_level.variable
    | Arrow of {
        lhs: type_expr;
        rhs: type_expr;
      }
    | Tuple of type_expr list
    | Const of type_expr list * ns_type path

  val desc : t -> desc

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val make : desc -> t

  val make_undefined : unit -> t
  val define : t -> desc -> unit
  exception Already_defined
  exception Undefined
end

module Type_scheme : sig
  type t = type_scheme

  val forall : t -> type_level
  val expr : t -> type_expr
  val make : type_level -> type_expr -> t
end

module Value_desc : sig
  type t = value_desc

  type desc =
    | Regular
    | Primitive

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_value binder
  val typ : t -> type_scheme
  val desc : t -> desc
  val make : location -> attributes -> ns_value binder -> type_scheme -> desc -> t
end

module Constructor : sig
  type t = constructor

  type path = {
    typ: ns_type Path.t;
    index: int;
    name: string;
  }

  type arguments =
    | Tuple of type_expr vector
    | Record of label vector

  val loc : t -> location
  val attrs : t -> attributes
  val path : t -> path
  val forall : t -> type_level
  val arguments : t -> arguments
  val result : t -> type_expr
  val make : location -> attributes -> path -> type_level -> arguments -> type_expr -> t
end

module Label : sig
  type t = label

  type kind =
    | Record of ns_type Path.t
    | Inline_record of Constructor.path

  type path = {
    kind: kind;
    index: int;
    name: string;
  }

  val loc : t -> location
  val attrs : t -> attributes
  val path : t -> path
  val forall : t -> type_level
  val mutability : t -> mutable_flag
  val record : t -> type_expr
  val field : t -> type_expr
  val make : location -> attributes -> path -> type_level -> mutable_flag -> type_expr -> type_expr -> t
end

module Type_decl : sig
  type t = type_decl

  type desc =
    | Abstract
    | Record of label list
    | Variant of constructor list
    | Open

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_type binder
  val forall : t -> type_level
  val params : t -> type_expr list
  val manifest : t -> type_expr option
  val desc : t -> desc
  val make : location -> attributes -> ns_type binder -> type_level -> type_expr list -> type_expr option -> desc -> t
end

module Functor_parameter : sig
  type t = functor_parameter

  type desc =
    | Unit
    | Named of module_decl
    | Anonymous of module_type

  val desc : t -> desc
  val make : desc -> t
end

module Module_type : sig
  type t = module_type

  type desc =
    | Ident of ns_module_type path
    | Signature of signature
    | Functor of functor_parameter * module_type
    | Alias of ns_module path

  val desc : t -> desc
  val make : desc -> t
end

module Module_decl : sig
  type t = module_decl

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_module binder
  val typ : t -> module_type
  val make : location -> attributes -> ns_module binder -> module_type -> t
end

module Module_type_decl : sig
  type t = module_type_decl

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_module_type binder
  val typ : t -> module_type option
  val make : location -> attributes -> ns_module_type binder -> module_type option -> t
end

module Signature_item : sig
  type t = signature_item

  type visibility =
    | Exported
    | Hidden

  type desc =
    | Value of value_desc
    | Type of rec_flag * type_decl list
    | Module of rec_flag * module_decl list
    | Module_type of module_type_decl

  val visibility : t -> visibility
  val desc : t -> desc
  val make : visibility -> desc -> t
end

module Signature : sig
  type t = signature

  val items : t -> signature_item list
  val make : signature_item list -> t
end

module Visitor : sig
  type 'a category =
    | Type_expr : type_expr category
    | Type_scheme : type_scheme category
    | Value_desc : value_desc category
    | Constructor : constructor category
    | Label : label category
    | Type_decl : type_decl category
    | Functor_parameter : functor_parameter category
    | Module_type : module_type category
    | Module_decl : module_decl category
    | Module_type_decl : module_type_decl category
    | Signature_item : signature_item category
    | Signature : signature category
  type 'a iter = {iter: 'b. 'a iter -> 'a -> 'b category -> 'b -> unit} [@@ocaml.unboxed]
  val iter: 'a iter
end
