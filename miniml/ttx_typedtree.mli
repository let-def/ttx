open Ttx_def
module Ty = Ttx_types

type core_type
type pattern
type expression
type binding_op
type case
type constructor_decl
type extension_constructor
type functor_parameter
type include_decl
type include_desc
type label_decl
type label_desc
type module_binding
type module_coercion
type module_decl
type module_expr
type module_substitution
type module_type
type module_type_constraint
type module_type_decl
type open_decl
type open_desc
type primitive_coercion
type package_type
type record_label_def
type signature
type signature_item
type structure
type structure_item
type type_decl
type type_exception
type type_extension
type value_binding
type value_desc
type with_constraint
type implementation

module Core_type : sig
  type t = core_type
  type desc =
    | Any
    | Var of string
    | Arrow of arg_label * core_type * core_type
    | Tuple of core_type list
    | Constr of ns_constructor path * Longident.t loc * core_type list
    | Alias of core_type * string

  val desc : t -> desc
  val typ : t -> Ty.type_expr
end

module Pattern : sig
  type t = pattern

  type desc =
    | Any
    | Var of Ident.t * string loc
    | Unpack of Ident.t * string loc
    | Alias of pattern * Ident.t * string loc
    | Constant of constant
    | Tuple of pattern list
    | Construct of
        Longident.t loc * Ty.constructor *
        pattern list * (Ident.t loc list * core_type) option
    | Record of
        (Longident.t loc * label_desc * pattern) list *
        closed_flag
    | Array of pattern list
    | Lazy of pattern
    | Or of pattern * pattern
    | Constraint of pattern * core_type
    | Open of ns_module path * Longident.t loc * pattern
    | Expression of pattern

  val desc : t -> desc
  val typ : t -> Ty.type_expr
end

module Expression : sig
  type t = expression

  type desc =
    | Ident of ns_value path * Longident.t loc * Ty.value_desc
    | Constant of constant
    | Let of rec_flag * value_binding list * expression
    | Function of {
        arg_label : arg_label;
        param : ns_value binder;
        cases : case list;
        partial : partial;
      }
    | Apply of expression * (arg_label * expression option) list
    | Match of expression * case list * partial
    | Try of expression * case list
    | Tuple of expression list
    | Construct of
        Longident.t loc * Ty.constructor * expression list
    | Variant of label * expression option
    | Record of {
        fields : ( label_desc * record_label_def ) array;
        (*representation : Ty.record_representation;*)
        extended_expression : expression option;
      }
    | Field of expression * longident loc * label_desc
    | Set_field of expression * longident loc * label_desc * expression
    | Array of expression list
    | If_then_else of expression * expression * expression option
    | Sequence of expression * expression
    | While of expression * expression
    | For of
        Ident.t * Parsetree.pattern * expression * expression * direction_flag *
        expression
    | Let_module of
        Ident.t option * string option loc * module_presence * module_expr *
        expression
    | Let_exception of extension_constructor * expression
    | Assert of expression
    | Lazy of expression
    | Pack of module_expr
    | Let_op of {
        let_ : binding_op;
        ands : binding_op list;
        param : Ident.t;
        body : case;
        partial : partial;
      }
    | Unreachable
    | Extension_constructor of Longident.t loc * Path.t
    | Open of open_decl * expression
    | Constraint of expression * core_type
    | Coerce of expression * core_type option * core_type
    | Newtype of string * expression

  val desc : t -> desc
  val typ : t -> Ty.type_expr
end

module Case : sig
  type t
  val lhs : t -> pattern
  val rhs : t -> pattern
  val guard : t -> expression option
end

module Record_label_def : sig
  type t = record_label_def

  type desc =
    | Kept of Ty.type_expr
    | Overridden of Longident.t loc * expression

  val desc : t -> desc
end

module Binding_op : sig
  type t = binding_op
  val path : t -> ns_value path
  val name : t -> string loc
  val value : t -> Ty.value_desc
  val typ : t -> Ty.type_expr
  val exp : t -> expression
  val loc : t -> location
end

module Module_expr : sig
  type t = module_expr

  type desc =
    | Ident of Path.t * Longident.t loc
    | Structure of structure
    | Functor of functor_parameter * module_expr
    | Apply of module_expr * module_expr * module_coercion
    | Constraint of
        module_expr * Ty.module_type * module_type_constraint * module_coercion
    | Unpack of expression * Ty.module_type

  val typ : t -> Ty.module_type

  val desc : t -> desc
  val loc : t -> location
  val typ : t -> Ty.module_type
  val attributes : t -> attributes
end

module Module_type_constraint : sig
  type t
  type desc =
    | Implicit
    | Explicit of Ty.module_type
  val desc : t -> desc
end

module Functor_parameter : sig
  type t

  type desc =
    | Unit
    | Named of ns_module binder option * string option loc * module_type

  val desc : t -> desc
end

module Structure : sig
  type t
  val items : t -> structure_item list
  val typ : t -> Ty.signature
end

module Structure_item : sig
  type t

  type desc =
    | Eval of expression * attributes
    | Value of rec_flag * value_binding list
    | Primitive of value_desc
    | Type of rec_flag * type_decl list
    | Type_ext of type_extension
    | Exception of type_exception
    | Module of module_binding
    | Recmodule of module_binding list
    | Modtype of module_type_decl
    | Open of open_decl
    | Include of include_decl
    | Attribute of attribute

  val desc : t -> desc
  val loc : t -> location
end

module Type_extension : sig
  type t = type_extension
  val path : t -> Path.t
  val txt : t -> longident loc
  val params : t -> (core_type * (variance * injectivity)) list
  val constructors : t -> extension_constructor list
  val privacy : t -> private_flag
  val loc : t -> location
  val attributes : t -> attributes
end

module Type_exception : sig
  type t = type_exception
  val constructor: t -> extension_constructor
  val loc: t -> location
  val attributes: t -> attributes
end

module Module_binding : sig
  type t = module_binding
  val id : t -> Ident.t option
  val name : t -> string option loc
  val presence : t -> module_presence
  val expr : t -> module_expr
  val attributes : t -> attributes
  val loc : t -> location
end

module Value_binding : sig
  type t = value_binding
  val pat: t -> pattern
  val expr: t -> expression
  val attributes: t -> attributes
  val loc: t -> location
end

module Module_coercion : sig
  type t = module_coercion

  type desc =
    | None
    | Structure of (int * module_coercion) list *
                           (Ident.t * int * module_coercion) list
    | Functor of module_coercion * module_coercion
    | Primitive of primitive_coercion
    | Alias of Path.t * module_coercion

  val desc : t -> desc
end

module Module_type : sig
  type t = module_type
  type desc =
    | Ident of Path.t * Longident.t loc
    | Signature of signature
    | Functor of functor_parameter * module_type
    | With of module_type * (Path.t * Longident.t loc * with_constraint) list
    | Type_of of module_expr
    | Alias of Path.t * Longident.t loc
  val desc : t -> desc
  val typ : t -> Ty.module_type
  val loc : t -> location
  val attributes : t -> attributes
end

module With_constraint : sig
  type t = with_constraint

  type desc =
    | Type of type_decl
    | Module of Path.t * Longident.t loc
    | Modtype of module_type
    | Type_subst of type_decl
    | Mod_subst of Path.t * Longident.t loc
    | Modtype_subst of module_type

  val desc : t -> desc
end

module Primitive_coercion : sig
  type t = primitive_coercion
  val typ : t -> Ty.type_expr
  val loc : t -> location
end

module Signature : sig
  type t = signature
  val items : t -> signature_item list
  val typ : t -> Ty.signature
end

module Signature_item : sig
  type t = signature_item
  type desc =
    | Value of value_desc
    | Type of rec_flag * type_decl list
    | Type_subst of type_decl list
    | Type_ext of type_extension
    | Exception of type_exception
    | Module of module_decl
    | Modsubst of module_substitution
    | Rec_module of module_decl list
    | Modtype of module_type_decl
    | Modtype_subst of module_type_decl
    | Open of open_desc
    | Include of include_desc
    | Attribute of attribute
  val desc: t -> desc
  val loc: t -> location
end

module Module_decl : sig
  type t = module_decl
  val ident : t -> Ident.t option
  val name : t -> string option loc
  val presence : t -> module_presence
  val typ : t -> module_type
  val attributes : t -> attributes
  val loc : t -> location
end

module Type_decl : sig
  type t = type_decl

  type kind =
    | Abstract
    | Variant of constructor_decl list
    | Record of label_decl list
    | Open

  val id : t -> Ident.t
  val name : t -> string loc
  val params : t -> (core_type * (variance * injectivity)) list
  val typ : t -> Ty.type_decl
  val cstrs : t -> (core_type * core_type * location) list
  val kind : t -> kind
  val privacy : t -> private_flag
  val manifest : t -> core_type option
  val loc : t -> location
  val attributes : t -> attributes
end

module Module_substitution : sig
  type t
  val id : t -> Ident.t
  val name : t -> string loc
  val manifest : t -> Path.t
  val txt : t -> longident loc
  val attributes: t -> attributes
  val loc : t -> location
end

module Module_type_decl : sig
  type t = module_type_decl
  val id : t -> Ident.t
  val name : t -> string loc
  val typ : t -> module_type option
  val attributes : t -> attributes
  val loc : t -> location
end

module Open_desc : sig
  type t = open_desc
  val desc : t -> Path.t * longident loc
  val bound_items: t -> Ty.signature
  val override: t -> override_flag
  val attributes : t -> attributes
  val loc : t -> location
end

module Open_decl : sig
  type t = open_decl
  val desc : t -> module_expr
  val bound_items: t -> Ty.signature
  val override: t -> override_flag
  val attributes : t -> attributes
  val loc : t -> location
end

module Include_desc : sig
  type t
  val desc : t -> module_type
  val typ : t -> Ty.signature
  val loc: t -> location
  val attributes: t -> attribute list
end

module Include_decl : sig
  type t
  val desc : t -> module_expr
  val typ : t -> Ty.signature
  val loc: t -> location
  val attributes: t -> attribute list
end

module Package_type : sig
  type t = package_type
  val path : t -> Path.t
  val fields : t -> (Longident.t loc * core_type) list
  val typ : t -> Ty.module_type
  val txt : t -> longident loc
end

module Value_desc : sig
  type t = value_desc
  val id: t -> Ident.t
  val name: t -> string loc
  val typ: t -> core_type
  val ty_desc: t -> Ty.value_desc
  val prim: t -> string list
  val loc: t -> location
  val attributes: t -> attributes
end

module Label_decl : sig
  type t = label_decl
  val id: t -> Ident.t
  val name: t -> string loc
  val mutability: t -> mutable_flag
  val typ: t -> core_type
  val loc: t -> location
  val attributes: t -> attributes
end

module Constructor_decl : sig
  type t = constructor_decl

  type arguments =
    | Tuple of core_type list
    | Record of label_decl list

  val id: t -> Ident.t
  val name: t -> string loc
  val args: t -> arguments
  val res: t -> core_type option
  val loc: t -> location
  val attributes: t -> attributes
end

module Extension_constructor : sig
  type t = extension_constructor

  type kind =
    | Decl of Constructor_decl.arguments * core_type option
    | Rebind of Path.t * Longident.t loc

  val id: t -> Ident.t
  val name: t -> string loc
  (*val typ : t -> Ty.extension_constructor*)
  val kind : t -> kind
  val loc : t -> location
  val attributes: t -> attributes
end

module Implementation : sig
  type t = implementation
  val structure : t -> structure
  val coercion  : t -> module_coercion
  val signature : t -> Ty.signature
end
