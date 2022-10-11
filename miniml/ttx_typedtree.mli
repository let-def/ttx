open Ttx_def
module Ty = Ttx_types

type core_type
type pattern
type expression
(*type binding_op*)
type case
type constructor
type extension_constructor
type functor_parameter
type include_decl
type include_desc
type label
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
type signature
type signature_item
type structure
type structure_item
type type_decl
(*type type_exception*)
(*type type_extension*)
type value_binding
type value_desc
type with_constraint
type implementation

type 'ns lpath = {
  path: 'ns path;
  name: longident loc;
}
val make_lpath : 'ns path -> longident loc -> 'ns lpath

module Core_type : sig
  type t = core_type
  type desc =
    | Any
    | Var of string
    | Arrow of arg_label * core_type * core_type
    | Tuple of core_type list
    | Constr of {
        params: core_type list;
        path: ns_type lpath;
      }
    | Alias of core_type * string

  val make : Ty.type_expr -> desc -> t
  val desc : t -> desc
  val typ : t -> Ty.type_expr
end

module Pattern : sig
  type t = pattern

  type record_field =
    | Field of {
        name: longident;
        label: Ty.label;
        pattern: pattern;
      }

  type desc =
    | Any
    | Var of Ident.t * string loc
    | Unpack of Ident.t * string loc
    | Alias of pattern * Ident.t * string loc
    | Constant of constant
    | Tuple of pattern list
    | Construct of {
        name: longident loc;
        def: Ty.constructor;
        exists: ns_type binder list;
        arguments: pattern list;
        typ: core_type option;
      }
    | Record of {
        fields: record_field list;
        closed: closed_flag;
      }
    | Array of pattern list
    | Lazy of pattern
    | Or of pattern * pattern
    | Constraint of {
        body: pattern;
        typ: core_type;
      }
    | Open of {
        path: ns_module lpath;
        body: pattern;
      }
    | Exception of pattern

  val make : Ty.type_expr -> desc -> t
  val desc : t -> desc
  val typ : t -> Ty.type_expr
end

module Expression : sig
  type t = expression

  type record_field =
    | Kept of Ty.type_expr
    | Overridden of longident loc * expression

  type desc =
    | Ident of ns_value lpath * Ty.value_desc
    | Constant of constant
    | Let of {
        rec_: rec_flag;
        bindings: value_binding list;
        body: expression;
      }
    | Function of {
        arg_label : arg_label;
        param : ns_value binder;
        cases : case list;
        partial : partial;
      }
    | Apply of {
        lhs: expression;
        rhs: (arg_label * expression option) list;
      }
    | Match of {
        scrutinee: expression;
        cases: case list;
        partial: partial;
      }
    | Try of {
        scrutinee: expression;
        cases: case list;
      }
    | Tuple of expression list
    | Construct of {
        constructor: Ty.constructor;
        arguments: expression list;
        name: longident loc;
      }
    | Variant of {
        label: label;
        argument: expression option;
      }
    | Record of {
        fields : (label_desc * record_field) vector;
        (*representation : Ty.record_representation;*)
        extended_expression : expression option;
      }
    | Field of {
        record: expression;
        name: longident loc;
        label: label_desc;
      }
    | Set_field of {
        record: expression;
        name: longident loc;
        label: label_desc;
        value: expression;
      }
    | Array of expression list
    | If_then_else of {
        cond: expression;
        then_: expression;
        else_: expression option;
      }
    | Sequence of expression * expression
    | While of {
        cond: expression;
        body: expression;
      }
    | For of {
        binder: ns_value binder;
        pattern: pattern;
        lobound: expression;
        hibound: expression;
        direction: direction_flag;
        body: expression;
      }
    | Let_module of {
        binder: ns_value binder;
        name: string option loc;
        presence: module_presence;
        expr: module_expr;
        body: expression;
      }
    | Let_exception of {
        constructor: extension_constructor;
        body: expression;
      }
    | Assert of expression
    | Lazy of expression
    | Pack of module_expr
    (*| Let_op of { let_: binding_op; ands: binding_op list;
                    param: Ident.t; body: case; partial: partial; } *)
    | Unreachable
    (*| Extension_constructor of longident loc * Path.t*)
    | Open of {
        decl: open_decl;
        body: expression;
      }
    | Constraint of {
        body: expression;
        typ: core_type;
      }
    | Coerce of {
        body: expression;
        typ: core_type option;
        subtyp: core_type;
      }
    | Newtype of {
        name: string;
        body: expression;
      }

  val make : Ty.type_expr -> desc -> t
  val desc : t -> desc
  val typ : t -> Ty.type_expr
end

module Case : sig
  type t = case
  val make : pattern -> guard:expression option -> expression -> case
  val lhs : t -> pattern
  val guard : t -> expression option
  val rhs : t -> expression
end

(*module Binding_op : sig
  type t = binding_op
  val path : t -> ns_value path
  val name : t -> string loc
  val value : t -> Ty.value_desc
  val typ : t -> Ty.type_expr
  val exp : t -> expression
  val loc : t -> location
end*)

module Module_expr : sig
  type t = module_expr
  type desc =
    | Ident of ns_module lpath
    | Structure of structure
    | Functor of functor_parameter * module_expr
    | Apply of module_expr * module_expr * module_coercion
    | Constraint of {
        expr: module_expr;
        typ: Ty.module_type;
        constr: module_type_constraint;
        coercion: module_coercion;
      }
    | Unpack of {
        expr: expression;
        typ: Ty.module_type;
      }

  val make : location -> attributes -> Ty.module_type -> desc -> t
  val desc : t -> desc
  val loc : t -> location
  val typ : t -> Ty.module_type
  val attrs : t -> attributes
end

module Functor_parameter : sig
  type t = functor_parameter
  type desc =
    | Unit
    | Named of module_decl
    | Anonymous of module_type

  val make : desc -> t
  val desc : t -> desc
end

module Module_type_constraint : sig
  type t = module_type_constraint
  type desc =
    | Implicit
    | Explicit of Ty.module_type
  val make : desc -> t
  val desc : t -> desc
end

module Structure : sig
  type t = structure
  val make : Ty.signature -> structure_item list -> t
  val items : t -> structure_item list
  val typ : t -> Ty.signature
end

module Structure_item : sig
  type t = structure_item

  type desc =
    | Eval of expression * attributes
    | Value of rec_flag * value_binding list
    | Primitive of value_desc
    | Type of rec_flag * type_decl list
    (*| Type_ext of type_extension*)
    (*| Exception of type_exception*)
    | Module of module_binding
    | Recmodule of module_binding list
    | Modtype of module_type_decl
    | Open of open_decl
    | Include of include_decl
    | Attribute of attribute

  val make : location -> desc -> t
  val desc : t -> desc
  val loc : t -> location
end

(*module Type_extension : sig
  type t = type_extension
  val path : t -> Path.t
  val txt : t -> longident loc
  val params : t -> (core_type * (variance * injectivity)) list
  val constructors : t -> extension_constructor list
  val privacy : t -> private_flag
  val loc : t -> location
  val attrs : t -> attributes
end

module Type_exception : sig
  type t = type_exception
  val constructor: t -> extension_constructor
  val loc: t -> location
  val attrs: t -> attributes
end*)

module Module_binding : sig
  type t = module_binding
  val make :
    location -> attributes -> ns_module binder -> string option loc ->
    module_presence -> module_expr -> t
  val binder : t -> ns_module binder
  val name : t -> string option loc
  val presence : t -> module_presence
  val expr : t -> module_expr
  val attrs : t -> attributes
  val loc : t -> location
end

module Value_binding : sig
  type t = value_binding
  val make : location -> attributes -> pattern -> expression -> t
  val pat: t -> pattern
  val expr: t -> expression
  val attrs: t -> attributes
  val loc: t -> location
end

module Module_coercion : sig
  type t = module_coercion
  type desc =
    | None
    | Structure of unit (*TODO*)
    (* (int * module_coercion) list * (Ident.t * int * module_coercion) list *)
    | Functor of {
        argument: module_coercion;
        result: module_coercion;
      }
    | Primitive of primitive_coercion
    | Alias of ns_module path * module_coercion

  val desc : t -> desc
end

module Module_type : sig
  type t = module_type
  type desc =
    | Ident of ns_module_type lpath
    | Signature of signature
    | Functor of functor_parameter * module_type
    | With of module_type * with_constraint list
    | Type_of of module_expr
    | Alias of ns_module lpath

  val desc : t -> desc
  val typ : t -> Ty.module_type
  val loc : t -> location
  val attrs : t -> attributes
end

module With_constraint : sig
  type t = with_constraint
  type desc =
    | Type of {
        lhs: ns_type lpath;
        rhs: type_decl;
        substitute: bool;
      }
    | Module of {
        lhs: ns_module lpath;
        rhs: ns_module lpath;
        substitute: bool;
      }
    | Modtype of {
        lhs: ns_module_type lpath;
        rhs: module_type;
        substitute: bool;
      }

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
    (*| Type_ext of type_extension*)
    (*| Exception of type_exception*)
    | Module of module_decl
    | Modsubst of module_substitution
    | Rec_module of module_decl list
    | Modtype of module_type_decl
    | Modtype_subst of module_type_decl
    | Open of open_desc
    | Include of include_desc
    | Attribute of attribute

  val make : location -> desc -> t
  val desc: t -> desc
  val loc: t -> location
end

module Module_decl : sig
  type t = module_decl
  val make :
    location -> attributes -> ns_module binder -> string option loc ->
    module_presence -> module_type -> t
  val ident : t -> ns_module binder
  val name : t -> string option loc
  val presence : t -> module_presence
  val typ : t -> module_type
  val attrs : t -> attributes
  val loc : t -> location
end

module Type_decl : sig
  type t = type_decl

  type kind =
    | Abstract
    | Variant of constructor list
    | Record of label list
    | Open

  type parameter = {
    typ: core_type;
    variance: variance;
    injectivity: injectivity;
  }

  type constraint_equation = {
    lhs: core_type;
    rhs: core_type;
    loc: location;
  }

  val make :
    location -> attributes -> ns_type binder -> string loc -> Ty.type_decl ->
    params:parameter list -> manifest:core_type option ->
    constraints:constraint_equation list ->
    private_flag -> kind ->
    t

  val id : t -> ns_type binder
  val name : t -> string loc
  val params : t -> (core_type * (variance * injectivity)) list
  val typ : t -> Ty.type_decl
  val constraints : t -> constraint_equation list
  val kind : t -> kind
  val privacy : t -> private_flag
  val manifest : t -> core_type option
  val loc : t -> location
  val attrs : t -> attributes
end

(*module Module_substitution : sig
  type t
  val id : t -> Ident.t
  val name : t -> string loc
  val manifest : t -> Path.t
  val txt : t -> longident loc
  val attrs: t -> attributes
  val loc : t -> location
end*)

module Module_type_decl : sig
  type t = module_type_decl
  val make :
    location -> attributes ->
    ns_module_type binder -> string loc -> module_type option -> t
  val binder : t -> ns_module_type binder
  val name : t -> string loc
  val typ : t -> module_type option
  val attrs : t -> attributes
  val loc : t -> location
end

module Open_desc : sig
  type t = open_desc
  val make :
    location -> attributes -> override_flag ->
    ns_module lpath -> Ty.signature -> t
  val path : t -> ns_module lpath
  val bound_items: t -> Ty.signature
  val override: t -> override_flag
  val attrs : t -> attributes
  val loc : t -> location
end

module Open_decl : sig
  type t = open_decl
  val make :
    location -> attributes -> override_flag ->
    module_expr -> Ty.signature -> t
  val desc : t -> module_expr
  val bound_items: t -> Ty.signature
  val override: t -> override_flag
  val attrs : t -> attributes
  val loc : t -> location
end

module Include_desc : sig
  type t
  val make : location -> attributes -> module_type -> Ty.signature -> t
  val desc : t -> module_type
  val typ : t -> Ty.signature
  val loc: t -> location
  val attrs: t -> attribute list
end

module Include_decl : sig
  type t
  val make : location -> attributes -> module_expr -> Ty.signature -> t
  val desc : t -> module_expr
  val typ : t -> Ty.signature
  val loc: t -> location
  val attrs: t -> attribute list
end

module Package_type : sig
  type t = package_type
  val make :
    ns_module_type lpath -> (longident loc * core_type) list ->
    Ty.module_type -> t
  val path : t -> ns_module_type lpath
  val constraints : t -> (longident loc * core_type) list
  val typ : t -> Ty.module_type
end

module Value_desc : sig
  type t = value_desc
  val make :
    location -> attributes -> ns_value binder -> string loc ->
    core_type -> Ty.value_desc
  val binder : t -> ns_value binder
  val name : t -> string loc
  val typ: t -> core_type
  val ty_desc: t -> Ty.value_desc
  val prim: t -> string list
  val loc: t -> location
  val attrs: t -> attributes
end

module Label : sig
  type t = label
  val id: t -> Ident.t
  val name: t -> string loc
  val mutability: t -> mutable_flag
  val typ: t -> core_type
  val loc: t -> location
  val attrs: t -> attributes
end

module Constructor : sig
  type t = constructor

  type arguments =
    | Tuple of core_type list
    | Record of label list

  val id: t -> Ident.t
  val name: t -> string loc
  val args: t -> arguments
  val res: t -> core_type option
  val loc: t -> location
  val attrs: t -> attributes
end

(*module Extension_constructor : sig
  type t = extension_constructor

  type kind =
    | Decl of Constructor_decl.arguments * core_type option
    | Rebind of Path.t * longident loc

  val id: t -> Ident.t
  val name: t -> string loc
  (*val typ : t -> Ty.extension_constructor*)
  val kind : t -> kind
  val loc : t -> location
  val attrs: t -> attributes
end*)

module Implementation : sig
  type t = implementation
  val structure : t -> structure
  val coercion  : t -> module_coercion
  val signature : t -> Ty.signature
end
