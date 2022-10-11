open Ttx_def
module Ty = Ttx_types

type 'ns lpath = {
  path: 'ns path;
  name: longident loc;
}
let make_lpath path name = {path; name}

(*type core_type
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
type implementation*)

module Core_type = struct
  type t = {
    desc: desc;
    typ: Ty.type_expr;
  }

  and desc =
    | Any
    | Var of string
    | Arrow of arg_label * t * t
    | Tuple of t list
    | Constr of {
        params: t list;
        path: ns_type lpath;
      }
    | Alias of t * string

  let make typ desc = {typ; desc}
  let desc t = t.desc
  let typ t = t.typ
end

module Pattern = struct
  type t = {
    typ: Ty.type_scheme;
    desc: desc;
  }

  and record_field =
    | Field of {
        name: longident;
        label: Ty.label;
        pattern: t;
      }

  and desc =
    | Any
    | Var of Ident.t * string loc
    | Unpack of Ident.t * string loc
    | Alias of t * Ident.t * string loc
    | Constant of constant
    | Tuple of t list
    | Construct of {
        name: longident loc;
        def: Ty.constructor;
        exists: ns_type binder list;
        arguments: t list;
        typ: Core_type.t option;
      }
    | Record of {
        fields: record_field list;
        closed: closed_flag;
      }
    | Array of t list
    | Lazy of t
    | Or of t * t
    | Constraint of {
        body: t;
        typ: Core_type.t;
      }
    | Open of {
        path: ns_module lpath;
        body: t;
      }
    | Exception of t

  let make typ desc = {typ; desc}
  let desc t = t.desc
  let typ t = t.typ
end

module rec Expression : sig
  type t

  type record_field =
    | Kept of Ty.type_expr
    | Overridden of longident loc * t

  type desc =
    | Ident of ns_value lpath * Ty.value_desc
    | Constant of constant
    | Let of {
        rec_: rec_flag;
        bindings: Value_binding.t list;
        body: t;
      }
    | Function of {
        arg_label : arg_label;
        param : ns_value binder;
        cases : Case.t list;
        partial : partial;
      }
    | Apply of {
        lhs: t;
        rhs: (arg_label * t option) list;
      }
    | Match of {
        scrutinee: t;
        cases: Case.t list;
        partial: partial;
      }
    | Try of {
        scrutinee: t;
        cases: Case.t list;
      }
    | Tuple of t list
    | Construct of {
        constructor: Ty.constructor;
        arguments: t list;
        name: longident loc;
      }
    | Variant of {
        label: label;
        argument: t option;
      }
    | Record of {
        fields : (Label.t * record_field) vector;
        (*representation : Ty.record_representation;*)
        extended_expression : t option;
      }
    | Field of {
        record: t;
        name: longident loc;
        label: Label.t;
      }
    | Set_field of {
        record: t;
        name: longident loc;
        label: Label.t;
        value: t;
      }
    | Array of t list
    | If_then_else of {
        cond: t;
        then_: t;
        else_: t option;
      }
    | Sequence of t * t
    | While of {
        cond: t;
        body: t;
      }
    | For of {
        binder: ns_value binder;
        pattern: Pattern.t;
        lobound: t;
        hibound: t;
        direction: direction_flag;
        body: t;
      }
    | Let_module of {
        binder: ns_value binder;
        name: string option loc;
        presence: module_presence;
        expr: Module_expr.t;
        body: t;
      }
    | Let_exception of {
        constructor: extension_constructor;
        body: t;
      }
    | Assert of t
    | Lazy of t
    | Pack of Module_expr.t
    (*| Let_op of { let_: binding_op; ands: binding_op list;
                    param: Ident.t; body: case; partial: partial; } *)
    | Unreachable
    (*| Extension_constructor of longident loc * Path.t*)
    | Open of {
        decl: Open_decl.t;
        body: t;
      }
    | Constraint of {
        body: t;
        typ: Core_type.t;
      }
    | Coerce of {
        body: t;
        typ: Core_type.t option;
        subtyp: Core_type.t;
      }
    | Newtype of {
        name: string;
        body: t;
      }

  val make : Ty.type_expr -> desc -> t
  val desc : t -> desc
  val typ : t -> Ty.type_expr
end = struct
  type t = {typ: Ty.type_expr; desc: desc}

  and record_field =
    | Kept of Ty.type_expr
    | Overridden of longident loc * t

  and desc =
    | Ident of ns_value lpath * Ty.value_desc
    | Constant of constant
    | Let of {
        rec_: rec_flag;
        bindings: Value_binding.t list;
        body: t;
      }
    | Function of {
        arg_label : arg_label;
        param : ns_value binder;
        cases : Case.t list;
        partial : partial;
      }
    | Apply of {
        lhs: t;
        rhs: (arg_label * t option) list;
      }
    | Match of {
        scrutinee: t;
        cases: Case.t list;
        partial: partial;
      }
    | Try of {
        scrutinee: t;
        cases: Case.t list;
      }
    | Tuple of t list
    | Construct of {
        constructor: Ty.constructor;
        arguments: t list;
        name: longident loc;
      }
    | Variant of {
        label: label;
        argument: t option;
      }
    | Record of {
        fields : (Label.t * record_field) vector;
        (*representation : Ty.record_representation;*)
        extended_expression : t option;
      }
    | Field of {
        record: t;
        name: longident loc;
        label: Label.t;
      }
    | Set_field of {
        record: t;
        name: longident loc;
        label: Label.t;
        value: t;
      }
    | Array of t list
    | If_then_else of {
        cond: t;
        then_: t;
        else_: t option;
      }
    | Sequence of t * t
    | While of {
        cond: t;
        body: t;
      }
    | For of {
        binder: ns_value binder;
        pattern: Pattern.t;
        lobound: t;
        hibound: t;
        direction: direction_flag;
        body: t;
      }
    | Let_module of {
        binder: ns_value binder;
        name: string option loc;
        presence: module_presence;
        expr: Module_expr.t;
        body: t;
      }
    | Let_exception of {
        constructor: extension_constructor;
        body: t;
      }
    | Assert of t
    | Lazy of t
    | Pack of Module_expr.t
    (*| Let_op of { let_: binding_op; ands: binding_op list;
                    param: Ident.t; body: case; partial: partial; } *)
    | Unreachable
    (*| Extension_constructor of longident loc * Path.t*)
    | Open of {
        decl: Open_decl.t;
        body: t;
      }
    | Constraint of {
        body: t;
        typ: Core_type.t;
      }
    | Coerce of {
        body: t;
        typ: Core_type.t option;
        subtyp: Core_type.t;
      }
    | Newtype of {
        name: string;
        body: t;
      }

  let make typ desc = {typ; desc}
  let desc t = t.desc
  let typ t = t.typ
end

and Case : sig
  type t
  val make : Pattern.t -> guard:Expression.t option -> Expression.t -> Case.t
  val lhs : t -> Pattern.t
  val guard : t -> Expression.t option
  val rhs : t -> Expression.t
end = struct
  type t = {
    lhs: Pattern.t;
    guard: Expression.t option;
    rhs: Expression.t;
  }

  let make lhs ~guard rhs = {lhs; guard; rhs}
  let lhs t = t.lhs
  let guard t = t.guard
  let rhs t = t.rhs
end

and Value_binding : sig
  type t
  val make : location -> attributes -> Pattern.t -> Expression.t -> t
  val pat: t -> Pattern.t
  val expr: t -> Expression.t
  val attrs: t -> attributes
  val loc: t -> location
end = struct
  type t = {
    pat: Pattern.t;
    expr: Expression.t;
    loc: location;
    attrs: attributes;
  }

  let make loc attrs pat expr = {pat; expr; loc; attrs}
  let pat   t = t.pat
  let expr  t = t.expr
  let attrs t = t.attrs
  let loc   t = t.loc
end

and Label : sig
  type t
  val make: location -> attributes -> Ty.Label.path -> string loc -> mutable_flag -> Core_type.t -> t
  val path: t -> Ty.Label.path
  val name: t -> string loc
  val mutability: t -> mutable_flag
  val typ: t -> Core_type.t
  val loc: t -> location
  val attrs: t -> attributes
end = struct
  type t = {
    path: Ty.Label.path;
    name: string loc;
    mutability: mutable_flag;
    typ: Core_type.t;
    loc: location;
    attrs: attributes;
  }
  let make loc attrs path name mutability typ =
    {loc; attrs; path; name; mutability; typ}
  let path t = t.path
  let name t = t.name
  let mutability t = t.mutability
  let typ t = t.typ
  let loc t = t.loc
  let attrs t = t.attrs
end

and Constructor : sig
  type t

  type arguments =
    | Tuple of Core_type.t list
    | Record of Label.t list

  val make: location -> attributes -> Ty.Constructor.path -> string loc -> arguments -> Core_type.t option -> t
  val path: t -> Ty.Constructor.path
  val name: t -> string loc
  val args: t -> arguments
  val res: t -> Core_type.t option
  val loc: t -> location
  val attrs: t -> attributes
end = struct
  type arguments =
    | Tuple of Core_type.t list
    | Record of Label.t list

  type t = {
    path  : Ty.Constructor.path;
    name  : string loc;
    args  : arguments;
    res   : Core_type.t option;
    loc   : location;
    attrs : attributes;
  }

  let make loc attrs path name args res =
    {loc; attrs; path; name; args; res}

  let path t = t.path
  let name t = t.name
  let args t = t.args
  let res t = t.res
  let loc t = t.loc
  let attrs t = t.attrs
end

and Module_expr : sig
  type t
  type desc =
    | Ident of ns_module lpath
    | Structure of Structure.t
    | Functor of {
        param: Functor_parameter.t;
        body: Module_expr.t;
      }
    | Apply of {
        funct: Module_expr.t;
        arg: Module_expr.t;
        arg_coercion: Module_coercion.t;
      }
    | Constraint of {
        expr: Module_expr.t;
        typ: Ty.module_type;
        constr: Module_type_constraint.t;
        coercion: Module_coercion.t;
      }
    | Unpack of {
        expr: Expression.t;
        typ: Ty.module_type;
      }

  val make : location -> attributes -> Ty.module_type -> desc -> t
  val desc : t -> desc
  val loc : t -> location
  val typ : t -> Ty.module_type
  val attrs : t -> attributes
end = struct
  type desc =
    | Ident of ns_module lpath
    | Structure of Structure.t
    | Functor of {
        param: Functor_parameter.t;
        body: Module_expr.t;
      }
    | Apply of {
        funct: Module_expr.t;
        arg: Module_expr.t;
        arg_coercion: Module_coercion.t;
      }
    | Constraint of {
        expr: Module_expr.t;
        typ: Ty.module_type;
        constr: Module_type_constraint.t;
        coercion: Module_coercion.t;
      }
    | Unpack of {
        expr: Expression.t;
        typ: Ty.module_type;
      }

  type t = {
    typ: Ty.module_type;
    desc: desc;
    loc: location;
    attrs: attributes;
  }

  let make loc attrs typ desc =
    {loc; attrs; typ; desc}

  let desc  t = t.desc
  let loc   t = t.loc
  let typ   t = t.typ
  let attrs t = t.attrs
end

and Module_coercion : sig
  type t
  type desc =
    | None
    | Structure of unit (*TODO*)
    (* (int * module_coercion) list * (Ident.t * int * module_coercion) list *)
    | Functor of {
        argument: t;
        result: t;
      }
    | Primitive of Primitive_coercion.t
    | Alias of ns_module path * Module_coercion.t

  val make : desc -> t
  val desc : t -> desc
end = struct
  type t = desc
  and desc =
    | None
    | Structure of unit (*TODO*)
    (* (int * module_coercion) list * (Ident.t * int * module_coercion) list *)
    | Functor of {
        argument: t;
        result: t;
      }
    | Primitive of Primitive_coercion.t
    | Alias of ns_module path * Module_coercion.t

  let make x = x
  let desc x = x
end

and Primitive_coercion : sig
  type t
  val make : location -> Ty.type_expr -> t
  val typ : t -> Ty.type_expr
  val loc : t -> location
end = struct
  type t = {
    typ: Ty.type_expr;
    loc: location;
  }
  let make loc typ = {loc; typ}
  let typ t = t.typ
  let loc t = t.loc
end

and Module_type_constraint : sig
  type t
  type desc =
    | Implicit
    | Explicit of Ty.module_type
  val make : desc -> t
  val desc : t -> desc
end = struct

end

and Functor_parameter : sig
  type t
  type desc =
    | Unit
    | Named of Module_decl.t
    | Anonymous of Module_type.t

  val make : desc -> t
  val desc : t -> desc
end = struct

end

and Open_decl : sig
  type t
  val make :
    location -> attributes -> override_flag ->
    Module_expr.t -> Ty.signature -> t
  val desc : t -> Module_expr.t
  val bound_items: t -> Ty.signature
  val override: t -> override_flag
  val attrs : t -> attributes
  val loc : t -> location
end = struct

end

and Module_decl : sig
  type t
  val make :
    location -> attributes -> ns_module binder -> string option loc ->
    module_presence -> Module_type.t -> t
  val ident : t -> ns_module binder
  val name : t -> string option loc
  val presence : t -> module_presence
  val typ : t -> Module_type.t
  val attrs : t -> attributes
  val loc : t -> location
end = struct

end

and Module_type : sig
  type t
  type desc =
    | Ident of ns_module_type lpath
    | Signature of Signature.t
    | Functor of Functor_parameter.t * Module_type.t
    | With of Module_type.t * With_constraint.t list
    | Type_of of Module_expr.t
    | Alias of ns_module lpath

  val desc : t -> desc
  val typ : t -> Ty.module_type
  val loc : t -> location
  val attrs : t -> attributes
end = struct

end

and Signature : sig
  type t
  val items : t -> Signature_item.t list
  val typ : t -> Ty.signature
end = struct

end

and Signature_item : sig
  type t
  type desc =
    | Value of Value_desc.t
    | Type of rec_flag * Type_decl.t list
    | Type_subst of Type_decl.t list
    (*| Type_ext of type_extension*)
    (*| Exception of type_exception*)
    | Module of Module_decl.t
    (*| Modsubst of Module_substitution.t*)
    | Rec_module of Module_decl.t list
    | Modtype of Module_type_decl.t
    (*| Modtype_subst of Module_type_decl.t*)
    | Open of Open_desc.t
    | Include of Include_desc.t
    | Attribute of attribute

  val make : location -> desc -> t
  val desc: t -> desc
  val loc: t -> location
end = struct

end

and Open_desc : sig
  type t
  val make :
    location -> attributes -> override_flag ->
    ns_module lpath -> Ty.signature -> t
  val path : t -> ns_module lpath
  val bound_items: t -> Ty.signature
  val override: t -> override_flag
  val attrs : t -> attributes
  val loc : t -> location
end = struct
end

and Include_desc : sig
  type t
  val make : location -> attributes -> Module_type.t -> Ty.signature -> t
  val desc : t -> Module_type.t
  val typ : t -> Ty.signature
  val loc: t -> location
  val attrs: t -> attribute list
end = struct

end

and Value_desc : sig
  type t
  val make :
    location -> attributes -> ns_value binder -> string loc ->
    Core_type.t -> Ty.value_desc
  val binder : t -> ns_value binder
  val name : t -> string loc
  val typ: t -> Core_type.t
  val ty_desc: t -> Ty.value_desc
  val prim: t -> string list
  val loc: t -> location
  val attrs: t -> attributes
end = struct

end

and Type_decl : sig
  type t

  type kind =
    | Abstract
    | Variant of Constructor.t list
    | Record of Label.t list
    | Open

  type parameter = {
    typ: Core_type.t;
    variance: variance;
    injectivity: injectivity;
  }

  type constraint_equation = {
    lhs: Core_type.t;
    rhs: Core_type.t;
    loc: location;
  }

  val make :
    location -> attributes -> ns_type binder -> string loc -> Ty.type_decl ->
    params:parameter list -> manifest:Core_type.t option ->
    constraints:constraint_equation list ->
    private_flag -> kind ->
    t

  val id : t -> ns_type binder
  val name : t -> string loc
  val params : t -> (Core_type.t * (variance * injectivity)) list
  val typ : t -> Ty.type_decl
  val constraints : t -> constraint_equation list
  val kind : t -> kind
  val privacy : t -> private_flag
  val manifest : t -> Core_type.t option
  val loc : t -> location
  val attrs : t -> attributes
end = struct

end

and With_constraint : sig
  type t
  type desc =
    | Type of {
        lhs: ns_type lpath;
        rhs: Type_decl.t;
        substitute: bool;
      }
    | Module of {
        lhs: ns_module lpath;
        rhs: ns_module lpath;
        substitute: bool;
      }
    | Modtype of {
        lhs: ns_module_type lpath;
        rhs: Module_type.t;
        substitute: bool;
      }

  val desc : t -> desc
end = struct

end

and Module_type_decl : sig
  type t
  val make :
    location -> attributes ->
    ns_module_type binder -> string loc -> Module_type.t option -> t
  val binder : t -> ns_module_type binder
  val name : t -> string loc
  val typ : t -> Module_type.t option
  val attrs : t -> attributes
  val loc : t -> location
end = struct

end

and Structure : sig
  type t
  val make : Ty.signature -> Structure_item.t list -> t
  val items : t -> Structure_item.t list
  val typ : t -> Ty.signature
end = struct
end

and Structure_item : sig
  type t

  type desc =
    | Eval of Expression.t * attributes
    | Value of rec_flag * Value_binding.t list
    | Primitive of Value_desc.t
    | Type of rec_flag * Type_decl.t list
    (*| Type_ext of type_extension*)
    (*| Exception of type_exception*)
    | Module of Module_binding.t
    | Recmodule of Module_binding.t list
    | Modtype of Module_type_decl.t
    | Open of Open_decl.t
    | Include of Include_decl.t
    | Attribute of attribute

  val make : location -> desc -> t
  val desc : t -> desc
  val loc : t -> location
end = struct
end

and Module_binding : sig
  type t
  val make :
    location -> attributes -> ns_module binder -> string option loc ->
    module_presence -> Module_expr.t -> t
  val binder : t -> ns_module binder
  val name : t -> string option loc
  val presence : t -> module_presence
  val expr : t -> Module_expr.t
  val attrs : t -> attributes
  val loc : t -> location
end = struct
end

and Include_decl : sig
  type t
  val make : location -> attributes -> Module_expr.t -> Ty.signature -> t
  val desc : t -> Module_expr.t
  val typ : t -> Ty.signature
  val loc: t -> location
  val attrs: t -> attribute list
end = struct
end

module Package_type : sig
  type t
  val make :
    ns_module_type lpath -> (longident loc * Core_type.t) list ->
    Ty.module_type -> t
  val path : t -> ns_module_type lpath
  val constraints : t -> (longident loc * Core_type.t) list
  val typ : t -> Ty.module_type
end = struct
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

(*module Module_substitution : sig
  type t
  val id : t -> Ident.t
  val name : t -> string loc
  val manifest : t -> Path.t
  val txt : t -> longident loc
  val attrs: t -> attributes
  val loc : t -> location
end*)

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
  type t
  val structure : t -> Structure.t
  val coercion  : t -> Module_coercion.t
  val signature : t -> Ty.signature
end = struct
end
