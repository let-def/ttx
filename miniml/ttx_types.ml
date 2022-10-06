open Ttx_def

type ns_value = private Ns_value
type ns_type = private Ns_type
type ns_type_level = private Ns_type_level
type ns_constructor = private Ns_constructor
type ns_label = private Ns_label
type ns_module = private Ns_module
type ns_module_type = private Ns_module_type

module Namespace = struct
  type 'a t =
    | Value : ns_value t
    | Type : ns_type t
    | Type_level : ns_type_level t
    | Constructor : ns_constructor t
    | Label : ns_label t
    | Module : ns_module t
    | Module_type : ns_module_type t
  let order (type a b) (a : a t) (b : b t) : (a, b) Context.type_ordering =
    match a, b with
    | Value       , Value       -> Eq
    | Type        , Type        -> Eq
    | Type_level  , Type_level  -> Eq
    | Constructor , Constructor -> Eq
    | Label       , Label       -> Eq
    | Module      , Module      -> Eq
    | Module_type , Module_type -> Eq
    | (Value|Type|Type_level|Constructor|Label|Module|Module_type), _ ->
      let c = compare (Obj.repr a) (Obj.repr b) in
      if c < 0 then Lt else Gt
  let to_string : type a. a t -> string = function
    | Value       -> "value"
    | Type        -> "type"
    | Type_level  -> "type variables"
    | Constructor -> "constructor"
    | Label       -> "label"
    | Module      -> "module"
    | Module_type -> "module type"
end

include Context.With_namespace(Namespace)

module Path = struct
  type 'a t =
    | Ident : 'a name -> 'a t
    | Dot   : { parent: ns_module t; component: string } -> 'a t
    (*| Apply : { lhs : ns_module t; rhs: ns_module t } -> ns_module t*)
end

module Type_expr : sig
  type t
  type desc =
    | Var of ns_type_level name
    | Arrow of { lhs: t; rhs: t; }
    | Tuple of { components: t list; }
    | Const of { parameters: t list; path: ns_type Path.t }

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val make : desc -> t
  val desc : t -> desc

  val make_undefined : unit -> t
  val define : t -> desc -> unit
  exception Already_defined of {old_desc: desc; new_desc: desc}
  exception Undefined
end = struct

  let gen_id =
    let r = ref 0 in
    fun () -> incr r; !r

  type t = {
    id: int;
    mutable desc: desc option;
  }

  and desc =
    | Var of ns_type_level name
    | Arrow of { lhs: t; rhs: t; }
    | Tuple of { components: t list; }
    | Const of { parameters: t list; path: ns_type Path.t }

  let compare t1 t2 =
    if t1 == t2
    then 0
    else
      let c = Int.compare t1.id t2.id in
      assert (c <> 0);
      c

  let equal t1 t2 =
    t1 == t2

  let hash t = t.id

  let make desc = {id = gen_id (); desc = Some desc}

  exception Undefined

  let desc t =
    match t.desc with
    | None -> raise Undefined
    | Some desc -> desc

  let make_undefined () = {id = gen_id (); desc = None}

  exception Already_defined of {old_desc: desc; new_desc: desc}

  let define t new_desc =
    match t.desc with
    | None -> t.desc <- Some new_desc
    | Some old_desc -> raise (Already_defined {old_desc; new_desc})
end

module Type_scheme : sig
  type t
  val make : ns_type_level binder list -> Type_expr.t -> t
  val level : t -> ns_type_level binder list
  val expr : t -> Type_expr.t
end = struct
  type t = {
    level: ns_type_level binder list;
    expr: Type_expr.t;
  }

  let make level expr =
    {level; expr}

  let level t = t.level
  let expr t = t.expr
end

module Value_description : sig
  type t
  type desc =
    | Regular
    | Primitive

  val name : t -> string
  val typ : t -> Type_scheme.t
  val desc : t -> desc

  val make : string -> Type_scheme.t -> desc -> t
end = struct
  type desc =
    | Regular
    | Primitive

  type t = {
    name: string;
    typ: Type_scheme.t;
    desc: desc;
  }

  let name t = t.name
  let typ t = t.typ
  let desc t = t.desc

  let make name typ desc = {name; typ; desc}
end

module Constructor : sig
  type t
  val make : string -> ns_type binder list -> Type_expr.t list -> Type_expr.t -> t
  val name : t -> string
  val parameters : t -> ns_type binder list
  val arguments : t -> Type_expr.t list
  val result : t -> Type_expr.t
end = struct
  type t = {
    name: string;
    parameters: ns_type binder list;
    arguments: Type_expr.t list;
    result: Type_expr.t;
  }

  let make name parameters arguments result =
    {name; parameters; arguments; result}

  let name       t = t.name
  let parameters t = t.parameters
  let arguments  t = t.arguments
  let result     t = t.result
end

module Type_decl : sig
  type t
  type desc =
    | Abstract
    | Record
    | Variant of Constructor.t list
    | Open

  val name : t -> string
  val parameters : t -> ns_type binder list
  val desc : t -> desc

  val make : string -> ns_type binder list -> desc -> t
end = struct
  type desc =
    | Abstract
    | Record
    | Variant of Constructor.t list
    | Open

  type t = {
    name: string;
    parameters: ns_type binder list;
    desc: desc;
  }
  let name t = t.name
  let parameters t = t.parameters
  let desc t = t.desc

  let make name parameters desc = {name; parameters; desc}
end

