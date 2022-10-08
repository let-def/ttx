open Ttx_def

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

module Type_expr : sig
  type t
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
end = struct

  let gen_id =
    let r = ref 0 in
    fun () -> incr r; !r

  type t = {
    id: int;
    mutable desc: desc option;
  }

  and desc =
    | Var of Type_level.variable
    | Arrow of { lhs: t; rhs: t; }
    | Tuple of t list
    | Const of t list * ns_type path

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

  exception Already_defined

  let define t new_desc =
    match t.desc with
    | None -> t.desc <- Some new_desc
    | Some _ -> raise Already_defined
end

module Type_scheme : sig
  type t
  val make : Type_level.t -> Type_expr.t -> t
  val forall : t -> Type_level.t
  val expr : t -> Type_expr.t
end = struct
  type t = {
    level: Type_level.t;
    expr: Type_expr.t;
  }

  let make level expr =
    {level; expr}

  let forall t = t.level
  let expr t = t.expr
end

module Value_desc : sig
  type t
  type desc =
    | Regular
    | Primitive

  val binder : t -> ns_value binder
  val typ : t -> Type_scheme.t
  val desc : t -> desc
  val make : location -> attributes -> ns_value binder -> Type_scheme.t -> desc -> t
  val loc : t -> location
  val attributes : t -> attributes
end = struct
  type desc =
    | Regular
    | Primitive

  type t = {
    binder: ns_value binder;
    typ: Type_scheme.t;
    desc: desc;
    loc: location;
    attributes: attributes;
  }

  let binder t = t.binder
  let typ t = t.typ
  let desc t = t.desc

  let make loc attributes binder typ desc =
    {binder; loc; attributes; typ; desc}

  let loc t = t.loc
  let attributes t = t.attributes
end

type nonrec constructor_path = {
  typ: ns_type path;
  index: int;
  name: string;
}

module Label : sig
  type t

  type kind =
    | Record of ns_type path
    | Inline_record of constructor_path

  type nonrec path = {
    kind: kind;
    index: int;
    name: string;
  }

  val make : location -> attributes -> path -> mutable_flag -> forall:Type_level.t -> record:Type_expr.t -> field:Type_expr.t -> t
  val path : t -> path
  val forall : t -> Type_level.t
  val mutability : t -> mutable_flag
  val record : t -> Type_expr.t
  val field : t -> Type_expr.t
  val loc : t -> location
  val attributes : t -> attributes
end = struct
  type kind =
    | Record of ns_type path
    | Inline_record of constructor_path

  type nonrec path = {
    kind: kind;
    index: int;
    name: string;
  }

  type t = {
    path: path;
    forall: Type_level.t;
    mutability: mutable_flag;
    record: Type_expr.t;
    field: Type_expr.t;
    loc: location;
    attributes: attributes;
  }

  let make loc attributes path mutability ~forall ~record ~field =
    {loc; attributes; path; forall; mutability; record; field}

  let path       t = t.path
  let forall     t = t.forall
  let mutability t = t.mutability
  let record     t = t.record
  let field      t = t.field

  let loc t = t.loc
  let attributes t = t.attributes
end

module Constructor : sig
  type t

  type arguments =
    | Tuple of Type_expr.t vector
    | Record of Label.t vector

  type nonrec path = constructor_path = {
    typ: ns_type path;
    index: int;
    name: string;
  }

  val make : location -> attributes -> path -> forall:Type_level.t -> arguments -> Type_expr.t -> t
  val path : t -> path
  val forall : t -> Type_level.t
  val arguments : t -> arguments
  val result : t -> Type_expr.t
  val loc : t -> location
  val attributes : t -> attributes
end = struct
  type nonrec path = constructor_path = {
    typ: ns_type path;
    index: int;
    name: string;
  }

  type arguments =
    | Tuple of Type_expr.t vector
    | Record of Label.t vector

  type t = {
    path: path;
    forall: Type_level.t;
    arguments: arguments;
    result: Type_expr.t;
    loc: location;
    attributes: attributes;
  }

  let make loc attributes path ~forall arguments result =
    {loc; attributes; path; forall; arguments; result}

  let path      t = t.path
  let forall    t = t.forall
  let arguments t = t.arguments
  let result    t = t.result

  let loc t = t.loc
  let attributes t = t.attributes
end

module Type_decl : sig
  type t
  type desc =
    | Abstract
    | Record of Label.t list
    | Variant of Constructor.t list
    | Open

  val make : location -> attributes ->
    ns_type binder ->
    forall:Type_level.t ->
    params:Type_expr.t list ->
    manifest:Type_expr.t option ->
    desc -> t

  val binder : t -> ns_type binder
  val forall : t -> Type_level.t
  val params : t -> Type_expr.t list
  val manifest : t -> Type_expr.t option
  val desc : t -> desc
  val loc : t -> location
  val attributes : t -> attributes
end = struct
  type desc =
    | Abstract
    | Record of Label.t list
    | Variant of Constructor.t list
    | Open

  type t = {
    binder: ns_type binder;
    forall: Type_level.t;
    params: Type_expr.t list;
    manifest: Type_expr.t option;
    desc: desc;
    loc: location;
    attributes: attributes;
  }

  let make loc attributes binder ~forall ~params ~manifest desc =
    {binder; loc; attributes; forall; params; manifest; desc}

  let binder t = t.binder
  let forall t = t.forall
  let params t = t.params
  let desc   t = t.desc
  let manifest t = t.manifest

  let loc t = t.loc
  let attributes t = t.attributes
end

module rec Module_type : sig
  type t
  type desc =
    | Ident of ns_module_type path
    | Signature of Signature.t
    | Functor of Functor_parameter.t * t
    | Alias of ns_module path

  val make : desc -> t
  val desc : t -> desc
end = struct
  type t = desc

  and desc =
    | Ident of ns_module_type path
    | Signature of Signature.t
    | Functor of Functor_parameter.t * t
    | Alias of ns_module path

  let make x = x
  let desc x = x
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
  type t = desc

  and desc =
    | Unit
    | Named of Module_decl.t
    | Anonymous of Module_type.t

  let make x = x
  let desc x = x
end

and Module_decl : sig
  type t

  val make : location -> attributes -> ns_module binder -> Module_type.t -> t
  val binder : t -> ns_module binder
  val typ : t -> Module_type.t
  val loc : t -> location
  val attributes : t -> attributes
end = struct
  type t = {
    binder: ns_module binder;
    typ: Module_type.t;
    loc: location;
    attributes: attributes;
  }
  let make loc attributes binder typ = {loc; attributes; binder; typ}
  let binder t = t.binder
  let typ t = t.typ

  let loc t = t.loc
  let attributes t = t.attributes
end

and Module_type_decl : sig
  type t
  val make : location -> attributes -> ns_module_type binder -> Module_type.t option -> t
  val binder : t -> ns_module_type binder
  val typ : t -> Module_type.t option
  val loc : t -> location
  val attributes : t -> attributes
end = struct
  type t = {
    binder: ns_module_type binder;
    typ: Module_type.t option;
    loc: location;
    attributes: attributes;
  }
  let make loc attributes binder typ = {loc; attributes; binder; typ }
  let binder t = t.binder
  let typ t = t.typ

  let loc t = t.loc
  let attributes t = t.attributes
end

and Signature_item : sig
  type t

  type visibility =
    | Exported
    | Hidden

  type desc =
    | Value of Value_desc.t
    | Type of rec_flag * Type_decl.t list
    | Module of rec_flag * Module_decl.t list
    | Module_type of Module_type_decl.t

  val desc : t -> desc
  val visibility : t -> visibility
  val make : visibility -> desc -> t
end = struct
  type visibility =
    | Exported
    | Hidden

  type desc =
    | Value of Value_desc.t
    | Type of rec_flag * Type_decl.t list
    | Module of rec_flag * Module_decl.t list
    | Module_type of Module_type_decl.t

  type t = {vis: visibility; desc: desc}

  let desc t = t.desc
  let visibility t = t.vis
  let make vis desc = {vis; desc}
end

and Signature : sig
  type t
  val make : Signature_item.t list -> t
  val items : t -> Signature_item.t list
end = struct
  type t = Signature_item.t list
  let make x = x
  let items x = x
end

type type_expr         = Type_expr.t
type type_level        = Type_level.t
type type_scheme       = Type_scheme.t
type constructor       = Constructor.t
type label             = Label.t
type type_decl         = Type_decl.t
type value_desc        = Value_desc.t
type module_type       = Module_type.t
type functor_parameter = Functor_parameter.t
type module_decl       = Module_decl.t
type module_type_decl  = Module_type_decl.t
type signature         = Signature.t
type signature_item    = Signature_item.t

module Visitor : sig
  type 'a binding =
    | Value       : value_desc -> ns_value binding
    | Type        : type_decl -> ns_type binding
    | Type_level  : type_level -> ns_type_level binding
    | Module      : module_decl -> ns_module binding
    | Import      : string * Digest.t -> ns_module binding
    | Module_type : module_type_decl -> ns_module_type binding

  val namespace : 'a binding -> 'a namespace

  type 'a category =
    | Location    : location category
    | Attributes  : attributes category
    | Type_expr   : type_expr category
    | Type_level  : type_level category
    | Type_scheme : type_scheme category
    | Constructor : constructor category
    | Label       : label category
    | Type_decl   : type_decl category
    | Value_desc  : value_desc category
    | Module_type : module_type category
    | Functor_parameter : functor_parameter category
    | Module_decl : module_decl category
    | Module_type_decl : module_type_decl category
    | Signature   : signature category
    | Signature_item : signature_item category

  type ('a, 'b) enter_category =
    | Enter_type_scheme : (unit, type_scheme) enter_category
    | Enter_constructor : (unit, constructor) enter_category
    | Enter_label : (unit, label) enter_category
    | Enter_type_decl : (unit, type_decl) enter_category
    | Enter_functor_parameter : (functor_parameter, module_type) enter_category
    | Enter_signature_items : (unit, signature_item list) enter_category

  type 'env bind = { bind: 'a. 'a binding -> 'env -> 'env }
  val enter : 'env bind -> ('a, 'b) enter_category -> 'a -> 'b -> 'env -> 'env

  type 'env iter = {
    syntax: 'a. 'env iter -> 'env -> 'a category -> 'a -> unit;
    enter: 'a 'b. 'env iter -> 'env -> ('a, 'b) enter_category -> 'a -> 'b -> unit;
  }
  val iter : 'env iter

  type 'env map = {
    syntax: 'a. 'env map -> 'env -> 'a category -> 'a -> 'a;
    enter: 'a 'b. 'env map -> 'env -> ('a, 'b) enter_category -> 'a -> 'b -> 'b;
  }
  val map : 'env map

end = struct
  type 'a binding =
    | Value       : value_desc -> ns_value binding
    | Type        : type_decl -> ns_type binding
    | Type_level  : type_level -> ns_type_level binding
    | Module      : module_decl -> ns_module binding
    | Import      : string * Digest.t -> ns_module binding
    | Module_type : module_type_decl -> ns_module_type binding

  let namespace (type a) : a binding -> a namespace = function
    | Value  _ -> Value
    | Type   _ -> Type
    | Module _ -> Module
    | Import _ -> Module
    | Type_level _ -> Type_level
    | Module_type _ -> Module_type

  type 'a category =
    | Location    : location category
    | Attributes  : attributes category
    | Type_expr   : type_expr category
    | Type_level  : type_level category
    | Type_scheme : type_scheme category
    | Constructor : constructor category
    | Label       : label category
    | Type_decl   : type_decl category
    | Value_desc  : value_desc category
    | Module_type : module_type category
    | Functor_parameter : functor_parameter category
    | Module_decl : module_decl category
    | Module_type_decl : module_type_decl category
    | Signature   : signature category
    | Signature_item : signature_item category

  type ('a, 'b) enter_category =
    | Enter_type_scheme : (unit, type_scheme) enter_category
    | Enter_constructor : (unit, constructor) enter_category
    | Enter_label : (unit, label) enter_category
    | Enter_type_decl : (unit, type_decl) enter_category
    | Enter_functor_parameter : (functor_parameter, module_type) enter_category
    | Enter_signature_items : (unit, signature_item list) enter_category

  type 'env bind = { bind: 'a. 'a binding -> 'env -> 'env }
  let enter (type a b env) (bind : env bind)
      (category : (a, b) enter_category) (param: a) (value : b) (env : env) : env =
    let bind d e = bind.bind d e in
    match category with
    | Enter_type_scheme ->
      bind (Type_level (Type_scheme.forall value)) env
    | Enter_constructor ->
      bind (Type_level (Constructor.forall value)) env
    | Enter_label ->
      bind (Type_level (Label.forall value)) env
    | Enter_type_decl ->
      bind (Type_level (Type_decl.forall value)) env
    | Enter_functor_parameter ->
      begin match Functor_parameter.desc param with
        | Unit -> env
        | Anonymous _ -> env
        | Named md -> bind (Module md) env
      end
    | Enter_signature_items ->
      begin match value with
        | [] -> env
        | x :: _ ->
          match Signature_item.desc x with
          | Value v -> bind (Value v) env
          | Type (_, bs) ->
            List.fold_left (fun env t -> bind (Type t) env) env bs
          | Module (_, bs) ->
            List.fold_left (fun env m -> bind (Module m) env) env bs
          | Module_type mt ->
            bind (Module_type mt) env
      end

  type 'env iter = {
    syntax: 'a. 'env iter -> 'env -> 'a category -> 'a -> unit;
    enter: 'a 'b. 'env iter -> 'env -> ('a, 'b) enter_category -> 'a -> 'b -> unit;
  }

  let iter_syntax (type a env)
      (iter : env iter) env (category : a category) (value : a) : unit =
    let syntax k x = iter.syntax iter env k x in
    let enter k x = iter.enter iter env k x in
    match category with
    | Location -> ()
    | Attributes -> ()
    | Type_expr -> begin match Type_expr.desc value with
        | Var _ -> ()
        | Arrow {lhs; rhs} ->
          syntax Type_expr lhs;
          syntax Type_expr rhs;
        | Tuple ts ->
          List.iter (syntax Type_expr) ts
        | Const (ts, _) ->
          List.iter (syntax Type_expr) ts
      end
    | Type_level -> ()
    | Type_scheme ->
      enter Enter_type_scheme () value
    | Constructor ->
      enter Enter_constructor () value
    | Label ->
      enter Enter_label () value
    | Type_decl ->
      enter Enter_type_decl () value
    | Value_desc ->
      syntax Location (Value_desc.loc value);
      syntax Attributes (Value_desc.attributes value);
      syntax Type_scheme (Value_desc.typ value)
    | Module_type ->
      begin match Module_type.desc value with
        | Ident _ -> ()
        | Signature s -> syntax Signature s
        | Functor (fp, mt) ->
          syntax Functor_parameter fp;
          enter Enter_functor_parameter fp mt
        | Alias _ -> ()
      end
    | Functor_parameter ->
      begin match Functor_parameter.desc value with
        | Unit -> ()
        | Anonymous mt -> syntax Module_type mt
        | Named md -> syntax Module_decl md
      end
    | Module_decl ->
      syntax Location (Module_decl.loc value);
      syntax Attributes (Module_decl.attributes value);
      syntax Module_type (Module_decl.typ value)
    | Module_type_decl ->
      syntax Location (Module_type_decl.loc value);
      syntax Attributes (Module_type_decl.attributes value);
      Option.iter (syntax Module_type) (Module_type_decl.typ value)
    | Signature ->
      List.iter (syntax Signature_item) (Signature.items value)
    | Signature_item -> begin match Signature_item.desc value with
        | Value d -> syntax Value_desc d
        | Type (_, ds) -> List.iter (syntax Type_decl) ds
        | Module (_, ds) -> List.iter (syntax Module_decl) ds
        | Module_type d ->syntax Module_type_decl d
      end

  let iter_enter (type a b env)
      (iter : env iter) env
      (category : (a, b) enter_category) (_ : a) (value : b) : unit =
    let syntax k x = iter.syntax iter env k x in
    let enter k x = iter.enter iter env k x in
    match category with
    | Enter_type_scheme ->
      syntax Type_level (Type_scheme.forall value);
      syntax Type_expr (Type_scheme.expr value)
    | Enter_constructor ->
      syntax Location (Constructor.loc value);
      syntax Attributes (Constructor.attributes value);
      syntax Type_level (Constructor.forall value);
      begin match Constructor.arguments value with
        | Tuple ts -> Vector.iter (syntax Type_expr) ts
        | Record ls -> Vector.iter (syntax Label) ls
      end;
      syntax Type_expr (Constructor.result value)
    | Enter_label ->
      syntax Location (Label.loc value);
      syntax Attributes (Label.attributes value);
      syntax Type_level (Label.forall value);
      syntax Type_expr (Label.record value);
      syntax Type_expr (Label.field value)
    | Enter_type_decl ->
      syntax Location (Type_decl.loc value);
      syntax Attributes (Type_decl.attributes value);
      syntax Type_level (Type_decl.forall value);
      List.iter (syntax Type_expr) (Type_decl.params value);
      begin match Type_decl.desc value with
        | Type_decl.Abstract -> ()
        | Type_decl.Open -> ()
        | Type_decl.Record ls ->
          List.iter (syntax Label) ls
        | Type_decl.Variant cs ->
          List.iter (syntax Constructor) cs
      end
    | Enter_functor_parameter ->
      syntax Module_type value
    | Enter_signature_items ->
      begin match value with
        | [] -> ()
        | x :: xs ->
          syntax Signature_item x;
          enter Enter_signature_items () xs
      end

  let iter = {syntax = iter_syntax; enter = iter_enter}

  type 'env map = {
    syntax: 'a. 'env map -> 'env -> 'a category -> 'a -> 'a;
    enter: 'a 'b. 'env map -> 'env -> ('a, 'b) enter_category -> 'a -> 'b -> 'b;
  }

  let map_syntax (type a env)
      (map : env map) env (category : a category) (value : a) : a =
    let syntax k x = map.syntax map env k x in
    let enter k x = map.enter map env k x in
    match category with
    | Location -> value
    | Attributes -> value
    | Type_expr ->
      begin match Type_expr.desc value with
        | Var _ -> value
        | Arrow {lhs; rhs} ->
          let lhs' = syntax Type_expr lhs in
          let rhs' = syntax Type_expr rhs in
          if Type_expr.equal lhs lhs' && Type_expr.equal rhs rhs'
          then value
          else Type_expr.make (Arrow {lhs=lhs'; rhs=rhs'})
        | Tuple ts ->
          let ts' = List.map (syntax Type_expr) ts in
          if List.equal Type_expr.equal ts ts'
          then value
          else Type_expr.make (Tuple ts')
        | Const (ts, p) ->
          let ts' = List.map (syntax Type_expr) ts in
          if List.equal Type_expr.equal ts ts'
          then value
          else Type_expr.make (Const (ts', p))
      end
    | Type_level -> value
    | Type_scheme ->
      enter Enter_type_scheme () value
    | Constructor ->
      enter Enter_constructor () value
    | Label ->
      enter Enter_label () value
    | Type_decl ->
      enter Enter_type_decl () value
    | Value_desc ->
      let loc = Value_desc.loc value in
      let attributes = Value_desc.attributes value in
      let loc' = syntax Location loc in
      let attributes' = syntax Attributes attributes in
      let typ = Value_desc.typ value in
      let typ' = syntax Type_scheme typ in
      if loc == loc' && attributes == attributes' && typ == typ'
      then value
      else Value_desc.make loc' attributes'
          (Value_desc.binder value) typ' (Value_desc.desc value)
    | Module_type ->
      begin match Module_type.desc value with
        | Ident _ -> value
        | Alias _ -> value
        | Signature s ->
          let s' = syntax Signature s in
          if s' == s
          then value
          else Module_type.make (Signature s')
        | Functor (fp, mt) ->
          let fp' = syntax Functor_parameter fp in
          let mt' = syntax Module_type mt in
          if fp' == fp && mt == mt'
          then value
          else Module_type.make (Functor (fp, mt))
      end
    | Functor_parameter -> begin match Functor_parameter.desc value with
        | Unit -> value
        | Anonymous mt ->
          let mt' = syntax Module_type mt in
          if mt == mt'
          then value
          else Functor_parameter.make (Anonymous mt')
        | Named md ->
          let md' = syntax Module_decl md in
          if md == md'
          then value
          else Functor_parameter.make (Named md')
      end
    | Module_decl ->
      let loc = syntax Location (Module_decl.loc value) in
      let attributes = syntax Attributes (Module_decl.attributes value) in
      let loc' = syntax Location loc in
      let attributes' = syntax Attributes attributes in
      let mt = Module_decl.typ value in
      let mt' = syntax Module_type mt in
      if loc == loc' && attributes == attributes' && mt == mt'
      then value
      else Module_decl.make loc' attributes' (Module_decl.binder value) mt'
    | Module_type_decl ->
      let loc = syntax Location (Module_type_decl.loc value) in
      let attributes = syntax Attributes (Module_type_decl.attributes value) in
      let loc' = syntax Location loc in
      let attributes' = syntax Attributes attributes in
      let mt = Module_type_decl.typ value in
      let mt' = Option.map (syntax Module_type) mt in
      if loc == loc' && attributes == attributes' && Option.equal (==) mt mt'
      then value
      else Module_type_decl.make loc' attributes' (Module_type_decl.binder value) mt'
    | Signature ->
      let items = Signature.items value in
      let items' = List.map (syntax Signature_item) items in
      if List.equal (==) items items'
      then value
      else Signature.make items'
    | Signature_item ->
      let desc = Signature_item.desc value in
      let desc' = match desc with
        | Value v ->
          let v' = syntax Value_desc v in
          if v == v'
          then desc
          else Value v'
        | Type (rf, ds) ->
          let ds' = List.map (syntax Type_decl) ds in
          if List.equal (==) ds ds'
          then desc
          else Type (rf, ds')
        | Module (rf, ds) ->
          let ds' = List.map (syntax Module_decl) ds in
          if List.equal (==) ds ds'
          then desc
          else Module (rf, ds')
        | Module_type mtd ->
          let mtd' = syntax Module_type_decl mtd in
          if mtd == mtd'
          then desc
          else Module_type mtd'
      in
      if desc == desc'
      then value
      else Signature_item.make (Signature_item.visibility value) desc'

  let map_enter (type a b env)
      (map : env map) env (category : (a, b) enter_category) (_ : a) (value : b) : b =
    let syntax k x = map.syntax map env k x in
    let enter k x = map.enter map env k x in
    match category with
    | Enter_type_scheme ->
      let forall = Type_scheme.forall value in
      let expr = Type_scheme.expr value in
      let forall' = syntax Type_level forall in
      let expr' = syntax Type_expr expr in
      if forall == forall' && Type_expr.equal expr expr'
      then value
      else Type_scheme.make forall' expr'
    | Enter_constructor ->
      let loc = Constructor.loc value in
      let attributes = Constructor.attributes value in
      let loc' = syntax Location loc in
      let attributes' = syntax Attributes attributes in
      let forall = Constructor.forall value in
      let forall' = syntax Type_level forall in
      let arguments = Constructor.arguments value in
      let arguments' = match arguments with
        | Tuple ts ->
          let ts' = Vector.map (syntax Type_expr) ts in
          if Vector.equal Type_expr.equal ts ts'
          then arguments
          else Tuple ts'
        | Record ls ->
          let ls' = Vector.map (syntax Label) ls in
          if Vector.equal (==) ls ls'
          then arguments
          else Record ls'
      in
      let result = Constructor.result value in
      let result' = syntax Type_expr result in
      if loc == loc' && attributes == attributes' &&
         forall == forall' && arguments == arguments' &&
         Type_expr.equal result result'
      then value
      else Constructor.make loc' attributes'
          (Constructor.path value)
          ~forall:forall' arguments' result'
    | Enter_label ->
      let loc = Label.loc value in
      let attributes = Label.attributes value in
      let loc' = syntax Location loc in
      let attributes' = syntax Attributes attributes in
      let forall = Label.forall value in
      let forall' = syntax Type_level forall in
      let record = Label.record value in
      let record' = syntax Type_expr record in
      let field = Label.field value in
      let field' = syntax Type_expr field in
      if loc == loc' && attributes == attributes' &&
         forall == forall' &&
         Type_expr.equal record record' &&
         Type_expr.equal field field'
      then value
      else Label.make loc' attributes'
          (Label.path value) (Label.mutability value)
          ~forall:forall' ~record:record' ~field:field'
    | Enter_type_decl ->
      let loc = Type_decl.loc value in
      let attributes = Type_decl.attributes value in
      let loc' = syntax Location loc in
      let attributes' = syntax Attributes attributes in
      let forall = Type_decl.forall value in
      let forall' = syntax Type_level forall in
      let params = Type_decl.params value in
      let params' = List.map (syntax Type_expr) params in
      let desc = Type_decl.desc value in
      let desc' = match desc with
        | Abstract -> desc
        | Open -> desc
        | Record ls ->
          let ls' = List.map (syntax Label) ls in
          if List.equal (==) ls ls'
          then desc
          else Record ls'
        | Variant cs ->
          let cs' = List.map (syntax Constructor) cs in
          if List.equal (==) cs cs'
          then desc
          else Variant cs'
      in
      let manifest = Type_decl.manifest value in
      let manifest' = Option.map (syntax Type_expr) manifest in
      if loc == loc' && attributes == attributes' &&
         forall == forall' && desc == desc' &&
         List.equal (==) params params' &&
         Option.equal Type_expr.equal manifest manifest'
      then value
      else Type_decl.make loc' attributes' (Type_decl.binder value)
          ~forall:forall' ~params:params' ~manifest:manifest' desc'
    | Enter_functor_parameter ->
      syntax Module_type value
    | Enter_signature_items  ->
      begin match value with
        | [] -> []
        | x :: xs ->
          let x = syntax Signature_item x in
          let xs = enter Enter_signature_items () xs in
          x :: xs
      end

  let map = {syntax = map_syntax; enter = map_enter}
end
