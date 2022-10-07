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

module Label_decl : sig
  type t
  val make :
    string -> mutable_flag -> forall:Type_level.t ->
    record:Type_expr.t -> field:Type_expr.t -> t
  val name : t -> string
  val forall : t -> Type_level.t
  val mutability : t -> mutable_flag
  val record : t -> Type_expr.t
  val field : t -> Type_expr.t
end = struct
  type t = {
    name: string;
    forall: Type_level.t;
    mutability: mutable_flag;
    record: Type_expr.t;
    field: Type_expr.t;
  }

  let make name mutability ~forall ~record ~field =
    { name; forall; mutability; record; field }

  let name       t = t.name
  let forall     t = t.forall
  let mutability t = t.mutability
  let record     t = t.record
  let field      t = t.field
end

module Constructor : sig
  type t

  type arguments =
    | Tuple of Type_expr.t list
    | Record of Label_decl.t list

  val make : string -> forall:Type_level.t -> arguments -> Type_expr.t -> t
  val name : t -> string
  val forall : t -> Type_level.t
  val arguments : t -> arguments
  val result : t -> Type_expr.t
end = struct
  type arguments =
    | Tuple of Type_expr.t list
    | Record of Label_decl.t list

  type t = {
    name: string;
    forall: Type_level.t;
    arguments: arguments;
    result: Type_expr.t;
  }

  let make name ~forall arguments result =
    {name; forall; arguments; result}

  let name      t = t.name
  let forall    t = t.forall
  let arguments t = t.arguments
  let result    t = t.result
end

module Type_decl : sig
  type t
  type desc =
    | Abstract
    | Record of Label_decl.t list
    | Variant of Constructor.t list
    | Open

  val make :
    string ->
    forall:Type_level.t ->
    params:Type_expr.t list ->
    manifest:Type_expr.t option ->
    desc -> t

  val name : t -> string
  val forall : t -> Type_level.t
  val params : t -> Type_expr.t list
  val manifest : t -> Type_expr.t option
  val desc : t -> desc
end = struct
  type desc =
    | Abstract
    | Record of Label_decl.t list
    | Variant of Constructor.t list
    | Open

  type t = {
    name: string;
    forall: Type_level.t;
    params: Type_expr.t list;
    manifest: Type_expr.t option;
    desc: desc;
  }

  let make name ~forall ~params ~manifest desc =
    {name; forall; params; manifest; desc}

  let name   t = t.name
  let forall t = t.forall
  let params t = t.params
  let desc   t = t.desc
  let manifest t = t.manifest
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
    | Named of ns_module binder option * Module_type.t

  val make : desc -> t
  val desc : t -> desc
end = struct
  type t = desc

  and desc =
    | Unit
    | Named of ns_module binder option * Module_type.t

  let make x = x
  let desc x = x
end

and Module_decl : sig
  type t

  val make : string -> Module_type.t -> t
  val name : t -> string
  val typ : t -> Module_type.t
end = struct
  type t = {
    name: string;
    typ: Module_type.t;
  }

  let make name typ = {name; typ}
  let name t = t.name
  let typ t = t.typ
end

and Module_type_decl : sig
  type t
  val make : Module_type.t option -> t
  val typ : t -> Module_type.t option
end = struct
  type t = { typ: Module_type.t option }
  let make typ = { typ }
  let typ t = t.typ
end

and Signature_item : sig
  type t

  type visibility =
    | Exported
    | Hidden

  type desc =
    | Value of ns_value binder * Value_desc.t
    | Type of rec_flag * (ns_type binder * Type_decl.t) list
    | Module of rec_flag * (ns_module binder * Module_decl.t) list
    | Module_type of ns_module_type binder * Module_type_decl.t

  val desc : t -> desc
  val visibility : t -> visibility
  val make : visibility -> desc -> t
end = struct
  type visibility =
    | Exported
    | Hidden

  type desc =
    | Value of ns_value binder * Value_desc.t
    | Type of rec_flag * (ns_type binder * Type_decl.t) list
    | Module of rec_flag * (ns_module binder * Module_decl.t) list
    | Module_type of ns_module_type binder * Module_type_decl.t

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
type label_decl        = Label_decl.t
type type_decl         = Type_decl.t
type value_desc        = Value_desc.t
type module_type       = Module_type.t
type functor_parameter = Functor_parameter.t
type module_decl       = Module_decl.t
type module_type_decl  = Module_type_decl.t
type signature         = Signature.t
type signature_item    = Signature_item.t

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
    | Constructor : constructor -> ns_constructor decl
    | Label       : label_decl -> ns_label decl
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
end = struct
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
    | Constructor : constructor -> ns_constructor decl
    | Label       : label_decl -> ns_label decl
    | Module      : module_decl -> ns_module decl
    | Import      : string * Digest.t -> ns_module decl
    | Module_type : module_type_decl -> ns_module_type decl

  let namespace (type a) : a decl -> a namespace = function
    | Value  _ -> Value
    | Type   _ -> Type
    | Module _ -> Module
    | Import _ -> Module
    | Type_level _ -> Type_level
    | Constructor _ -> Constructor
    | Label _ -> Label
    | Module_type _ -> Module_type

  type 'env bind = { bind: 'a. 'a binder -> 'a decl -> 'env -> 'env }
  let enter (type a env) (bind : env bind)
      (category : a category) (value : a) (env : env) : env =
    match category with
    | Type_expr -> env
    | Type_level -> env
    | Type_scheme ->
      let lvl = Type_scheme.forall value in
      bind.bind (Type_level.binder lvl) (Type_level lvl) env
    (* Constructors and labels are bound by the type declaration *)
    | Constructor -> env
    | Label_decl -> env
    | Type_decl ->
      let lvl = Type_decl.forall value in
      bind.bind (Type_level.binder lvl) (Type_level lvl) env
    | Value_desc -> env
    | Module_type ->
      begin match Module_type.desc value with
       | Functor (fp, _) ->
         begin match Functor_parameter.desc fp with
           | Named (Some binder, mt) ->
             bind.bind binder (Module (Module_decl.make (get_text (get_name binder)) mt)) env
           | _ -> env
         end
       | _ -> env
      end
    | Functor_parameter -> env
    | Module_decl -> env
    | Module_type_decl -> env
    | Signature -> env
    | Signature_item -> begin match Signature_item.desc value with
        | Value (b, d) ->
          bind.bind b (Value d) env
        | Type (_, bs) ->
          List.fold_left (fun env (b, d) -> bind.bind b (Type d) env) env bs
        | Module (_, bs) ->
          List.fold_left (fun env (b, d) -> bind.bind b (Module d) env) env bs
        | Module_type (b, d) ->
          bind.bind b (Module_type d) env
      end

  type 'env iter = { iter: 'a. 'env iter -> 'env -> 'a category -> 'a -> unit }
  let iter (type a env) (iter : env iter) env
      (category : a category) (value : a) : unit =
    let iter k x = iter.iter iter env k x in
    match category with
    | Type_expr -> begin match Type_expr.desc value with
        | Var _ -> ()
        | Arrow {lhs; rhs} ->
          iter Type_expr lhs;
          iter Type_expr rhs;
        | Tuple ts ->
          List.iter (iter Type_expr) ts
        | Const (ts, _) ->
          List.iter (iter Type_expr) ts
      end
    | Type_level -> ()
    | Type_scheme ->
      iter Type_level (Type_scheme.forall value);
      iter Type_expr (Type_scheme.expr value)
    | Constructor ->
      iter Type_level (Constructor.forall value);
      begin match Constructor.arguments value with
        | Tuple ts -> List.iter (iter Type_expr) ts
        | Record ls -> List.iter (iter Label_decl) ls
      end;
      iter Type_expr (Constructor.result value)
    | Label_decl ->
      iter Type_level (Label_decl.forall value);
      iter Type_expr (Label_decl.record value);
      iter Type_expr (Label_decl.field value)
    | Type_decl ->
      iter Type_level (Type_decl.forall value);
      List.iter (iter Type_expr) (Type_decl.params value);
      begin match Type_decl.desc value with
        | Type_decl.Abstract -> ()
        | Type_decl.Open -> ()
        | Type_decl.Record ls ->
          List.iter (iter Label_decl) ls
        | Type_decl.Variant cs ->
          List.iter (iter Constructor) cs
      end
    | Value_desc ->
      iter Type_scheme (Value_desc.typ value)
    | Module_type ->
      begin match Module_type.desc value with
        | Ident _ -> ()
        | Signature s -> iter Signature s
        | Functor (fp, mt) ->
          iter Functor_parameter fp;
          iter Module_type mt
        | Alias _ -> ()
      end
    | Functor_parameter -> begin match Functor_parameter.desc value with
        | Unit -> ()
        | Named (_id, mt) -> iter Module_type mt
      end
    | Module_decl ->
      iter Module_type (Module_decl.typ value)
    | Module_type_decl ->
      Option.iter (iter Module_type) (Module_type_decl.typ value)
    | Signature ->
      List.iter (iter Signature_item) (Signature.items value)
    | Signature_item -> begin match Signature_item.desc value with
        | Value (_, d) -> iter Value_desc d
        | Type (_, ds) -> List.iter (fun (_,d) -> iter Type_decl d) ds
        | Module (_, ds) -> List.iter (fun (_,d) -> iter Module_decl d) ds
        | Module_type (_, d) ->iter Module_type_decl d
      end

  type 'env map = { map: 'a. 'env map -> 'env -> 'a category -> 'a -> 'a }
  let map (type a env) (map : env map) env
      (category : a category) (value : a) : a =
    let map k x = map.map map env k x in
    match category with
    | Type_expr -> begin match Type_expr.desc value with
        | Var _ -> value
        | Arrow {lhs; rhs} ->
          let lhs' = map Type_expr lhs in
          let rhs' = map Type_expr rhs in
          if Type_expr.equal lhs lhs' && Type_expr.equal rhs rhs' then
            value
          else
            Type_expr.make (Arrow {lhs=lhs'; rhs=rhs'})
        | Tuple ts ->
          let ts' = List.map (map Type_expr) ts in
          if List.equal Type_expr.equal ts ts' then
            value
          else
            Type_expr.make (Tuple ts')
        | Const (ts, p) ->
          let ts' = List.map (map Type_expr) ts in
          if List.equal Type_expr.equal ts ts' then
            value
          else
            Type_expr.make (Const (ts', p))
      end
    | Type_level -> value
    | Type_scheme ->
      let forall = Type_scheme.forall value in
      let expr = Type_scheme.expr value in
      let forall' = map Type_level forall in
      let expr' = map Type_expr expr in
      if forall == forall' && Type_expr.equal expr expr' then
        value
      else
        Type_scheme.make forall' expr'
    | Constructor ->
      let forall = Constructor.forall value in
      let forall' = map Type_level forall in
      let arguments = Constructor.arguments value in
      let arguments' = match arguments with
        | Tuple ts ->
          let ts' = List.map (map Type_expr) ts in
          if List.equal Type_expr.equal ts ts'
          then arguments
          else Tuple ts'
        | Record ls ->
          let ls' = List.map (map Label_decl) ls in
          if List.equal (==) ls ls'
          then arguments
          else Record ls'
      in
      let result = Constructor.result value in
      let result' = map Type_expr result in
      if forall == forall' &&
         arguments == arguments' &&
         Type_expr.equal result result'
      then value
      else Constructor.make (Constructor.name value) ~forall:forall' arguments' result'
    | Label_decl ->
      let forall = Label_decl.forall value in
      let forall' = map Type_level forall in
      let record = Label_decl.record value in
      let record' = map Type_expr record in
      let field = Label_decl.field value in
      let field' = map Type_expr field in
      if forall == forall' &&
         Type_expr.equal record record' &&
         Type_expr.equal field field'
      then value
      else Label_decl.make
          (Label_decl.name value) (Label_decl.mutability value)
          ~forall:forall' ~record:record' ~field:field'
    | Type_decl ->
      let forall = Type_decl.forall value in
      let forall' = map Type_level forall in
      let params = Type_decl.params value in
      let params' = List.map (map Type_expr) params in
      let desc = Type_decl.desc value in
      let desc' = match desc with
        | Abstract -> desc
        | Open -> desc
        | Record ls ->
          let ls' = List.map (map Label_decl) ls in
          if List.equal (==) ls ls'
          then desc
          else Record ls'
        | Variant cs ->
          let cs' = List.map (map Constructor) cs in
          if List.equal (==) cs cs'
          then desc
          else Variant cs'
      in
      let manifest = Type_decl.manifest value in
      let manifest' = Option.map (map Type_expr) manifest in
      if forall == forall' &&
         List.equal (==) params params' &&
         desc == desc' &&
         Option.equal Type_expr.equal manifest manifest'
      then value
      else Type_decl.make (Type_decl.name value)
          ~forall:forall' ~params:params' ~manifest:manifest' desc'
    | Value_desc ->
      let typ = Value_desc.typ value in
      let typ' = map Type_scheme typ in
      if typ == typ' then
        value
      else
        Value_desc.make (Value_desc.name value) typ' (Value_desc.desc value)
    | Module_type ->
      begin match Module_type.desc value with
        | Ident _ -> value
        | Alias _ -> value
        | Signature s ->
          let s' = map Signature s in
          if s' == s then
            value
          else
            Module_type.make (Signature s')
        | Functor (fp, mt) ->
          let fp' = map Functor_parameter fp in
          let mt' = map Module_type mt in
          if fp' == fp && mt == mt' then
            value
          else
            Module_type.make (Functor (fp, mt))
      end
    | Functor_parameter -> begin match Functor_parameter.desc value with
        | Unit -> value
        | Named (id, mt) ->
          let mt' = map Module_type mt in
          if mt == mt' then
            Functor_parameter.make (Named (id, mt'))
          else
            value
      end
    | Module_decl ->
      let mt = Module_decl.typ value in
      let mt' = map Module_type mt in
      if mt == mt' then
        value
      else
        Module_decl.make (Module_decl.name value) mt'
    | Module_type_decl ->
      let mt = Module_type_decl.typ value in
      let mt' = Option.map (map Module_type) mt in
      if mt == mt' then
        value
      else
        Module_type_decl.make mt'
    | Signature ->
      let items = Signature.items value in
      let items' = List.map (map Signature_item) items in
      if List.equal (==) items items' then
        value
      else
        Signature.make items'
    | Signature_item ->
      let desc = Signature_item.desc value in
      let desc' = match desc with
        | Value (b, d) ->
          let d' = map Value_desc d in
          if d == d'
          then desc
          else Value (b, d')
        | Type (rf, ds) ->
          let ds' =
            List.map (fun (b,d as input) ->
                let d' = map Type_decl d in
                if d == d'
                then input
                else (b, d')
              ) ds
          in
          if List.equal (==) ds ds'
          then desc
          else Type (rf, ds')
        | Module (rf, ds) ->
          let ds' =
            List.map (fun (b,d as input) ->
                let d' = map Module_decl d in
                if d == d'
                then input
                else (b, d')
              ) ds
          in
          if List.equal (==) ds ds'
          then desc
          else Module (rf, ds')
        | Module_type (b, d) ->
          let d' = map Module_type_decl d in
          if d == d'
          then desc
          else Module_type (b, d')
      in
      if desc == desc' then
        value
      else
        Signature_item.make (Signature_item.visibility value) desc'
end
