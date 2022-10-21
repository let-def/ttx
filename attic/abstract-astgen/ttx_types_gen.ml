open Ttx_def
module rec Type_expr : sig
  type t

  type desc =
    | Var of Type_level.variable
    | Arrow of {
        lhs: Type_expr.t;
        rhs: Type_expr.t;
      }
    | Tuple of Type_expr.t list
    | Const of Type_expr.t list * ns_type path

  val desc : t -> desc

  val compare : t -> t -> int
  val equal : t -> t -> bool
  val hash : t -> int

  val make : desc -> t

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
    | Arrow of {
        lhs: t;
        rhs: t;
      }
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

and Type_scheme : sig
  type t

  val forall : t -> type_level
  val expr : t -> Type_expr.t
  val make : type_level -> Type_expr.t -> t
end = struct
  type t = {
    forall: type_level;
    expr: Type_expr.t;
  }

  let forall (t : t) = t.forall
  let expr (t : t) = t.expr
  let make forall expr = {forall; expr}
end

and Value_desc : sig
  type t

  type desc =
    | Regular
    | Primitive

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_value binder
  val typ : t -> Type_scheme.t
  val desc : t -> desc
  val make : location -> attributes -> ns_value binder -> Type_scheme.t -> desc -> t
end = struct
  type t = {
    loc: location;
    attrs: attributes;
    binder: ns_value binder;
    typ: Type_scheme.t;
    desc: desc;
  }
  and desc =
    | Regular
    | Primitive

  let loc (t : t) = t.loc
  let attrs (t : t) = t.attrs
  let binder (t : t) = t.binder
  let typ (t : t) = t.typ
  let desc (t : t) = t.desc
  let make loc attrs binder typ desc = {loc; attrs; binder; typ; desc}
end

and Constructor : sig
  type t

  type path = {
    typ: ns_type Path.t;
    index: int;
    name: string;
  }

  type arguments =
    | Tuple of Type_expr.t vector
    | Record of Label.t vector

  val loc : t -> location
  val attrs : t -> attributes
  val path : t -> path
  val forall : t -> type_level
  val arguments : t -> arguments
  val result : t -> Type_expr.t
  val make : location -> attributes -> path -> type_level -> arguments -> Type_expr.t -> t
end = struct
  type t = {
    loc: location;
    attrs: attributes;
    path: path;
    forall: type_level;
    arguments: arguments;
    result: Type_expr.t;
  }
  and path = {
    typ: ns_type Path.t;
    index: int;
    name: string;
  }
  and arguments =
    | Tuple of Type_expr.t vector
    | Record of Label.t vector

  let loc (t : t) = t.loc
  let attrs (t : t) = t.attrs
  let path (t : t) = t.path
  let forall (t : t) = t.forall
  let arguments (t : t) = t.arguments
  let result (t : t) = t.result
  let make loc attrs path forall arguments result = {loc; attrs; path; forall; arguments; result}
end

and Label : sig
  type t

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
  val record : t -> Type_expr.t
  val field : t -> Type_expr.t
  val make : location -> attributes -> path -> type_level -> mutable_flag -> Type_expr.t -> Type_expr.t -> t
end = struct
  type t = {
    loc: location;
    attrs: attributes;
    path: path;
    forall: type_level;
    mutability: mutable_flag;
    record: Type_expr.t;
    field: Type_expr.t;
  }
  and kind =
    | Record of ns_type Path.t
    | Inline_record of Constructor.path
  and path = {
    kind: kind;
    index: int;
    name: string;
  }

  let loc (t : t) = t.loc
  let attrs (t : t) = t.attrs
  let path (t : t) = t.path
  let forall (t : t) = t.forall
  let mutability (t : t) = t.mutability
  let record (t : t) = t.record
  let field (t : t) = t.field
  let make loc attrs path forall mutability record field = {loc; attrs; path; forall; mutability; record; field}
end

and Type_decl : sig
  type t

  type desc =
    | Abstract
    | Record of Label.t list
    | Variant of Constructor.t list
    | Open

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_type binder
  val forall : t -> type_level
  val params : t -> Type_expr.t list
  val manifest : t -> Type_expr.t option
  val desc : t -> desc
  val make : location -> attributes -> ns_type binder -> type_level -> Type_expr.t list -> Type_expr.t option -> desc -> t
end = struct
  type t = {
    loc: location;
    attrs: attributes;
    binder: ns_type binder;
    forall: type_level;
    params: Type_expr.t list;
    manifest: Type_expr.t option;
    desc: desc;
  }
  and desc =
    | Abstract
    | Record of Label.t list
    | Variant of Constructor.t list
    | Open

  let loc (t : t) = t.loc
  let attrs (t : t) = t.attrs
  let binder (t : t) = t.binder
  let forall (t : t) = t.forall
  let params (t : t) = t.params
  let manifest (t : t) = t.manifest
  let desc (t : t) = t.desc
  let make loc attrs binder forall params manifest desc = {loc; attrs; binder; forall; params; manifest; desc}
end

and Functor_parameter : sig
  type t

  type desc =
    | Unit
    | Named of Module_decl.t
    | Anonymous of Module_type.t

  val desc : t -> desc
  val make : desc -> t
end = struct
  type t = {
    desc: desc;
  }
  and desc =
    | Unit
    | Named of Module_decl.t
    | Anonymous of Module_type.t

  let desc (t : t) = t.desc
  let make desc = {desc}
end

and Module_type : sig
  type t

  type desc =
    | Ident of ns_module_type path
    | Signature of Signature.t
    | Functor of Functor_parameter.t * Module_type.t
    | Alias of ns_module path

  val desc : t -> desc
  val make : desc -> t
end = struct
  type t = {
    desc: desc;
  }
  and desc =
    | Ident of ns_module_type path
    | Signature of Signature.t
    | Functor of Functor_parameter.t * t
    | Alias of ns_module path

  let desc (t : t) = t.desc
  let make desc = {desc}
end

and Module_decl : sig
  type t

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_module binder
  val typ : t -> Module_type.t
  val make : location -> attributes -> ns_module binder -> Module_type.t -> t
end = struct
  type t = {
    loc: location;
    attrs: attributes;
    binder: ns_module binder;
    typ: Module_type.t;
  }

  let loc (t : t) = t.loc
  let attrs (t : t) = t.attrs
  let binder (t : t) = t.binder
  let typ (t : t) = t.typ
  let make loc attrs binder typ = {loc; attrs; binder; typ}
end

and Module_type_decl : sig
  type t

  val loc : t -> location
  val attrs : t -> attributes
  val binder : t -> ns_module_type binder
  val typ : t -> Module_type.t option
  val make : location -> attributes -> ns_module_type binder -> Module_type.t option -> t
end = struct
  type t = {
    loc: location;
    attrs: attributes;
    binder: ns_module_type binder;
    typ: Module_type.t option;
  }

  let loc (t : t) = t.loc
  let attrs (t : t) = t.attrs
  let binder (t : t) = t.binder
  let typ (t : t) = t.typ
  let make loc attrs binder typ = {loc; attrs; binder; typ}
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

  val visibility : t -> visibility
  val desc : t -> desc
  val make : visibility -> desc -> t
end = struct
  type t = {
    visibility: visibility;
    desc: desc;
  }
  and visibility =
    | Exported
    | Hidden
  and desc =
    | Value of Value_desc.t
    | Type of rec_flag * Type_decl.t list
    | Module of rec_flag * Module_decl.t list
    | Module_type of Module_type_decl.t

  let visibility (t : t) = t.visibility
  let desc (t : t) = t.desc
  let make visibility desc = {visibility; desc}
end

and Signature : sig
  type t

  val items : t -> Signature_item.t list
  val make : Signature_item.t list -> t
end = struct
  type t = {
    items: Signature_item.t list;
  }

  let items (t : t) = t.items
  let make items = {items}
end

type type_expr = Type_expr.t
type type_scheme = Type_scheme.t
type value_desc = Value_desc.t
type constructor = Constructor.t
type label = Label.t
type type_decl = Type_decl.t
type functor_parameter = Functor_parameter.t
type module_type = Module_type.t
type module_decl = Module_decl.t
type module_type_decl = Module_type_decl.t
type signature_item = Signature_item.t
type signature = Signature.t

module Visitor = struct
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

  let iter = {
    iter = begin fun (type a k) (self : a iter) a (k : k category) (v : k) : unit ->
      match k with
      | Type_expr ->
        begin match (Type_expr.desc v) with
          | Var (_x_0) ->
            ()
          | Arrow _r ->
            self.iter self a Type_expr _r.lhs;
            self.iter self a Type_expr _r.rhs;
          | Tuple (_x_0) ->
            List.iter (fun x ->
                self.iter self a Type_expr x;
              ) _x_0;
            ()
          | Const (_x_0, _x_1) ->
            List.iter (fun x ->
                self.iter self a Type_expr x;
              ) _x_0;
            ()
        end;
      | Type_scheme ->
        self.iter self a Type_expr (Type_scheme.expr v);
      | Value_desc ->
        self.iter self a Type_scheme (Value_desc.typ v);
        begin match (Value_desc.desc v) with
          | Regular -> ()
          | Primitive -> ()
        end;
      | Constructor ->
        let _x = (Constructor.path v) in
        begin match (Constructor.arguments v) with
          | Tuple (_x_0) ->
            Vector.iter (fun x ->
                self.iter self a Type_expr x;
              ) _x_0;
            ()
          | Record (_x_0) ->
            Vector.iter (fun x ->
                self.iter self a Label x;
              ) _x_0;
            ()
        end;
        self.iter self a Type_expr (Constructor.result v);
      | Label ->
        let _x = (Label.path v) in
        begin match _x.kind with
          | Record (_x_0) ->
            ()
          | Inline_record (_x_0) ->
            ()
        end;
        self.iter self a Type_expr (Label.record v);
        self.iter self a Type_expr (Label.field v);
      | Type_decl ->
        List.iter (fun x ->
            self.iter self a Type_expr x;
          ) (Type_decl.params v);
        Option.iter (fun x ->
            self.iter self a Type_expr x;
          ) (Type_decl.manifest v);
        begin match (Type_decl.desc v) with
          | Abstract -> ()
          | Record (_x_0) ->
            List.iter (fun x ->
                self.iter self a Label x;
              ) _x_0;
            ()
          | Variant (_x_0) ->
            List.iter (fun x ->
                self.iter self a Constructor x;
              ) _x_0;
            ()
          | Open -> ()
        end;
      | Functor_parameter ->
        begin match (Functor_parameter.desc v) with
          | Unit -> ()
          | Named (_x_0) ->
            self.iter self a Module_decl _x_0;
            ()
          | Anonymous (_x_0) ->
            self.iter self a Module_type _x_0;
            ()
        end;
      | Module_type ->
        begin match (Module_type.desc v) with
          | Ident (_x_0) ->
            ()
          | Signature (_x_0) ->
            self.iter self a Signature _x_0;
            ()
          | Functor (_x_0, _x_1) ->
            self.iter self a Functor_parameter _x_0;
            self.iter self a Module_type _x_1;
            ()
          | Alias (_x_0) ->
            ()
        end;
      | Module_decl ->
        self.iter self a Module_type (Module_decl.typ v);
      | Module_type_decl ->
        Option.iter (fun x ->
            self.iter self a Module_type x;
          ) (Module_type_decl.typ v);
      | Signature_item ->
        begin match (Signature_item.visibility v) with
          | Exported -> ()
          | Hidden -> ()
        end;
        begin match (Signature_item.desc v) with
          | Value (_x_0) ->
            self.iter self a Value_desc _x_0;
            ()
          | Type (_x_0, _x_1) ->
            List.iter (fun x ->
                self.iter self a Type_decl x;
              ) _x_1;
            ()
          | Module (_x_0, _x_1) ->
            List.iter (fun x ->
                self.iter self a Module_decl x;
              ) _x_1;
            ()
          | Module_type (_x_0) ->
            self.iter self a Module_type_decl _x_0;
            ()
        end;
      | Signature ->
        List.iter (fun x ->
            self.iter self a Signature_item x;
          ) (Signature.items v);
    end;
  }
end
