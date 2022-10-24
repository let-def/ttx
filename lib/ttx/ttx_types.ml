open Ttx_def

class virtual ['self] binding_iter = object (self : 'self)
  inherit ['self] Binding.iter
  method visit_vd_type_binding = self#visit_binding
  method visit_vc_def_binding = self#visit_binding
  method visit_rl_def_binding = self#visit_binding
  method visit_td_body_binding = self#visit_binding
  method visit_mt_functor_binding = self#visit_binding
  method visit_s_item_binding = self#visit_binding
end

class virtual ['self] binding_map = object (self : 'self)
  inherit ['self] Binding.map
  method visit_vd_type_binding = self#visit_binding
  method visit_vc_def_binding = self#visit_binding
  method visit_rl_def_binding = self#visit_binding
  method visit_td_body_binding = self#visit_binding
  method visit_mt_functor_binding = self#visit_binding
  method visit_s_item_binding = self#visit_binding
end

class virtual ['self] vector_iter = object (_ : 'self)
  method visit_vector
    : 'a. ('env -> 'a -> unit) -> 'env -> 'a vector -> unit
    = fun visit_a env v ->
      Vector.iter (visit_a env) v
end

class virtual ['self] vector_map = object (_ : 'self)
  method visit_vector
    : 'a. ('env -> 'a -> 'a) -> 'env -> 'a vector -> 'a vector
    = fun visit_a env v ->
      Vector.map (visit_a env) v
end

type type_expr = {
  te_id: int;
  mutable te_desc: type_expr_desc option;
}

and type_expr_desc =
  | Te_var of (Type_level.variable[@opaque])
  | Te_arrow of {
      lhs: type_expr;
      rhs: type_expr;
    }
  | Te_tuple of type_expr list
  | Te_const of type_expr list * ns_type path

(*type*)
and value_desc = {
  vd_loc: location;
  vd_attrs: attributes;
  vd_binder: ns_value binder;
  vd_type: ((type_level, type_expr) binding [@name "vd_type_binding"]);
  vd_kind: vd_kind;
}

and vd_kind =
  | Vd_regular
  | Vd_primitive

(*type*)
and variant_constructor = {
  vc_loc: location;
  vc_attrs: attributes;
  vc_path: vc_path;
  vc_def: ((type_level, vc_arguments * type_expr) binding [@name "vc_def_binding"]);
}

and vc_path = {
  vc_type: ns_type path;
  vc_index: int;
  vc_name: string;
}

and vc_arguments =
  | Vc_tuple of type_expr vector
  | Vc_record of record_label vector

and record_label = {
  rl_loc: location;
  rl_attrs: attributes;
  rl_path: rl_path;
  rl_mutable: (mutable_flag [@opaque]);
  rl_def: ((type_level, rl_def) binding [@name "rl_def_binding"]);
}

and rl_def = {
  rl_label_type: type_expr;
  rl_record_type: type_expr;
}

and rl_path = {
  rl_kind: rl_kind;
  rl_index: int;
  rl_name: string;
}

and rl_kind =
  | Rl_normal of ns_type path
  | Rl_inline of vc_path

(*type*)
and type_decl = {
  td_loc: location;
  td_attrs: attributes;
  td_binder: ns_type binder;
  td_body: ((type_level, td_body) binding [@name "td_body_binding"]);
}

and td_body = {
  td_params: type_expr list;
  td_manifest: type_expr option;
  td_desc: td_desc;
}

and td_desc =
  | Td_abstract
  | Td_record of record_label vector
  | Td_variant of variant_constructor vector
  | Td_open

(*type*)
and functor_parameter =
  | Fp_unit
  | Fp_named of module_decl
  | Fp_anonymous of module_type

and module_type =
  | Mt_ident of ns_module_type path
  | Mt_signature of signature
  | Mt_functor of ((functor_parameter, module_type) binding [@name "mt_functor_binding"])
  | Mt_alias of ns_module path

and module_decl = {
  md_loc: location;
  md_attrs: attributes;
  md_binder: ns_module binder;
  md_type: module_type;
}

and module_type_decl = {
  mtd_loc: location;
  mtd_attrs: attributes;
  mtd_binder: ns_module_type binder;
  mtd_def: mtd_def;
}

and mtd_def =
  | Mtd_abstract
  | Mtd_concrete of module_type

and signature_item = {
  si_visibility: si_visibility;
  si_desc: si_desc;
}

and si_visibility =
  | Si_exported
  | Si_hidden

and si_desc =
  | Si_value of value_desc
  | Si_type of (rec_flag [@opaque]) * type_decl list
  | Si_module of (rec_flag [@opaque]) * module_decl list
  | Si_module_type of module_type_decl

and signature =
  | S_item of ((signature_item, signature) binding [@name "s_item_binding"])
  | S_done

[@@deriving
  visitors {
    variety = "map";
    data = false;
    ancestors = [
      "path_map";
      "location_map";
      "attributes_map";
      "binder_map";
      "binding_map";
      "type_level_map";
      "vector_map";
      "Namespace.visitor";
    ]
  },
  visitors {
    variety = "iter";
    data = false;
    ancestors = [
      "path_iter";
      "location_iter";
      "attributes_iter";
      "binder_iter";
      "binding_iter";
      "type_level_iter";
      "vector_iter";
      "Namespace.visitor";
    ]
  }]

module Type_expr = struct
  type t = type_expr
  type desc = type_expr_desc

  let compare t1 t2 =
    match Int.compare t1.te_id t2.te_id with
    | 0 -> assert (t1 == t2); 0
    | n -> n

  let equal t1 t2 =
    let result = t1 == t2 in
    assert (result = (t1.te_id = t2.te_id));
    result

  let hash t = t.te_id * 2147483647

  let id =
    let k = ref 0 in
    fun () -> let r = !k in incr k; r

  exception Already_defined
  exception Undefined

  let make desc = {te_id = id(); te_desc = Some desc}

  let make_undefined () = {te_id = id(); te_desc = None}

  let define t desc =
    match t.te_desc with
    | None -> t.te_desc <- Some desc
    | Some _ -> raise Already_defined

  let desc t =
    match t.te_desc with
    | None -> raise Undefined
    | Some d -> d

  module Table = Hashtbl.Make(struct
      type t = type_expr
      let hash = hash
      let equal = equal
    end)

  module Map = Map.Make(struct
      type t = type_expr
      let compare = compare
    end)

  module Set = Set.Make(struct
      type t = type_expr
      let compare = compare
    end)

  type 'a table = 'a Table.t
  type 'a map = 'a Map.t
  type set = Set.t
end


