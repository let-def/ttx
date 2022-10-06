module Ml = struct
  module Path = Path
  module Ident = Ident
  module Types = Types

  module Map : sig
    type 'a t
    val empty : debug_name:string -> 'a t
    val add : 'a t -> Ident.t -> 'a -> 'a t
    val get : 'a t -> Ident.t -> 'a
    val find : 'a t -> Ident.t -> 'a option
  end = struct
    type 'a t = string * (Ident.t, 'a) Bt2.t

    let empty ~debug_name = (debug_name, Bt2.leaf)

    let rec add t k v =
      match t with
      | Bt2.Leaf -> Bt2.node Bt2.leaf k v Bt2.leaf
      | Bt2.Node (_, l, k', v', r) ->
        let c = Ident.compare k k' in
        if c < 0 then
          Bt2.node (add l k v) k' v' r
        else if c > 0 then
          Bt2.node l k' v' (add r k v)
        else
          Bt2.node l k v r

    let add (name, t) k v = (name, add t k v)

    let rec find t k =
      match t with
      | Bt2.Leaf -> None
      | Bt2.Node (_, l, k', v, r) ->
        let c = Ident.compare k k' in
        if c < 0 then
          find l k
        else if c > 0 then
          find r k
        else
          Some v

    let get (name, t) k =
      match find t k with
      | Some v -> v
      | None ->
        invalid_arg (Printf.sprintf "Ml_map.get: %s unbound in map %s"
                       (Ident.unique_name k) name)

    let find (_, t) k = find t k
  end

  type 'a map = 'a Map.t
  module Tytable = Hashtbl.Make(Types.TypeOps)

end

open Ttx_types

module Context = struct
  include Make_context(struct
      type 'a t = 'a Visitor.decl
  end)

  let raw_fresh = fresh
  let fresh context info =
    let binder = fresh context (Visitor.namespace info) in
    (bind context binder info, binder)
end

type env = {
  context: Context.t;
  ml_types: ns_type binder Ml.map;
  ml_modules: ns_module binder Ml.map;
  ml_modtypes: ns_module_type binder Ml.map;
  ml_values: ns_value binder Ml.map;
  ml_cstrs: ns_constructor binder Ml.map;
}

module Initial = struct
  let context = ref Context.empty

  let simple_level vars =
    let binder = Context.raw_fresh !context Type_level in
    let level = Type_level.make binder in
    let vars = List.map (Type_level.fresh level) vars in
    Type_level.freeze level;
    (level, vars)

  let abstract_type vars name =
    let forall, params = simple_level vars in
    let mk_var var = Type_expr.make (Var var) in
    Type_decl.make ~name
      ~forall ~params:(List.map mk_var params)
      ~manifest:None Abstract

  let add decl =
    let context', binder = Context.fresh !context decl in
    context := context';
    name binder

  let update binder decl =
    context := Context.update !context binder decl

  let int    = add (Type (abstract_type [] "int"))
  let string = add (Type (abstract_type [] "string"))
  let char   = add (Type (abstract_type [] "char"))
  let float  = add (Type (abstract_type [] "float"))

  let bool, bool_true, bool_false =
    let decl = Type_decl.make_undefined () in
    let name = add (Type decl) in
    let forall, _ = simple_level [] in
    let typ = Type_expr.make (Const ([], Path.Ident name)) in
    let k_true = Constructor.make "true" ~forall (Tuple []) typ in
    let k_false = Constructor.make "false" ~forall (Tuple []) typ in
    Type_decl.define decl ~name:"bool" ~forall ~params:[] ~manifest:None
      (Variant [k_true; k_false]);
    (name, add (Constructor k_true), add (Constructor k_false))

  let list, list_nil, list_cons =
    let decl = Type_decl.make_undefined () in
    let name = add (Type decl) in
    let forall, vars = simple_level [Some "a"] in
    let a = Type_expr.make (Var (List.hd vars)) in
    let typ = Type_expr.make (Const ([a], Path.Ident name)) in
    let k_nil = Constructor.make "[]" ~forall (Tuple []) typ in
    let k_cons = Constructor.make "::" ~forall (Tuple [a; typ]) typ in
    Type_decl.define decl ~name:"list" ~forall ~params:[a] ~manifest:None
      (Variant [k_nil; k_cons]);
    (typ, add (Constructor k_nil), add (Constructor k_cons))

  let option, option_none, option_some =
    let decl = Type_decl.make_undefined () in
    let name = add (Type decl) in
    let forall, vars = simple_level [Some "a"] in
    let a = Type_expr.make (Var (List.hd vars)) in
    let typ = Type_expr.make (Const ([a], Path.Ident name)) in
    let k_nil = Constructor.make "None" ~forall (Tuple []) typ in
    let k_cons = Constructor.make "Some" ~forall (Tuple [a]) typ in
    Type_decl.define decl ~name:"option" ~forall ~params:[a] ~manifest:None
      (Variant [k_nil; k_cons]);
    (typ, add (Constructor k_nil), add (Constructor k_cons))

  let context = !context
end

let rec import_ml_module_path env : Ml.Path.t -> ns_module path = function
  | Pident id ->
    Path.Ident (name (Ml.Map.get env.ml_modules id))
  | Pdot (parent, dot) ->
    Path.Dot (import_ml_module_path env parent, dot)
  | Papply _ ->
    assert false (*TODO*)

let import_ml_path (type ns) env (map : ns binder Ml.map)
  : Ml.Path.t -> ns path = function
  | Pident id ->
    Path.Ident (name (Ml.Map.get map id))
  | Pdot (parent, dot) ->
    Path.Dot (import_ml_module_path env parent, dot)
  | Papply _ ->
    assert false

type type_import_state = {
  table: type_expr Ml.Tytable.t;
  level: string option Type_level.t;
  mutable suspended: bool;
  parent: type_import_state;
}

let new_level env =
  let binder = Context.raw_fresh env.context Type_level in
  let level = Type_level.make binder in
  let context = Context.bind env.context binder (Type_level level) in
  let rec self = {
    table = Ml.Tytable.create 7;
    level;
    suspended = false;
    parent = self;
  } in
  (self, {env with context})

let enter st env =
  assert (not st.suspended);
  let binder = Context.raw_fresh env.context Type_level in
  let level = Type_level.make binder in
  let context = Context.bind env.context binder (Type_level level) in
  st.suspended <- true;
  let st' = {table = st.table; level; suspended = false; parent = st} in
  (st', {env with context})

let leave st =
  assert (not st.suspended);
  if st.parent != st then (
    assert st.parent.suspended;
    st.parent.suspended <- false;
  );
  st.suspended <- true;
  Type_level.freeze st.level;
  st.level

let rec repr (t : Ml.Types.type_expr) =
  match t.desc with
  | Tlink t' -> repr t'
  | _ -> t

let rec import_type_expr st env (t : Ml.Types.type_expr) : type_expr =
  assert (not st.suspended);
  let t = repr t in
  match Ml.Tytable.find_opt st.table t with
  | Some t' -> t'
  | None ->
    let t' = Type_expr.make_undefined () in
    Ml.Tytable.add st.table t t';
    let desc : Type_expr.desc =
      match t.desc with
      | Tvar name ->
        Var (Type_level.fresh st.level name)
      | Tarrow (Nolabel, tlhs, trhs, _) ->
        let lhs = import_type_expr st env tlhs in
        let rhs = import_type_expr st env trhs in
        Arrow {lhs; rhs}
      | Ttuple ts ->
        let ts = List.map (import_type_expr st env) ts in
        Tuple ts
      | Tconstr (path, params, _) ->
        let path = import_ml_path env env.ml_types path in
        Const (List.map (import_type_expr st env) params, path)
      | Tarrow (_, _, _, _) -> assert false
      | Tobject _  -> assert false
      | Tfield _   -> assert false
      | Tnil       -> assert false
      | Tlink _    -> assert false
      | Tsubst _   -> assert false
      | Tvariant _ -> assert false
      | Tunivar _  -> assert false
      | Tpoly _    -> assert false
      | Tpackage _ -> assert false
    in
    Type_expr.define t' desc;
    t'

let import_value_description env name (vd : Ml.Types.value_description) =
  let st, env = new_level env in
  let typ = import_type_expr st env vd.val_type in
  let desc = match vd.val_kind with
    | Val_reg -> Value_desc.Regular
    | Val_prim _ -> Value_desc.Primitive
    | Val_ivar _ -> assert false
    | Val_self _ -> assert false
    | Val_anc _ -> assert false
  in
  let forall = leave st in
  let sch = Type_scheme.make forall typ in
  Value_desc.make name sch desc

let import_visibility : Ml.Types.visibility -> _ = function
  | Exported -> Signature_item.Exported
  | Hidden   -> Signature_item.Hidden

let same_visibility (v1 : Ml.Types.visibility) (v2 : Ml.Types.visibility) =
  match v1, v2 with
  | Exported, Exported | Hidden, Hidden -> true
  | Exported, Hidden | Hidden, Exported -> false

let rec gather_recursive_types acc (vis : Ml.Types.visibility) = function
  | Types.Sig_type (id, td, Trec_next, vis') :: rest ->
    assert (same_visibility vis vis');
    gather_recursive_types ((id, td) :: acc) vis rest
  | other ->
    (List.rev acc, other)

let rec gather_recursive_modules acc (vis : Ml.Types.visibility) = function
  | Types.Sig_module (id, mp, md, Trec_next, vis') :: rest ->
    assert (same_visibility vis vis');
    gather_recursive_modules ((id, mp, md) :: acc) vis rest
  | other ->
    (List.rev acc, other)

let import_rec_flag : Ml.Types.rec_status -> Ttx_def.rec_flag = function
  | Trec_not   -> Nonrecursive
  | Trec_first -> Recursive
  | Trec_next  -> assert false

let import_constructor_decl
    st forall params typ env (cd : Ml.Types.constructor_declaration) =
  let arguments : Constructor.arguments = match cd.cd_args with
    | Cstr_record _ -> assert false (*TODO*)
    | Cstr_tuple ts -> Tuple (List.map (import_type_expr st env) ts)
  in
  let result = match cd.cd_res with
    | Some te -> import_type_expr st env te
    | None -> Type_expr.make (Const (params, Path.Ident typ))
  in
  Constructor.make (Ident.name cd.cd_id) ~forall arguments result

let import_type_declaration env binder (ident, (td : Ml.Types.type_declaration)) =
  let st, env_inner = new_level env in
  let params = List.map (import_type_expr st env_inner) td.type_params in
  let forall = leave st in
  let manifest = Option.map (import_type_expr st env_inner) td.type_manifest in
  let desc =
    match td.type_kind with
    | Type_abstract ->
      (env, Type_decl.Abstract)
    | Type_open ->
      (env, Type_decl.Open)
    | Type_variant (cstrs, _repr) ->
      let decls = List.map
          (import_constructor_decl st forall params (name binder) env_inner)
          cstrs
      in
      let env = List.fold_left2 (fun env decl cd ->
          let context, name = Context.fresh env.context (Constructor decl) in
          let ml_cstrs = Ml.Map.add env.ml_cstrs cd.Ml.Types.cd_id name in
          {env with context; ml_cstrs}
        ) env decls cstrs
      in
      (env, Type_decl.Variant decls)
    | Type_record (_labels, _repr) ->
      assert false (*TODO*)
let define desc =
  Type_decl.make ~name:(Ident.name ident) ~forall ~params ~manifest desc
  in

let import_signature_ : (env -> Types.signature -> signature) ref =
  ref (fun _ -> assert false)

let rec import_module_type env (mt : Ml.Types.module_type) =
  Module_type.make (match mt with
    | Mty_ident p ->
      Ident (import_ml_path env env.ml_modtypes p)
    | Mty_alias p ->
      Alias (import_ml_path env env.ml_modules p)
    | Mty_signature sg ->
      Signature (!import_signature_ env sg)
    | Mty_functor (fp, mt) ->
      let env, fp = match fp with
        | Unit ->
          env, Functor_parameter.Unit
        | Named (name, mt') ->
          let mt' = import_module_type env mt' in
          match name with
          | None ->
            env, Functor_parameter.Named (None, mt')
          | Some n ->
            let md = Module_decl.make (Ident.name n) mt' in
            let context, binder = Context.fresh env.context (Module md) in
            let ml_modules = Ml.Map.add env.ml_modules n binder in
            let env = {env with ml_modules; context} in
            env, Functor_parameter.Named (Some binder, mt')
      in
      Functor (Functor_parameter.make fp, import_module_type env mt)
    )


let import_type_defs env vis rec_ defs =
  let vis = import_visibility vis in
  let rec_ = import_rec_flag rec_ in
  let group = Context.group_empty env.context in
  let group, binders =
    List.fold_left_map
      (fun group (id, _td) -> Context.group_fresh group Type (Ident.name id))
      group defs
  in
  let ml_types =
    List.fold_left2
      (fun map binder (id, _) -> Ml.Map.add map id binder)
      env.ml_types binders defs
  in
  let env = {env with ml_types} in
  let env = List.fold_left2 import_type_declaration env binders defs in
  let item = Signature_item.make vis (Type (rec_, decls)) in
  (env, item)

let import_module env = ()

let import_modules env vis rec_ mods =
  let vis = import_visibility vis in
  let rec_ = import_rec_flag rec_ in
  let env, decls = List.fold_left_map (fun env (id, _mp, _md) ->
      let decl = Module_decl.make_undefined () in
      let context, binder = Context.fresh env.context (Type decl) in
      let ml_types = Ml.Map.add env.ml_types id binder in
      let env = {env with context; ml_types} in
      (env, (binder, decl))
    ) env mods
  in
  let env = List.fold_left2 import_type_declaration env decls defs in
  let item = Signature_item.make vis (Type (rec_, decls)) in
  (env, item)

let import_signature_items env = function
  | [] -> assert false
  | Types.Sig_value (id, vd, vis) :: rest ->
    let vis = import_visibility vis in
    let vd = import_value_description env (Ident.name id) vd in
    let context, binder = Context.fresh env.context (Value vd) in
    let ml_values = Ml.Map.add env.ml_values id binder in
    let env = {env with context; ml_values} in
    let item = Signature_item.make vis (Value (binder, vd)) in
    (env, item, rest)
  | Types.Sig_type (id, td, rec_, vis) :: rest ->
    let defs, rest = gather_recursive_types [(id, td)] vis rest in
    let env, item = import_type_defs env vis rec_ defs in
    (env, item, rest)
  | Types.Sig_module (id, mp, md, rec_, vis) :: rest ->
    let defs, rest = gather_recursive_modules [(id, mp, md)] vis rest in
    let env, item = import_modules env vis rec_ defs in
    (emv, item, rest)
  | Types.Sig_modtype (_id, _mtd, _vis) :: _rest ->
    import_module_type
    assert false
  | (Types.Sig_typext _ | Types.Sig_class _ | Types.Sig_class_type _) :: _ ->
    assert false

  (* Sig_typext of Ident.t * extension_constructor * ext_status * visibility*)
  (* Sig_class of Ident.t * class_declaration * rec_status * visibility*)
  (* Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility*)
