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

  let fresh context name info =
    extend context (Visitor.namespace info) name info
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
  let simple_level context name vars =
    let _, binder = Context.reserve context Type_level name in
    let level = Type_level.make binder in
    let vars = List.map (Type_level.fresh level) vars in
    Type_level.freeze level;
    (level, vars)

  let abstract_type context vars name =
    let forall, params = simple_level context "forall" vars in
    let mk_var var = Type_expr.make (Var var) in
    Type_decl.make name
      ~forall ~params:(List.map mk_var params)
      ~manifest:None Abstract


  let add context text decl =
    let context, binder = Context.fresh context text decl in
    (context, get_name binder)

  let context = Context.empty

  let context, int =
    add context "int" (Type (abstract_type context [] "int"))

  let context, string =
    add context "string" (Type (abstract_type context [] "string"))

  let context, char =
    add context "char" (Type (abstract_type context [] "char"))

  let context, float =
    add context "float" (Type (abstract_type context [] "float"))

  let context, bool, bool_true, bool_false =
    let context, ident = Context.reserve context Type "bool" in
    let forall, _ = simple_level context "forall" [] in
    let typ = Type_expr.make (Const ([], Path.Ident (get_name ident))) in
    let k_true = Constructor.make "true" ~forall (Tuple []) typ in
    let k_false = Constructor.make "false" ~forall (Tuple []) typ in
    let decl =
      Type_decl.make "bool" ~forall ~params:[] ~manifest:None
        (Variant [k_true; k_false]);
    in
    let context = Context.enter context ident (Type decl) in
    let context, bool_true  = add context "true"  (Constructor k_true)  in
    let context, bool_false = add context "false" (Constructor k_false) in
    (context, get_name ident, bool_true, bool_false)

  let context, list, list_nil, list_cons =
    let context, ident = Context.reserve context Type "list" in
    let forall, vars = simple_level context "forall" [Some "a"] in
    let a = Type_expr.make (Var (List.hd vars)) in
    let typ = Type_expr.make (Const ([a], Path.Ident (get_name ident))) in
    let k_nil = Constructor.make "[]" ~forall (Tuple []) typ in
    let k_cons = Constructor.make "::" ~forall (Tuple [a; typ]) typ in
    let decl =
      Type_decl.make "list" ~forall ~params:[a] ~manifest:None
        (Variant [k_nil; k_cons])
    in
    let context = Context.enter context ident (Type decl) in
    let context, list_nil  = add context "[]" (Constructor k_nil)  in
    let context, list_cons = add context "::" (Constructor k_cons) in
    (context, get_name ident, list_nil, list_cons)

  let context, option, option_none, option_some =
    let context, ident = Context.reserve context Type "option" in
    let forall, vars = simple_level context "forall" [Some "a"] in
    let a = Type_expr.make (Var (List.hd vars)) in
    let typ = Type_expr.make (Const ([a], Path.Ident (get_name ident))) in
    let k_none = Constructor.make "None" ~forall (Tuple []) typ in
    let k_some = Constructor.make "Some" ~forall (Tuple [a]) typ in
    let decl =
      Type_decl.make "option" ~forall ~params:[a] ~manifest:None
        (Variant [k_none; k_some])
    in
    let context = Context.enter context ident (Type decl) in
    let context, option_none = add context "None" (Constructor k_none) in
    let context, option_some = add context "Some" (Constructor k_some) in
    (context, get_name ident, option_none, option_some)
end

let rec import_ml_module_path env : Ml.Path.t -> ns_module path = function
  | Pident id ->
    Path.Ident (get_name (Ml.Map.get env.ml_modules id))
  | Pdot (parent, dot) ->
    Path.Dot (import_ml_module_path env parent, dot)
  | Papply _ ->
    assert false (*TODO*)

let import_ml_path (type ns) env (map : ns binder Ml.map)
  : Ml.Path.t -> ns path = function
  | Pident id ->
    Path.Ident (get_name (Ml.Map.get map id))
  | Pdot (parent, dot) ->
    Path.Dot (import_ml_module_path env parent, dot)
  | Papply _ ->
    assert false

type type_import_state = {
  table: type_expr Ml.Tytable.t;
  level: Type_level.t;
  mutable suspended: bool;
  parent: type_import_state;
}

let new_level name env =
  let context, binder = Context.reserve env.context Type_level name in
  let level = Type_level.make binder in
  let context = Context.enter context binder (Type_level level) in
  let rec self = {
    table = Ml.Tytable.create 7;
    level;
    suspended = false;
    parent = self;
  } in
  (self, {env with context})

let enter name st env =
  assert (not st.suspended);
  let context, binder = Context.reserve env.context Type_level name in
  let level = Type_level.make binder in
  let context = Context.enter context binder (Type_level level) in
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
  let st, env = new_level "forall" env in
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

let import_type_declaration env (binder, ident, (td : Ml.Types.type_declaration)) =
  let st, env_inner = new_level "forall" env in
  let params = List.map (import_type_expr st env_inner) td.type_params in
  let forall = leave st in
  let manifest = Option.map (import_type_expr st env_inner) td.type_manifest in
  let env, desc =
    match td.type_kind with
    | Type_abstract ->
      (env, Type_decl.Abstract)
    | Type_open ->
      (env, Type_decl.Open)
    | Type_variant (cstrs, _repr) ->
      let decls = List.map
          (import_constructor_decl st forall params (get_name binder) env_inner)
          cstrs
      in
      let env = List.fold_left2 (fun env decl cd ->
          let name = Ident.name cd.Ml.Types.cd_id in
          let context, name = Context.fresh env.context name (Constructor decl) in
          let ml_cstrs = Ml.Map.add env.ml_cstrs cd.cd_id name in
          {env with context; ml_cstrs}
        ) env decls cstrs
      in
      (env, Type_decl.Variant decls)
    | Type_record (_labels, _repr) ->
      assert false (*TODO*)
  in
  let decl = Type_decl.make (Ident.name ident) ~forall ~params ~manifest desc in
  let context = Context.enter env.context binder (Type decl) in
  ({env with context}, (binder, decl))

let import_signature_ : (env -> Types.signature -> signature) ref =
  ref (fun _ -> assert false)

let import_type_defs env vis rec_ defs =
  let vis = import_visibility vis in
  let rec_ = import_rec_flag rec_ in
  let (context, ml_types), defs =
    List.fold_left_map
      (fun (context, ml_types) (id, td) ->
         let context, binder = Context.reserve context Type (Ident.name id) in
         let ml_types = Ml.Map.add ml_types id binder in
         (context, ml_types), (binder, id, td)
      )
      (env.context, env.ml_types) defs
  in
  let env = {env with ml_types; context} in
  let env, decls = List.fold_left_map import_type_declaration env defs in
  let item = Signature_item.make vis (Type (rec_, decls)) in
  (env, item)

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
            let name = Ident.name n in
            let md = Module_decl.make name mt' in
            let context, binder = Context.fresh env.context name (Module md) in
            let ml_modules = Ml.Map.add env.ml_modules n binder in
            let env = {env with context; ml_modules} in
            env, Functor_parameter.Named (Some binder, mt')
      in
      Functor (Functor_parameter.make fp, import_module_type env mt)
    )

let import_module env (binder, id, _mp, (md : Ml.Types.module_declaration)) =
  let mty = import_module_type env md.md_type in
  let decl = Module_decl.make (Ident.name id) mty in
  let context = Context.enter env.context binder (Module decl) in
  ({env with context}, (binder, decl))

let import_modules env vis rec_ defs =
  let vis = import_visibility vis in
  let rec_ = import_rec_flag rec_ in
  let (context, ml_modules), defs =
    List.fold_left_map
      (fun (context, ml_modules) (id, mp, md) ->
         let context, binder = Context.reserve context Module (Ident.name id) in
         let ml_modules = Ml.Map.add ml_modules id binder in
         (context, ml_modules), (binder, id, mp, md)
      )
      (env.context, env.ml_modules) defs
  in
  let env = {env with context; ml_modules} in
  let env, decls = List.fold_left_map import_module env defs in
  let item = Signature_item.make vis (Module (rec_, decls)) in
  (env, item)

let import_signature_item env = function
  | [] -> assert false
  | Types.Sig_value (id, vd, vis) :: rest ->
    let name = Ident.name id in
    let vis = import_visibility vis in
    let vd = import_value_description env name vd in
    let context, binder = Context.fresh env.context name (Value vd) in
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
    (env, item, rest)
  | Types.Sig_modtype (id, mtd, vis) :: rest ->
    let name = Ident.name id in
    let vis = import_visibility vis in
    let decl =
      Module_type_decl.make
        (Option.map (import_module_type env) mtd.mtd_type)
    in
    let context, binder = Context.fresh env.context name (Module_type decl) in
    let item = Signature_item.make vis (Module_type (binder, decl)) in
    let ml_modtypes = Ml.Map.add env.ml_modtypes id binder in
    let env = {env with context; ml_modtypes} in
    (env, item, rest)
  | (Types.Sig_typext _ | Types.Sig_class _ | Types.Sig_class_type _) :: _ ->
    assert false

let import_signature_items env items =
  let rec aux env acc = function
    | [] -> env, List.rev acc
    | items ->
      let env, item, rest = import_signature_item env items in
      aux env (item :: acc) rest
  in
  aux env [] items
