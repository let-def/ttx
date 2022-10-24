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
    val of_list : debug_name:string -> (Ident.t * 'a) list -> 'a t
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

    let of_list ~debug_name idents =
      List.fold_left (fun acc (k, v) -> add acc k v) (empty ~debug_name) idents
  end

  type 'a map = 'a Map.t
  module Tytable = Hashtbl.Make(Types.TypeOps)

end

open Ttx_def
open Ttx_types

type 'ns decl =
  | Value       : value_desc -> ns_value decl
  | Type        : type_decl -> ns_type decl
  | Type_level  : type_level -> ns_type_level decl
  | Module      : module_decl -> ns_module decl
  | Import      : string * Digest.t option -> ns_module decl
  | Module_type : module_type_decl -> ns_module_type decl

module Context = Make_context(struct type 'ns t = 'ns decl end)

type env = {
  context: Context.t;
  ml_types: ns_type binder Ml.map;
  ml_modules: ns_module binder Ml.map;
  ml_modtypes: ns_module_type binder Ml.map;
  ml_values: ns_value binder Ml.map;
  ml_cstrs: vc_path Ml.map;
}

module Initial : sig
  val type_unit   : ns_type binder
  val type_int    : ns_type binder
  val type_bool   : ns_type binder
  val type_char   : ns_type binder
  val type_string : ns_type binder
  val type_bytes  : ns_type binder
  val type_float  : ns_type binder
  val type_option : ns_type binder
  val type_list   : ns_type binder

  val bool_false : vc_path
  val bool_true  : vc_path
  val list_nil   : vc_path
  val list_cons  : vc_path
  val option_none  : vc_path
  val option_some  : vc_path

  val env : env
end = struct

  let context = Context.empty

  let names = Context.empty_group context

  let names, type_unit   = Context.extend names Type "unit"
  let names, type_int    = Context.extend names Type "int"
  let names, type_bool   = Context.extend names Type "bool"
  let names, type_char   = Context.extend names Type "char"
  let names, type_string = Context.extend names Type "string"
  let names, type_bytes  = Context.extend names Type "bytes"
  let names, type_float  = Context.extend names Type "float"
  let names, type_option = Context.extend names Type "option"
  let names, type_list   = Context.extend names Type "list"

  let context = Context.enter context names

  let simple_level context name vars =
    let group = Context.empty_group context in
    let group, binder = Context.extend group Type_level name in
    let level = Type_level.make binder in
    List.iter (fun v -> ignore (Type_level.fresh level (Some v))) vars;
    Type_level.freeze level;
    (group, level)

  let predef_loc name =
    let pos =
      let pos_fname = "predef:" ^ name in
      {Lexing. pos_fname; pos_lnum=1; pos_cnum=0; pos_bol=0}
    in
    {loc_start=pos; loc_end=pos; loc_ghost=true}

  let abstract_type context td_binder =
    let group, forall = simple_level context "forall" [] in
    let td_loc = predef_loc (get_text (get_name td_binder)) in
    let td_body =
      Binding.make group forall
        {td_params=[]; td_manifest=None; td_desc=Td_abstract}
    in
    Context.bind context td_binder
      (Type {td_loc; td_attrs = []; td_binder; td_body})

  let context = abstract_type context type_int
  let context = abstract_type context type_char
  let context = abstract_type context type_string
  let context = abstract_type context type_float

  let context, unit_unit =
    let group, forall = simple_level context "forall" [] in
    let path = Path.Ident (get_name type_unit) in
    let vc_def =
      Binding.make group forall (
        Vc_tuple Vector.empty,
        Type_expr.make (Te_const ([], path))
      )
    in
    let unit_unit = {
      vc_loc = predef_loc "()";
      vc_path = {vc_type=path; vc_index=0; vc_name="()"};
      vc_attrs = []; vc_def;
    } in
    let td_body =
      Binding.make group forall {
        td_params=[];
        td_manifest=None;
        td_desc=Td_variant (Vector.of_list [unit_unit]);
      }
    in
    let td = {
      td_loc=predef_loc "unit";
      td_attrs=[];
      td_binder=type_bool;
      td_body;
    } in
    (Context.bind context type_unit (Type td), unit_unit.vc_path)

  let context, bool_false, bool_true =
    let group, forall = simple_level context "forall" [] in
    let path = Path.Ident (get_name type_bool) in
    let vc_def =
      Binding.make group forall (
        Vc_tuple Vector.empty,
        Type_expr.make (Te_const ([], path))
      )
    in
    let bool_false = {
      vc_loc = predef_loc "false";
      vc_path = {vc_type=path; vc_index=0; vc_name="false"};
      vc_attrs = []; vc_def;
    } in
    let bool_true = {
      vc_loc = predef_loc "true";
      vc_path = {vc_type=path; vc_index=1; vc_name="true"};
      vc_attrs = []; vc_def;
    } in
    let td_body =
      Binding.make group forall {
        td_params=[];
        td_manifest=None;
        td_desc=Td_variant (Vector.of_list [bool_false; bool_true]);
      }
    in
    let td = {
      td_loc=predef_loc "bool";
      td_attrs=[];
      td_binder=type_bool;
      td_body;
    } in
    (Context.bind context type_bool (Type td),
      bool_false.vc_path,
      bool_true.vc_path)

  let context, option_none, option_some =
    let group, forall = simple_level context "forall" ["a"] in
    let a = Type_expr.make (Te_var (Type_level.get_var forall 0)) in
    let path = Path.Ident (get_name type_option) in
    let typ = Type_expr.make (Te_const ([a], path)) in
    let option_none = {
      vc_loc = predef_loc "None";
      vc_path = {vc_type=path; vc_index=0; vc_name="None"};
      vc_attrs = [];
      vc_def = Binding.make group forall (Vc_tuple Vector.empty, typ);
    } in
    let option_some = {
      vc_loc = predef_loc "Some";
      vc_path = {vc_type=path; vc_index=1; vc_name="Some"};
      vc_attrs = [];
      vc_def = Binding.make group forall (Vc_tuple (Vector.of_list [a]), typ);
    } in
    let td_body =
      Binding.make group forall {
        td_params=[a];
        td_manifest=None;
        td_desc=Td_variant (Vector.of_list [option_none; option_some]);
      }
    in
    let td = {
      td_loc=predef_loc "option";
      td_attrs=[];
      td_binder=type_option;
      td_body;
    } in
    (Context.bind context type_option (Type td),
      option_none.vc_path,
      option_some.vc_path)

  let context, list_nil, list_cons =
    let group, forall = simple_level context "forall" ["a"] in
    let a = Type_expr.make (Te_var (Type_level.get_var forall 0)) in
    let path = Path.Ident (get_name type_option) in
    let typ = Type_expr.make (Te_const ([a], path)) in
    let list_nil = {
      vc_loc = predef_loc "[]";
      vc_path = {vc_type=path; vc_index=0; vc_name="[]"};
      vc_attrs = [];
      vc_def = Binding.make group forall (Vc_tuple Vector.empty, typ);
    } in
    let list_cons = {
      vc_loc = predef_loc "::";
      vc_path = {vc_type=path; vc_index=1; vc_name="::"};
      vc_attrs = [];
      vc_def = Binding.make group forall (Vc_tuple (Vector.of_list [a; typ]), typ);
    } in
    let td_body =
      Binding.make group forall {
        td_params=[a];
        td_manifest=None;
        td_desc=Td_variant (Vector.of_list [list_nil; list_cons]);
      }
    in
    let td = {
      td_loc=predef_loc "list";
      td_attrs=[];
      td_binder=type_list;
      td_body;
    } in
    (Context.bind context type_list (Type td),
      list_nil.vc_path,
      list_cons.vc_path)

  let env = {
    context;
    ml_modules = Ml.Map.empty ~debug_name:"modules";
    ml_modtypes = Ml.Map.empty ~debug_name:"module types";
    ml_values = Ml.Map.empty ~debug_name:"values";
    ml_types = (
      let of_path = function Ml.Path.Pident id -> id | _ -> assert false in
      Ml.Map.of_list ~debug_name:"types" [
        of_path Predef.path_int    , type_int;
        of_path Predef.path_char   , type_char;
        of_path Predef.path_string , type_string;
        of_path Predef.path_bytes  , type_bytes;
        of_path Predef.path_float  , type_float;
        of_path Predef.path_bool   , type_bool;
        of_path Predef.path_unit   , type_unit;
        of_path Predef.path_list   , type_list;
        of_path Predef.path_option , type_option;
        (*of_path Predef.path_exn;*)
        (*of_path Predef.path_array;*)
        (*of_path Predef.path_nativeint;*)
        (*of_path Predef.path_int32;*)
        (*of_path Predef.path_int64;*)
        (*of_path Predef.path_lazy_t;*)
        (*of_path Predef.path_extension_constructor;*)
        (*of_path Predef.path_floatarray;*)
      ]
    );
    ml_cstrs = Ml.Map.of_list ~debug_name:"constructors" [
        Predef.ident_false , bool_false;
        Predef.ident_true  , bool_true;
        Predef.ident_void  , unit_unit;
        Predef.ident_nil   , list_nil;
        Predef.ident_cons  , list_cons;
        Predef.ident_none  , option_none;
        Predef.ident_some  , option_some;
      ];
  }
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
  group: namegroup;
  level: Type_level.t;
  mutable suspended: bool;
  parent: type_import_state;
}

let new_level name env =
  let group, binder =
    Context.extend (Context.empty_group env.context) Type_level name
  in
  let context = Context.enter env.context group in
  let level = Type_level.make binder in
  let context = Context.bind context binder (Type_level level) in
  let rec self = {
    table = Ml.Tytable.create 7;
    group;
    level;
    suspended = false;
    parent = self;
  } in
  (self, {env with context})

let enter name st env =
  assert (not st.suspended);
  let group, binder =
    Context.extend (Context.empty_group env.context) Type_level name
  in
  let context = Context.enter env.context group in
  let level = Type_level.make binder in
  let context = Context.bind context binder (Type_level level) in
  st.suspended <- true;
  let st' = {table = st.table; group; level; suspended = false; parent = st} in
  (st', {env with context})

let leave st =
  assert (not st.suspended);
  if st.parent != st then (
    assert st.parent.suspended;
    st.parent.suspended <- false;
  );
  st.suspended <- true;
  Type_level.freeze st.level;
  (st.group, st.level)

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
        Te_var (Type_level.fresh st.level name)
      | Tarrow (Nolabel, tlhs, trhs, _) ->
        let lhs = import_type_expr st env tlhs in
        let rhs = import_type_expr st env trhs in
        Te_arrow {lhs; rhs}
      | Ttuple ts ->
        let ts = List.map (import_type_expr st env) ts in
        Te_tuple ts
      | Tconstr (path, params, _) ->
        let path = import_ml_path env env.ml_types path in
        Te_const (List.map (import_type_expr st env) params, path)
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

let import_value_description env binder (vd : Ml.Types.value_description) =
  let st, env = new_level "forall" env in
  let typ = import_type_expr st env vd.val_type in
  let desc = match vd.val_kind with
    | Val_reg -> Vd_regular
    | Val_prim _ -> Vd_primitive
    | Val_ivar _ -> assert false
    | Val_self _ -> assert false
    | Val_anc _ -> assert false
  in
  let group, forall = leave st in
  {
    vd_loc    = vd.val_loc;
    vd_attrs  = vd.val_attributes;
    vd_binder = binder;
    vd_type   = Binding.make group forall typ;
    vd_kind   = desc;
  }


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
    st group forall params typ env vc_index (cd : Ml.Types.constructor_declaration) =
  let arguments = match cd.cd_args with
    | Cstr_record _ -> assert false (*TODO*)
    | Cstr_tuple ts ->
      Vc_tuple (Vector.of_list (List.map (import_type_expr st env) ts))
  in
  let result = match cd.cd_res with
    | Some te -> import_type_expr st env te
    | None -> Type_expr.make (Te_const (params, Path.Ident typ))
  in
  { vc_loc = cd.cd_loc;
    vc_attrs = cd.cd_attributes;
    vc_path = {vc_type=Path.Ident typ; vc_index; vc_name=Ident.name cd.cd_id};
    vc_def = Binding.make group forall (arguments, result);
  }

let import_type_declaration env (binder, (td : Ml.Types.type_declaration)) =
  let st, env_inner = new_level "forall" env in
  let td_params =
    List.map (import_type_expr st env_inner) td.type_params
  in
  let group, forall = leave st in
  let td_manifest =
    Option.map (import_type_expr st env_inner) td.type_manifest
  in
  let td_desc =
    match td.type_kind with
    | Type_abstract -> Td_abstract
    | Type_open -> Td_open
    | Type_variant (cstrs, _repr) ->
      let decls = List.mapi
          (import_constructor_decl st group forall td_params (get_name binder)
             env_inner)
          cstrs
      in
      Td_variant (Vector.of_list decls)
    | Type_record (_labels, _repr) ->
      assert false (*TODO*)
  in
  let decl = {
    td_loc = td.type_loc;
    td_attrs = td.type_attributes;
    td_binder = binder;
    td_body = Binding.make group forall {td_params; td_manifest; td_desc};
  } in
  let context = Context.bind env.context binder (Type decl) in
  ({env with context}, decl)

let import_signature_ : (env -> Types.signature -> signature) ref =
  ref (fun _ -> assert false)

let import_type_defs env rec_ defs =
  let (group, ml_types), defs =
    List.fold_left_map
      (fun (group, ml_types) (id, td) ->
         let group, binder = Context.extend group Type (Ident.name id) in
         let ml_types = Ml.Map.add ml_types id binder in
         (group, ml_types), (binder, td)
      )
      (Context.empty_group env.context, env.ml_types) defs
  in
  let env = {env with ml_types; context = Context.enter env.context group} in
  let env, decls = List.fold_left_map import_type_declaration env defs in
  (env, group, Si_type (import_rec_flag rec_, decls))

let rec import_module_type env : Ml.Types.module_type -> _ = function
  | Mty_ident p ->
    Mt_ident (import_ml_path env env.ml_modtypes p)
  | Mty_alias p ->
    Mt_alias (import_ml_path env env.ml_modules p)
  | Mty_signature sg ->
    Mt_signature (!import_signature_ env sg)
  | Mty_functor (fp, mt) ->
    let group = Context.empty_group env.context in
    let env, group, fp = match fp with
      | Unit ->
        env, group, Fp_unit
      | Named (name, mt') ->
        let md_type = import_module_type env mt' in
        match name with
        | None ->
          env, group, Fp_anonymous md_type
        | Some n ->
          let name = Ident.name n in
          let group, md_binder = Context.extend group Module name in
          let md = {md_loc=location_none; md_attrs=[]; md_binder; md_type} in
          let ml_modules = Ml.Map.add env.ml_modules n md_binder in
          let context =
            Context.bind
              (Context.enter env.context group)
              md_binder (Module md)
          in
          let env = {env with context; ml_modules} in
          env, group, Fp_named md
    in
    Mt_functor (Binding.make group fp (import_module_type env mt))

let import_module env (binder, _mp, (md : Ml.Types.module_declaration)) =
  let mty = import_module_type env md.md_type in
  let decl = {
    md_loc = md.md_loc;
    md_attrs = md.md_attributes;
    md_binder = binder;
    md_type = mty;
  } in
  let context = Context.bind env.context binder (Module decl) in
  ({env with context}, decl)

let import_modules env rec_ defs =
  let (group, ml_modules), defs =
    List.fold_left_map
      (fun (group, ml_modules) (id, mp, md) ->
         let group, binder = Context.extend group Module (Ident.name id) in
         let ml_modules = Ml.Map.add ml_modules id binder in
         (group, ml_modules), (binder, mp, md)
      )
      (Context.empty_group env.context, env.ml_modules) defs
  in
  let env = {env with ml_modules; context = Context.enter env.context group} in
  let env, decls = List.fold_left_map import_module env defs in
  (env, group, Si_module (import_rec_flag rec_, decls))

let reserve_one_name env ns name =
  let ng = Context.empty_group env.context in
  let ng, binder = Context.extend ng ns name in
  let context = Context.enter env.context ng in
  ({env with context}, ng, binder)

let import_signature_item env = function
  | [] -> assert false
  | Types.Sig_value (id, vd, vis) :: rest ->
    let env, ng, binder = reserve_one_name env Value (Ident.name id) in
    let vd = import_value_description env binder vd in
    let context = Context.bind env.context binder (Value vd) in
    let ml_values = Ml.Map.add env.ml_values id binder in
    let env = {env with context; ml_values} in
    (env, (ng, vis, Si_value vd), rest)
  | Types.Sig_type (id, td, rec_, vis) :: rest ->
    let defs, rest = gather_recursive_types [(id, td)] vis rest in
    let env, ng, item = import_type_defs env rec_ defs in
    (env, (ng, vis, item), rest)
  | Types.Sig_module (id, mp, md, rec_, vis) :: rest ->
    let defs, rest = gather_recursive_modules [(id, mp, md)] vis rest in
    let env, ng, item = import_modules env rec_ defs in
    (env, (ng, vis, item), rest)
  | Types.Sig_modtype (id, mtd, vis) :: rest ->
    let env, ng, binder = reserve_one_name env Module_type (Ident.name id) in
    let decl = {
      mtd_loc = mtd.mtd_loc;
      mtd_attrs = mtd.mtd_attributes;
      mtd_binder = binder;
      mtd_def = (match mtd.mtd_type with
          | None -> Mtd_abstract
          | Some mt -> Mtd_concrete (import_module_type env mt)
        );
    } in
    let context = Context.bind env.context binder (Module_type decl) in
    let ml_modtypes = Ml.Map.add env.ml_modtypes id binder in
    let env = {env with context; ml_modtypes} in
    (env, (ng, vis, Si_module_type decl), rest)
  | (Types.Sig_typext _ | Types.Sig_class _ | Types.Sig_class_type _) :: _ ->
    assert false

let import_signature env items =
  let rec aux env acc = function
    | [] -> acc
    | items ->
      let env, item, rest = import_signature_item env items in
      aux env (item :: acc) rest
  in
  let rev_items = aux env [] items in
  List.fold_left (fun sg (ng, vis, si_desc) ->
      let si_visibility = match vis with
        | Types.Exported -> Si_exported
        | Types.Hidden   -> Si_hidden
      in
      S_item (Binding.make ng {si_visibility; si_desc} sg)
    ) S_done rev_items

let () = import_signature_ := import_signature

type modname = string

type import = {
  imp_binder: ns_module binder;
  imp_digest: Digest.t option;
}

type cmi = {
  cmi_modname: string;
  cmi_digest: Digest.t;
  cmi_body: (import list, Ttx_types.signature) binding;
  cmi_flags: [`Rectypes | `Opaque | `Unsafe_string] list;
}

let import_cmi {Cmi_format. cmi_name; cmi_sign; cmi_crcs; cmi_flags} =
  let cmi_flags =
    List.filter_map (function
        | Cmi_format.Rectypes -> Some `Rectypes
        | Cmi_format.Opaque   -> Some `Opaque
        | Cmi_format.Unsafe_string -> Some `Unsafe_string
        | Cmi_format.Alerts _ -> None
      ) cmi_flags
  in
  let cmi_digest, ng, imports =
    List.fold_left (fun (cmi_digest, ng, imports) (imp_name, imp_digest) ->
        if imp_name = cmi_name then (
          assert (Option.is_none cmi_digest);
          (imp_digest, ng, imports)
        ) else (
          let ng, imp_binder = Context.extend ng Module imp_name in
          (cmi_digest, ng, {imp_binder; imp_digest} :: imports)
        )
      ) (None, Context.empty_group Initial.env.context, []) cmi_crcs
  in
  let cmi_digest = match cmi_digest with
    | None ->
      failwith
        ("Ttx_import.import_cmi: interface " ^ cmi_name ^ " has no digest")
    | Some d -> d
  in
  let context, ml_modules =
    List.fold_left (fun (ctx, map) {imp_binder; imp_digest} ->
        let name = get_text (get_name imp_binder) in
        let ctx = Context.bind ctx imp_binder (Import (name, imp_digest)) in
        let map = Ml.Map.add map (Ident.create_persistent name) imp_binder in
        (ctx, map)
    ) (Context.enter Initial.env.context ng, Initial.env.ml_modules) imports
  in
  let sign = import_signature {Initial.env with context; ml_modules} cmi_sign in
  let cmi_body = Binding.make ng imports sign in
  {cmi_modname=cmi_name; cmi_digest; cmi_body; cmi_flags}
