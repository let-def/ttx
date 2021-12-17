open Cmon
open Print_utils

let position {Lexing. pos_fname; pos_lnum; pos_bol; pos_cnum} =
  record [
    "pos_fname" , string pos_fname;
    "pos_lnum"  , int pos_lnum;
    "pos_bol"   , int pos_bol;
    "pos_cnum"  , int pos_cnum;
  ]

let location {Location. loc_ghost; loc_start; loc_end} =
  record [
    "loc_start" , position loc_start;
    "loc_end"   , position loc_end;
    "loc_ghost" , bool loc_ghost;
  ]

let loc f {Location. txt; loc} =
  record [
    "txt", f txt;
    "loc", location loc;
  ]

module Asttypes = struct
  open Asttypes

  let constant = function
    | Const_int x ->
      construct "Const_int" [int x]
    | Const_char x ->
      construct "Const_char" [char x]
    | Const_string (txt, loc, txtopt) ->
      construct "Const_string" [string txt; location loc; option string txtopt]
    | Const_float str ->
      construct "Const_float" [string str]
    | Const_int32 i32 ->
      construct "Const_int32" [int32 i32]
    | Const_int64 i64 ->
      construct "Const_int32" [int64 i64]
    | Const_nativeint n ->
      construct "Const_nativeint" [nativeint n]

  let rec_flag = function
    | Nonrecursive -> construct "Nonrecursive" []
    | Recursive    -> construct "Recursive" []

  let direction_flag = function
    | Upto   -> construct "Upto" []
    | Downto -> construct "Downto" []

  let private_flag = function
    | Private -> construct "Private" []
    | Public  -> construct "Public" []

  let mutable_flag = function
    | Immutable -> construct "Immutable" []
    | Mutable   -> construct "Mutable" []

  let virtual_flag = function
    | Virtual  -> construct "Virtual" []
    | Concrete -> construct "Concrete" []

  let override_flag = function
    | Override -> construct "Override" []
    | Fresh    -> construct "Fresh" []

  let closed_flag = function
    | Closed -> construct "Closed" []
    | Open   -> construct "Open" []

  let label = string

  let arg_label = function
    | Nolabel    -> construct "Nolabel" []
    | Labelled x -> construct "Labelled" [string x]
    | Optional x -> construct "Optional" [string x]

  let variance = function
    | Covariant     -> construct "Covariant" []
    | Contravariant -> construct "Contravariant" []
    | NoVariance    -> construct "NoVariance" []

  let injectivity = function
    | Injective     -> construct "Injective" []
    | NoInjectivity -> construct "NoInjectivity" []
end

let ident t =
  if Ident.global t then
    construct "Global" [string (Ident.name t)]
  else (
    let unique_ids = !Clflags.unique_ids in
    Clflags.unique_ids := true;
    let txt = Format.asprintf "%a" Ident.print_with_scope t in
    Clflags.unique_ids := unique_ids;
    let tag =
      if Ident.is_predef t then
        "Predef"
      else if Ident.scope t = Ident.highest_scope then
        "Local"
      else
        "Scoped"
    in
    construct tag [string txt]
  )

let rec path = function
  | Path.Pident id ->
    construct "Pident" [ident id]
  | Path.Pdot (p, dot) ->
    construct "Pdot" [path p; string dot]
  | Path.Papply (p1, p2) ->
    construct "Papply" [path p1; path p2]

let rec longident = function
  | Longident.Lident var ->
    construct "Lident" [string var]
  | Longident.Ldot (l, dot) ->
    construct "Ldot" [longident l; string dot]
  | Longident.Lapply (l1, l2) ->
    construct "Lapply" [longident l1; longident l2]

module Primitive = struct
  open Primitive

  let boxed_integer = function
    | Pnativeint -> constant "Pnativeint"
    | Pint32     -> constant "Pint32"
    | Pint64     -> constant "Pint64"

  (* Representation of arguments/result for the native code version
     of a primitive *)
  let native_repr = function
    | Same_as_ocaml_repr -> constant "Same_as_ocaml_repr"
    | Unboxed_float      -> constant "Unboxed_float"
    | Unboxed_integer bi -> construct "Unboxed_integer" [boxed_integer bi]
    | Untagged_int       -> constant "Untagged_int"

  let description {
      prim_name;
      prim_arity;
      prim_alloc;
      prim_native_name;
      prim_native_repr_args;
      prim_native_repr_res;
    } =
    record [
      "prim_name"             , string prim_name;
      "prim_arity"            , int prim_arity;
      "prim_alloc"            , bool prim_alloc;
      "prim_native_name"      , string prim_native_name;
      "prim_native_repr_args" , list_map native_repr prim_native_repr_args;
      "prim_native_repr_res"  , native_repr prim_native_repr_res;
    ]

end

let attribute {Parsetree. attr_name; attr_payload=_; attr_loc} =
  record [
    "attr_name"    , loc string attr_name;
    "attr_payload" , constant "TODO";
    "attr_loc"     , location attr_loc;
  ]

let attributes xs =
  list_map attribute xs

let type_immediacy = function
  | Type_immediacy.Unknown          -> constant "Unknown"
  | Type_immediacy.Always           -> constant "Always"
  | Type_immediacy.Always_on_64bits -> constant "Always_on_64bits"

module Types = struct
  open Types

  let rec commutable = function
    | Cok -> construct "Cok" []
    | Cunknown -> construct "Cunknown" []
    | Clink r -> construct "Clink" [cmon_ref commutable r]

  let rec field_kind = function
    | Fvar fkor -> construct "Fvar" [cmon_ref (option field_kind) fkor]
    | Fpresent -> construct "Fpresent" []
    | Fabsent  -> construct "Fabsent" []

  let rec type_expr { id; level; scope; desc } =
    record [
      "id", int id;
      "level", int level;
      "scope", int scope;
      "desc", type_desc desc;
    ]

  and type_desc = function
    | Tvar varopt -> construct "Tvar" [option string varopt]
    | Tarrow (label, cod, dom, commu) ->
      construct "Tarrow" [
        Asttypes.arg_label label;
        type_expr cod;
        type_expr dom;
        commutable commu;
      ]
    | Ttuple args ->
      construct "Ttuple" (List.map type_expr args)
    | Tconstr (p, args, abbrev) ->
      construct "Tconstr" [
        path p;
        list_map type_expr args;
        cmon_ref abbrev_memo abbrev;
      ]
    | Tobject (te, names) ->
      construct "Tobject" [
        type_expr te;
        cmon_ref (option (pair path (list_map type_expr))) names;
      ]
    | Tnil -> construct "Tnil" []
    | Tlink te -> construct "Tlink" [type_expr te]
    | Tsubst (te, teo) ->
      construct "Tsubst" [type_expr te; option type_expr teo]
    | Tvariant rd ->
      construct "Tvariant" [row_desc rd]
    | Tunivar var ->
      construct "Tunivar" [option string var]
    | Tpoly (te, tes) ->
      construct "Tpoly" [type_expr te; list_map type_expr tes]
    | Tpackage (p, constraints) ->
      construct "Tpackage" [
        path p;
        list_map (pair longident type_expr) constraints;
      ]
    | Tfield (txt, kind, t1, t2) ->
      construct "Tfield" [
        string txt;
        field_kind kind;
        type_expr t1;
        type_expr t2;
      ]

  and abbrev_memo = function
    | Mnil -> construct "Mnil" []
    | Mcons (priv, p, t1, t2, abb) ->
      construct "Mcons" [
        Asttypes.private_flag priv;
        path p;
        type_expr t1;
        type_expr t2;
        abbrev_memo abb;
      ]
    | Mlink abb ->
      construct "Mlink" [cmon_ref abbrev_memo abb]

  and row_desc
      { row_fields; row_more; row_bound=(); row_closed; row_fixed; row_name }
    =
    record [
      "row_fields" , list_map (pair Asttypes.label row_field) row_fields;
      "row_more"   , type_expr row_more;
      (*"row_bound"  , unit;*)
      "row_closed" , bool row_closed;
      "row_fixed"  , option fixed_explanation row_fixed;
      "row_name"   , option (pair path (list_map type_expr)) row_name;
    ]

  and fixed_explanation = function
    | Univar te     -> construct "Univar" [type_expr te]
    | Fixed_private -> construct "Fixed_private" []
    | Reified p     -> construct "Reified" [path p]
    | Rigid         -> construct "Rigid" []

  and row_field = function
    | Rpresent teo -> construct "Rpresent" [option type_expr teo]
    | Reither (b1, tes, b2, rfor) ->
      construct "Reither" [bool b1; list_map type_expr tes; bool b2;
                           cmon_ref (option row_field) rfor]
    | Rabsent -> construct "Rabsent" []

  let uid t =
    constant (Format.asprintf "%a" Uid.print t)

  let meths f m =
    construct "Meths.of_list" [list_map (pair string f) (Meths.bindings m)]

  let vars f v =
    construct "Vars.of_list" [list_map (pair string f) (Vars.bindings v)]

  let value_kind = function
    | Val_reg -> constant "Val_reg"
    | Val_prim prim ->
      construct "Val_prim" [Primitive.description prim]
    | Val_ivar (mf, txt) ->
      construct "Val_ivar" [Asttypes.mutable_flag mf; string txt]
    | Val_anc (sil, s) ->
      construct "Val_anc" [list_map (pair string ident) sil; string s]
      (* Ancestor *)
    | Val_self (m, v, txt, te) ->
      construct "Val_self" [
        cmon_ref (meths (pair ident type_expr)) m;
        cmon_ref (vars (tup4 ident Asttypes.mutable_flag Asttypes.virtual_flag type_expr)) v;
        string txt;
        type_expr te;
      ]

  let value_description
      { val_type; val_kind; val_loc; val_attributes; val_uid } =
    record [
      "val_type"       , type_expr val_type;
      "val_kind"       , value_kind val_kind;
      "val_loc"        , location val_loc;
      "val_attributes" , attributes val_attributes;
      "val_uid"        , uid val_uid;
    ]

  let variance_f = function
    | Variance.May_pos  -> constant "May_pos"
    | Variance.May_neg  -> constant "May_neg"
    | Variance.May_weak -> constant "May_weak"
    | Variance.Inj      -> constant "Inj"
    | Variance.Pos      -> constant "Pos"
    | Variance.Neg      -> constant "Neg"
    | Variance.Inv      -> constant "Inv"

  let variance v =
    list_map variance_f @@
    List.filter (fun f -> Variance.mem f v)
      Variance.[May_pos; May_neg; May_weak; Inj; Pos; Neg; Inv]

  let separability = function
    | Separability.Ind     -> constant "Ind"
    | Separability.Sep     -> constant "Sep"
    | Separability.Deepsep -> constant "Deepsep"

  let record_representation = function
    | Record_regular     -> constant "Record_regular"
    | Record_float       -> constant "Record_float"
    | Record_unboxed b   -> construct "Record_unboxed" [bool b]
    | Record_inlined i   -> construct "Record_inlined" [int i]
    | Record_extension p -> construct "Record_extension" [path p]

  let variant_representation = function
    | Variant_regular -> constant "Variant_regular"
    | Variant_unboxed -> constant "Variant_unboxed"

  let label_declaration
      { ld_id; ld_mutable; ld_type; ld_loc; ld_attributes; ld_uid }
    =
    record [
      "ld_id"         , ident ld_id;
      "ld_mutable"    , Asttypes.mutable_flag ld_mutable;
      "ld_type"       , type_expr ld_type;
      "ld_loc"        , location ld_loc;
      "ld_attributes" , attributes ld_attributes;
      "ld_uid"        , uid ld_uid;
    ]

  let constructor_arguments = function
    | Cstr_tuple tes -> construct "Cstr_tuple" [list_map type_expr tes]
    | Cstr_record lds -> construct "Cstr_record" [list_map label_declaration lds]

  let constructor_declaration
    { cd_id; cd_args; cd_res; cd_loc; cd_attributes; cd_uid }
    =
    record [
      "cd_id"  , ident cd_id;
      "cd_args", constructor_arguments cd_args;
      "cd_res" , option type_expr cd_res;
      "cd_loc" , location cd_loc;
      "cd_attributes" , attributes cd_attributes;
      "cd_uid" , uid cd_uid;
    ]

  let type_decl_kind : type_decl_kind -> _ = function
    | Type_abstract -> constant "Type_abstract"
    | Type_open -> constant "Type_open"
    | Type_record (lds, rr) ->
      construct "Type_record"
        [list_map label_declaration lds; record_representation rr]
    | Type_variant (cds, vr) ->
      construct "Type_variant"
        [list_map constructor_declaration cds; variant_representation vr]

  let type_declaration
      { type_params; type_arity; type_kind; type_private; type_manifest;
        type_variance; type_separability; type_is_newtype;
        type_expansion_scope; type_loc; type_attributes; type_immediate;
        type_unboxed_default; type_uid;
      }
    =
    record [
      "type_params"          , list_map type_expr type_params;
      "type_arity"           , int type_arity;
      "type_kind"            , type_decl_kind type_kind;
      "type_private"         , Asttypes.private_flag type_private;
      "type_manifest"        , option type_expr type_manifest;
      "type_variance"        , list_map variance type_variance;
      "type_separability"    , list_map separability type_separability;
      "type_is_newtype"      , bool type_is_newtype;
      "type_expansion_scope" , int type_expansion_scope;
      "type_loc"             , location type_loc;
      "type_attributes"      , attributes type_attributes;
      "type_immediate"       , type_immediacy type_immediate;
      "type_unboxed_default" , bool type_unboxed_default;
      "type_uid"             , uid type_uid;
    ]

  let type_transparence = function
    | Type_public  -> constant "Type_public"
    | Type_new     -> constant "Type_new"
    | Type_private -> constant "Type_private"

  let extension_constructor
      { ext_type_path; ext_type_params; ext_args; ext_ret_type;
        ext_private; ext_loc; ext_attributes; ext_uid } =
    record [
      "ext_type_path"   , path ext_type_path;
      "ext_type_params" , list_map type_expr ext_type_params;
      "ext_args"        , constructor_arguments ext_args;
      "ext_ret_type"    , option type_expr ext_ret_type;
      "ext_private"     , Asttypes.private_flag ext_private;
      "ext_loc"         , location ext_loc;
      "ext_attributes"  , attributes ext_attributes;
      "ext_uid"         , uid ext_uid;
    ]

  let concr c =
    construct "Concr.of_list" [list_map string (Concr.elements c)]

  let class_signature
    { csig_self; csig_vars; csig_concr; csig_inher }
    =
    record [
      "csig_self"  , type_expr csig_self;
      "csig_vars"  , vars (tup3 Asttypes.mutable_flag Asttypes.virtual_flag type_expr) csig_vars;
      "csig_concr" , concr csig_concr;
      "csig_inher" , list_map (pair path (list_map type_expr)) csig_inher;
    ]

  let rec class_type = function
    | Cty_constr (p, tes, ct) ->
      construct "Cty_constr" [path p; list_map type_expr tes; class_type ct]
    | Cty_signature cs ->
      construct "Cty_signature" [class_signature cs]
    | Cty_arrow (al, te, ct) ->
      construct "Cty_arrow" [Asttypes.arg_label al; type_expr te; class_type ct]

  let class_declaration
      { cty_params; cty_type; cty_path; cty_new; cty_variance; cty_loc;
        cty_attributes; cty_uid }
    =
    record [
      "cty_params"     , list_map type_expr cty_params;
      "cty_type"       , class_type cty_type;
      "cty_path"       , path cty_path;
      "cty_new"        , option type_expr cty_new;
      "cty_variance"   , list_map variance cty_variance;
      "cty_loc"        , location cty_loc;
      "cty_attributes" , attributes cty_attributes;
      "cty_uid"        , uid cty_uid;
    ]

  let class_type_declaration
    { clty_params; clty_type; clty_path; clty_variance; clty_loc;
      clty_attributes; clty_uid }
    =
    record [
      "clty_params"     ,  list_map type_expr clty_params;
      "clty_type"       ,  class_type clty_type;
      "clty_path"       ,  path clty_path;
      "clty_variance"   ,  list_map variance clty_variance;
      "clty_loc"        ,  location clty_loc;
      "clty_attributes" ,  attributes clty_attributes;
      "clty_uid"        ,  uid clty_uid;
    ]

  let visibility = function
    | Exported -> constant "Exported"
    | Hidden   -> constant "Hidden"

  let module_presence = function
    | Mp_present -> constant "Mp_present"
    | Mp_absent  -> constant "Mp_absent"

  let rec_status = function
    | Trec_not   -> constant "Trec_not"
    | Trec_first -> constant "Trec_first"
    | Trec_next  -> constant "Trec_next"

  let ext_status = function
    | Text_first     -> constant "Text_first"
    | Text_next      -> constant "Text_next"
    | Text_exception -> constant "Text_exception"

  let rec module_type = function
    | Mty_ident p ->
      construct "Mty_ident" [path p]
    | Mty_signature sg ->
      construct "Mty_signature" [signature sg]
    | Mty_functor (fp, mt) ->
      construct "Mty_functor" [functor_parameter fp; module_type mt]
    | Mty_alias p ->
      construct "Mty_alias" [path p]

  and functor_parameter = function
    | Unit -> constant "Unit"
    | Named (oi, mt) ->
      construct "Named" [option ident oi; module_type mt]

  and signature xs = list_map signature_item xs

  and signature_item = function
    | Sig_value (id, vd, vis) ->
      construct "Sig_value" [ident id; value_description vd; visibility vis]
    | Sig_type (id, td, rs, vis) ->
      construct "Sig_type" [ident id; type_declaration td; rec_status rs; visibility vis]
    | Sig_typext (id, ec, es, vis) ->
      construct "Sig_typext" [ident id; extension_constructor ec; ext_status es; visibility vis]
    | Sig_module (id, mp, md, rs, vis) ->
      construct "Sig_module" [ident id; module_presence mp; module_declaration md; rec_status rs; visibility vis]
    | Sig_modtype (id, mtd, vis) ->
      construct "Sig_modtype" [ident id; modtype_declaration mtd; visibility vis]
    | Sig_class (id, cd, rs, vis) ->
      construct "Sig_class" [ident id; class_declaration cd; rec_status rs; visibility vis]
    | Sig_class_type (id, ctd, rs, vis) ->
      construct "Sig_class_type" [ident id; class_type_declaration ctd; rec_status rs; visibility vis]

  and module_declaration
      { md_type; md_attributes; md_loc; md_uid }
    =
    record [
      "md_type"       , module_type md_type;
      "md_attributes" , attributes md_attributes;
      "md_loc"        , location md_loc;
      "md_uid"        , uid md_uid;
    ]

  and modtype_declaration
      { mtd_type; mtd_attributes; mtd_loc; mtd_uid }
    =
    record [
      "mtd_type"       , option module_type mtd_type;
      "mtd_attributes" , attributes mtd_attributes;
      "mtd_loc"        , location mtd_loc;
      "mtd_uid"        , uid mtd_uid;
    ]

  let constructor_tag = function
    | Cstr_constant n       -> construct "Cstr_constant" [int n]
    | Cstr_block n          -> construct "Cstr_block" [int n]
    | Cstr_unboxed          -> construct "Cstr_unboxed" []
    | Cstr_extension (p, b) -> construct "Cstr_extension" [path p; bool b]

  let constructor_description
      { cstr_name; cstr_res; cstr_existentials; cstr_args; cstr_arity;
        cstr_tag; cstr_consts; cstr_nonconsts; cstr_normal; cstr_generalized;
        cstr_private; cstr_loc; cstr_attributes; cstr_inlined; cstr_uid } =
    record [
      "cstr_name"         , string cstr_name;
      "cstr_res"          , type_expr cstr_res;
      "cstr_existentials" , list_map type_expr cstr_existentials;
      "cstr_args"         , list_map type_expr cstr_args;
      "cstr_arity"        , int cstr_arity;
      "cstr_tag"          , constructor_tag cstr_tag;
      "cstr_consts"       , int cstr_consts;
      "cstr_nonconsts"    , int cstr_nonconsts;
      "cstr_normal"       , int cstr_normal;
      "cstr_generalized"  , bool cstr_generalized;
      "cstr_private"      , Asttypes.private_flag cstr_private;
      "cstr_loc"          , location cstr_loc;
      "cstr_attributes"   , attributes cstr_attributes;
      "cstr_inlined"      , option type_declaration cstr_inlined;
      "cstr_uid"          , uid cstr_uid;
    ]

  let rec label_description
      { lbl_name; lbl_res; lbl_arg; lbl_mut; lbl_pos; lbl_all; lbl_repres;
        lbl_private; lbl_loc; lbl_attributes; lbl_uid }
    =
    record [
      "lbl_name"       , string lbl_name;
      "lbl_res"        , type_expr lbl_res;
      "lbl_arg"        , type_expr lbl_arg;
      "lbl_mut"        , Asttypes.mutable_flag lbl_mut;
      "lbl_pos"        , int lbl_pos;
      "lbl_all"        , array_map label_description lbl_all;
      "lbl_repres"     , record_representation lbl_repres;
      "lbl_private"    , Asttypes.private_flag lbl_private;
      "lbl_loc"        , location lbl_loc;
      "lbl_attributes" , attributes lbl_attributes;
      "lbl_uid"        , uid lbl_uid;
    ]

end
