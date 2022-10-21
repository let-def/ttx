open Cmon
open Print_utils
open Types_printer
open Typedtree

module Env = struct
  open Env

  let value_unbound_reason = function
    | Val_unbound_instance_variable ->
      constant "Val_unbound_instance_variable"
    | Val_unbound_self ->
      constant "Val_unbound_self"
    | Val_unbound_ancestor ->
      constant "Val_unbound_ancestor"
    | Val_unbound_ghost_recursive loc ->
      construct "Val_unbound_ghost_recursive" [location loc]

  let module_unbound_reason = function
    | Mod_unbound_illegal_recursion ->
      constant "Mod_unbound_illegal_recursion"

  let summary_table : summary Memo.t = Memo.create ()

  let rec summary s =
    match Memo.lookup summary_table s with
    | Some cmon -> cmon
    | None ->
      let cmon = summary' s in
      Memo.add summary_table s cmon;
      cmon

  and summary' = function
    | Env_empty -> constant "Env_empty"
    | Env_value (sm, id, vd) ->
      construct "Env_value" [summary sm; ident id;
                             Types.value_description vd]
    | Env_type (sm, id, td) ->
      construct "Env_type" [summary sm; ident id;
                            Types.type_declaration td]
    | Env_extension (sm, id, ec) ->
      construct "Env_extension" [summary sm; ident id;
                            Types.extension_constructor ec]
    | Env_module (sm, id, mp, md) ->
      construct "Env_module" [summary sm; ident id;
                            Types.module_presence mp;
                            Types.module_declaration md]
    | Env_modtype (sm, id, mtd) ->
      construct "Env_modtype" [summary sm; ident id;
                            Types.modtype_declaration mtd]
    | Env_class (sm, id, cd) ->
      construct "Env_class" [summary sm; ident id;
                            Types.class_declaration cd]
    | Env_cltype (sm, id, ctd) ->
      construct "Env_cltype" [summary sm; ident id;
                            Types.class_type_declaration ctd]
    | Env_open (sm, p) ->
      construct "Env_open" [summary sm; path p]
    | Env_functor_arg (sm, id) ->
      construct "Env_functor_arg" [summary sm; ident id]
    | Env_constraints (sm, td_pm) ->
      let print_pm pm = list_map (pair path Types.type_declaration) pm in
      construct "Env_constraints" [summary sm;
                                   print_pm (Path.Map.bindings td_pm)]
    | Env_copy_types sm ->
      construct "Env_copy_types" [summary sm; ]
    | Env_persistent (sm, id) ->
      construct "Env_persistent" [summary sm; ident id]
    | Env_value_unbound (sm, txt, vur) ->
      construct "Env_value_unbound" [summary sm; string txt;
                                     value_unbound_reason vur
                                    ]
    | Env_module_unbound (sm, txt, mur) ->
      construct "Env_module_unbound" [summary sm; string txt;
                                     module_unbound_reason mur]

  let env t =
    construct "env_summary" [summary (Env.summary t)]
end

let parsetree_pattern _ =
  constant "(TODO : Parsetree.pattern)"

let partial = function
  | Partial -> constant "Partial"
  | Total   -> constant "Total"

let pattern_category (type a) : a pattern_category -> _ = function
  | Value       -> constant "Value"
  | Computation -> constant "Computation"

let meth = function
  | Tmeth_name txt -> construct "Tmeth_name" [string txt]
  | Tmeth_val id -> construct "Tmeth_val" [ident id]

let rec pat_extra = function
  | Tpat_constraint ct ->
    construct "Tpat_constraint" [core_type ct]
  | Tpat_type (p, li) ->
    construct "Tpat_type" [path p; loc longident li]
  | Tpat_open (p, li, env) ->
    construct "Tpat_open" [path p; loc longident li; Env.env env]
  | Tpat_unpack ->
    constant "Tpat_unpack"

and pattern_data
  : type a. (a -> Cmon.t) -> a pattern_data -> Cmon.t =
  fun pd {pat_desc; pat_loc; pat_extra=pe; pat_type; pat_env; pat_attributes} ->
  record [
    "pat_desc"       , pd pat_desc;
    "pat_loc"        , location pat_loc;
    "pat_extra"      , list_map (tup3 pat_extra location attributes) pe;
    "pat_type"       , Types.type_expr pat_type;
    "pat_env"        , Env.env pat_env;
    "pat_attributes" , attributes pat_attributes;
  ]

and pattern_desc : type a . a pattern_desc -> Cmon.t = function
  | Tpat_any -> constant "Tpat_any"
  | Tpat_var (id, sl) ->
    construct "Tpat_var" [ident id; loc string sl]
  | Tpat_alias (gp, id, sl) ->
    construct "Tpat_alias" [general_pattern gp; ident id; loc string sl]
  | Tpat_constant c ->
    construct "Tpat_constant" [Asttypes.constant c]
  | Tpat_tuple gps ->
    construct "Tpat_tuple" [list_map general_pattern gps]
  | Tpat_construct (li, cd, gps, exists) ->
    construct "Tpat_construct" [
      loc longident li;
      Types.constructor_description cd;
      list_map general_pattern gps;
      option (pair (list_map (loc ident)) core_type) exists;
    ]
  | Tpat_variant (lbl, gpo, rdr) ->
    construct "Tpat_variant" [
      Asttypes.label lbl;
      option general_pattern gpo;
      cmon_ref Types.row_desc rdr;
    ]
  | Tpat_record (fields, cf) ->
    let pf = tup3 (loc longident) Types.label_description general_pattern in
    construct "Tpat_record" [list_map pf fields; Asttypes.closed_flag cf]
  | Tpat_array gps ->
    construct "Tpat_array" [array_map general_pattern (Array.of_list gps)]
  | Tpat_lazy gp ->
    construct "Tpat_lazy" [general_pattern gp]
  | Tpat_value va ->
    construct "Tpat_value" [tpat_value_argument va]
  | Tpat_exception gp ->
    construct "Tpat_exception" [general_pattern gp]
  | Tpat_or (gp1, gp2, rdo) ->
    construct "Tpat_or" [general_pattern gp1; general_pattern gp2;
                         option Types.row_desc rdo]

and general_pattern : type k. k pattern_desc pattern_data -> Cmon.t =
  fun p -> pattern_data pattern_desc p

and tpat_value_argument (tva : tpat_value_argument) : Cmon.t =
  general_pattern (tva :> value general_pattern)

and expression
  { exp_desc; exp_loc; exp_extra=ee; exp_type; exp_env; exp_attributes }
  =
  record [
    "exp_desc"       , expression_desc exp_desc;
    "exp_loc"        , location exp_loc;
    "exp_extra"      , list_map (tup3 exp_extra location attributes) ee;
    "exp_type"       , Types.type_expr exp_type;
    "exp_env"        , Env.env exp_env;
    "exp_attributes" , attributes exp_attributes;
  ]

and exp_extra = function
  | Texp_constraint ct ->
    construct "Texp_constraint" [core_type ct]
  | Texp_coerce (cto, ct) ->
    construct "Texp_coerce" [option core_type cto; core_type ct]
  | Texp_poly cto ->
    construct "Texp_poly" [option core_type cto]
  | Texp_newtype txt ->
    construct "Texp_newtype" [string txt]

and expression_desc = function
  | Texp_ident (p, li, vd) ->
    construct "Texp_ident" [path p; loc longident li;
                            Types.value_description vd]
  | Texp_constant cst ->
    construct "Texp_constant" [Asttypes.constant cst]
  | Texp_let (rf, vbs, exp) ->
    construct "Texp_let" [
      Asttypes.rec_flag rf;
      list_map value_binding vbs;
      expression exp;
    ]
  | Texp_function { arg_label; param; cases; partial=pt } ->
    crecord "Texp_function" [
      "arg_label", Asttypes.arg_label arg_label;
      "param", ident param;
      "cases", list_map case cases;
      "partial", partial pt;
    ]
  | Texp_apply (exp, args) ->
    construct "Texp_apply" [
      expression exp;
      list_map (pair Asttypes.arg_label (option expression)) args;
    ]
  | Texp_match (exp, cases, pt) ->
    construct "Texp_match" [
      expression exp;
      list_map case cases;
      partial pt;
    ]
  | Texp_try (exp, cases) ->
    construct "Texp_try" [
      expression exp;
      list_map case cases;
    ]
  | Texp_tuple ts ->
    construct "Texp_tuple" [list_map expression ts]
  | Texp_construct (li, cd, args) ->
    construct "Texp_construct" [
      loc longident li;
      Types.constructor_description cd;
      list_map expression args;
    ]
  | Texp_variant (label, eo) ->
    construct "Texp_variant" [Asttypes.label label; option expression eo]
  | Texp_record { fields; representation; extended_expression } ->
    let pfield = pair Types.label_description record_label_definition in
    crecord "Texp_record" [
      "fields"              , array_map pfield fields;
      "representation"      , Types.record_representation representation;
      "extended_expression" , option expression extended_expression;
    ]
  | Texp_field (e, li, ld) ->
    construct "Texp_field" [
      expression e;
      loc longident li;
      Types.label_description ld;
    ]
  | Texp_setfield (e1, li, ld, e2) ->
    construct "Texp_setfield" [
      expression e1;
      loc longident li;
      Types.label_description ld;
      expression e2;
    ]
  | Texp_array es ->
    construct "Texp_array" [list_map expression es]
  | Texp_ifthenelse (e1, e2, e3o) ->
    construct "Texp_ifthenelse" [expression e1; expression e2;
                                 option expression e3o]
  | Texp_sequence (e1, e2) ->
    construct "Texp_sequence" [expression e1; expression e2]
  | Texp_while (e1, e2) ->
    construct "Texp_while" [expression e1; expression e2]
  | Texp_assert e ->
    construct "Texp_assert" [expression e]
  | Texp_lazy e ->
    construct "Texp_lazy" [expression e]
  | Texp_unreachable ->
    constant "Texp_unreachable"
  | Texp_extension_constructor (li, p) ->
    construct "Texp_extension_constructor" [loc longident li; path p]
  | Texp_open (od, e) ->
    construct "Texp_open" [open_declaration od; expression e]
  | Texp_for (id, p, e1, e2, df, e3) ->
    construct "Texp_for" [ident id; parsetree_pattern p;
                          expression e1; expression e2;
                          Asttypes.direction_flag df;
                          expression e3]
  | Texp_send (e, m, eo) ->
    construct "Texp_send" [expression e; meth m; option expression eo]
  | Texp_new (p, li, cd) ->
    construct "Texp_new" [path p; loc longident li;
                          Types.class_declaration cd]
  | Texp_instvar (p1, p2, sl) ->
    construct "Texp_instvar" [path p1; path p2; loc string sl]
  | Texp_setinstvar (p1, p2, sl, e) ->
    construct "Texp_setinstvar" [path p1; path p2; loc string sl;
                                 expression e]
  | Texp_override (p, xs) ->
    construct "Texp_override" [path p;
                               list_map (tup3 path (loc string) expression) xs]
  | Texp_letmodule (ido, sol, mp, me, e) ->
    construct "Texp_letmodule" [
      option ident ido;
      loc (option string) sol;
      Types.module_presence mp;
      module_expr me;
      expression e;
    ]
  | Texp_letexception (ec, e) ->
    construct "Texp_letexception" [
      extension_constructor ec;
      expression e;
    ]
  | Texp_object (cs, ss) ->
    construct "Texp_object" [
      class_structure cs;
      list_map string ss;
    ]
  | Texp_pack me ->
    construct "Texp_pack" [module_expr me]
  | Texp_letop { let_; ands; param; body; partial=pt } ->
    crecord "Texp_letop" [
      "let_"    , binding_op let_;
      "ands"    , list_map binding_op ands;
      "param"   , ident param;
      "body"    , case body;
      "partial" , partial pt;
    ]

and case : type k. k case -> Cmon.t =
  fun { c_lhs; c_guard; c_rhs } ->
  record [
    "c_lhs"   , general_pattern c_lhs;
    "c_guard" , option expression c_guard;
    "c_rhs"   , expression c_rhs;
  ]

and record_label_definition = function
  | Kept te ->
    construct "Kept" [Types.type_expr te]
  | Overridden (li, e) ->
    construct "Overridden" [loc longident li; expression e]

and binding_op
    { bop_op_path; bop_op_name; bop_op_val; bop_op_type; bop_exp; bop_loc }
  =
  record [
    "bop_op_path" , path bop_op_path;
    "bop_op_name" , loc string bop_op_name;
    "bop_op_val"  , Types.value_description bop_op_val;
    "bop_op_type" , Types.type_expr bop_op_type;
    "bop_exp"     , expression bop_exp;
    "bop_loc"     , location bop_loc;
  ]

and class_expr
    { cl_desc; cl_loc; cl_type; cl_env; cl_attributes; }
  =
  record [
    "cl_desc"       , class_expr_desc cl_desc;
    "cl_loc"        , location cl_loc;
    "cl_type"       , Types.class_type cl_type;
    "cl_env"        , Env.env cl_env;
    "cl_attributes" , attributes cl_attributes;
  ]

and class_expr_desc = function
  | Tcl_ident (p, lil, cts) ->
    construct "Tcl_ident" [path p; loc longident lil; list_map core_type cts]
  | Tcl_structure cs ->
    construct "Tcl_structure" [class_structure cs]
  | Tcl_fun (lbl, pat, args, ce, pt) ->
    construct "Tcl_fun" [
      Asttypes.arg_label lbl;
      general_pattern pat;
      list_map (pair ident expression) args;
      class_expr ce;
      partial pt
    ]
  | Tcl_apply (ce, args) ->
    construct "Tcl_apply" [
      class_expr ce;
      list_map (pair Asttypes.arg_label (option expression)) args;
    ]
  | Tcl_let (rf, vbs, bindings, ce) ->
    construct "Tcl_let" [
      Asttypes.rec_flag rf;
      list_map value_binding vbs;
      list_map (pair ident expression) bindings;
      class_expr ce;
    ]
  | Tcl_constraint (ce, cto, ss1, ss2, concr) ->
    construct "Tcl_constraint" [
      class_expr ce;
      option class_type cto;
      list_map string ss1;
      list_map string ss2;
      Types.concr concr;
    ]
  | Tcl_open (od, ce) ->
    construct "Tcl_open" [
      open_description od;
      class_expr ce;
    ]

and class_structure
    { cstr_self; cstr_fields; cstr_type; cstr_meths }
  =
  record [
    "cstr_self"   ,  general_pattern cstr_self;
    "cstr_fields" ,  list_map class_field cstr_fields;
    "cstr_type"   ,  Types.class_signature cstr_type;
    "cstr_meths"  ,  Types.meths ident cstr_meths;
  ]

and class_field
    { cf_desc; cf_loc; cf_attributes }
  =
  record [
    "cf_desc"       , class_field_desc cf_desc;
    "cf_loc"        , location cf_loc;
    "cf_attributes" , attributes cf_attributes;
  ]

and class_field_kind = function
  | Tcfk_virtual ct ->
    construct "Tcfk_virtual" [core_type ct]
  | Tcfk_concrete (ovf, e) ->
    construct "Tcfk_concrete" [Asttypes.override_flag ovf; expression e]

and class_field_desc = function
  | Tcf_inherit (ovf, ce, so, sil1, sil2) ->
    construct "Tcf_inherit" [
      Asttypes.override_flag ovf;
      class_expr ce;
      option string so;
      list_map (pair string ident) sil1;
      list_map (pair string ident) sil2;
    ]
  | Tcf_val (sl, mf, id, cfk, b) ->
    construct "Tcf_val" [
      loc string sl;
      Asttypes.mutable_flag mf;
      ident id;
      class_field_kind cfk;
      bool b;
    ]
  | Tcf_method (sl, pf, cfk) ->
    construct "Tcf_method" [
      loc string sl;
      Asttypes.private_flag pf;
      class_field_kind cfk;
    ]
  | Tcf_constraint (ct1, ct2) ->
    construct "Tcf_constraint" [
      core_type ct1;
      core_type ct2;
    ]
  | Tcf_initializer e ->
    construct "Tcf_initializer" [
      expression e;
    ]
  | Tcf_attribute attr ->
    construct "Tcf_attribute" [
      attribute attr;
    ]

and module_expr
    { mod_desc; mod_loc; mod_type; mod_env; mod_attributes }
  =
  record [
    "mod_desc"       , module_expr_desc mod_desc;
    "mod_loc"        , location mod_loc;
    "mod_type"       , Types.module_type mod_type;
    "mod_env"        , Env.env mod_env;
    "mod_attributes" , attributes mod_attributes;
  ]

and module_type_constraint = function
  | Tmodtype_implicit -> constant "Tmodtype_implicit"
  | Tmodtype_explicit mt -> construct "Tmodtype_explicit" [module_type mt]

and functor_parameter = function
  | Unit -> constant "Unit"
  | Named (ido, sol, mt) ->
    construct "Named" [
      option ident ido;
      loc (option string) sol;
      module_type mt;
    ]

and module_expr_desc = function
  | Tmod_ident (p, lil) ->
    construct "Tmod_ident" [path p; loc longident lil]
  | Tmod_structure str ->
    construct "Tmod_structure" [structure str]
  | Tmod_functor (fp, me) ->
    construct "Tmod_functor" [functor_parameter fp; module_expr me]
  | Tmod_apply (me1, me2, mc) ->
    construct "Tmod_apply"
      [module_expr me1; module_expr me2; module_coercion mc]
  | Tmod_constraint (me, mt, mtc, mc) ->
    construct "Tmod_constraint" [
      module_expr me;
      Types.module_type mt;
      module_type_constraint mtc;
      module_coercion mc;
    ]
  | Tmod_unpack (e, mt) ->
    construct "Tmod_unpack" [
      expression e;
      Types.module_type mt;
    ]

and structure { str_items; str_type; str_final_env } =
  record [
    "str_items"     , list_map structure_item str_items;
    "str_type"      , Types.signature str_type;
    "str_final_env" , Env.env str_final_env;
  ]

and structure_item { str_desc; str_loc; str_env } =
  record [
    "str_desc" , structure_item_desc str_desc;
    "str_loc"  , location str_loc;
    "str_env"  , Env.env str_env;
  ]

and structure_item_desc = function
  | Tstr_eval (e, attrs) ->
    construct "Tstr_eval" [expression e; attributes attrs]
  | Tstr_value (rf, vbs) ->
    construct "Tstr_value" [Asttypes.rec_flag rf; list_map value_binding vbs]
  | Tstr_primitive vd ->
    construct "Tstr_primitive" [value_description vd]
  | Tstr_type (rf, tds) ->
    construct "Tstr_type" [Asttypes.rec_flag rf; list_map type_declaration tds]
  | Tstr_typext te ->
    construct "Tstr_typext" [type_extension te]
  | Tstr_exception te ->
    construct "Tstr_exception" [type_exception te]
  | Tstr_module mb ->
    construct "Tstr_module" [module_binding mb]
  | Tstr_recmodule mbs ->
    construct "Tstr_recmodule" [list_map module_binding mbs]
  | Tstr_modtype mtd ->
    construct "Tstr_modtype" [module_type_declaration mtd]
  | Tstr_open od ->
    construct "Tstr_open" [open_declaration od]
  | Tstr_class cdssl ->
    construct "Tstr_class"
      [list_map (pair class_declaration (list_map string)) cdssl]
  | Tstr_class_type islctdl ->
    construct "Tstr_class_type"
      [list_map (tup3 ident (loc string) class_type_declaration) islctdl]
  | Tstr_include id ->
    construct "Tstr_include" [include_declaration id]
  | Tstr_attribute attr ->
    construct "Tstr_attribute" [attribute attr]

and module_binding
    { mb_id; mb_name; mb_presence; mb_expr; mb_attributes; mb_loc }
  =
  record [
    "mb_id"         , option ident mb_id;
    "mb_name"       , loc (option string) mb_name;
    "mb_presence"   , Types.module_presence mb_presence;
    "mb_expr"       , module_expr mb_expr;
    "mb_attributes" , attributes mb_attributes;
    "mb_loc"        , location mb_loc;
  ]

and value_binding
  { vb_pat; vb_expr; vb_attributes; vb_loc }
  =
  record [
    "vb_pat", general_pattern vb_pat;
    "vb_expr", expression vb_expr;
    "vb_attributes", attributes vb_attributes;
    "vb_loc", location vb_loc;
  ]

and module_coercion = function
  | Tcoerce_none ->
    construct "Tcoerce_none" []
  | Tcoerce_structure (mcl, imcl) ->
    construct "Tcoerce_structure" [
      list_map (pair int module_coercion) mcl;
      list_map (tup3 ident int module_coercion) imcl;
    ]
  | Tcoerce_functor (mc1, mc2) ->
    construct "Tcoerce_functor" [module_coercion mc1; module_coercion mc2]
  | Tcoerce_primitive pc ->
    construct "Tcoerce_primitive" [primitive_coercion pc]
  | Tcoerce_alias (e, p, mc) ->
    construct "Tcoerce_alias" [Env.env e; path p; module_coercion mc]

and module_type
    { mty_desc; mty_type; mty_env; mty_loc; mty_attributes }
  =
  record [
    "mty_desc"       , module_type_desc mty_desc;
    "mty_type"       , Types.module_type mty_type;
    "mty_env"        , Env.env mty_env;
    "mty_loc"        , location mty_loc;
    "mty_attributes" , attributes mty_attributes;
  ]

and module_type_desc = function
  | Tmty_ident (p, lil) ->
    construct "Tmty_ident" [path p; loc longident lil]
  | Tmty_signature sg ->
    construct "Tmty_signature" [signature sg]
  | Tmty_functor (fp, mt) ->
    construct "Tmty_functor" [functor_parameter fp; module_type mt]
  | Tmty_with (mt, pllwcl) ->
    construct "Tmty_with" [
      module_type mt;
      list_map (tup3 path (loc longident) with_constraint) pllwcl
    ]
  | Tmty_typeof me ->
    construct "Tmty_typeof" [module_expr me]
  | Tmty_alias (p, lil) ->
    construct "Tmty_alias" [path p; loc longident lil]

and primitive_coercion { pc_desc; pc_type; pc_env; pc_loc } =
  record [
    "pc_desc" , Primitive.description pc_desc;
    "pc_type" , Types.type_expr pc_type;
    "pc_env"  , Env.env pc_env;
    "pc_loc"  , location pc_loc;
  ]

and signature { sig_items; sig_type; sig_final_env } =
  record [
    "sig_items"     , list_map signature_item sig_items;
    "sig_type"      , Types.signature sig_type;
    "sig_final_env" , Env.env sig_final_env;
  ]

and signature_item { sig_desc; sig_env; sig_loc } =
  record [
    "sig_desc" , signature_item_desc sig_desc;
    "sig_env"  , Env.env sig_env;
    "sig_loc"  , location sig_loc;
  ]

and signature_item_desc = function
  | Tsig_value vd ->
    construct "Tsig_value" [value_description vd]
  | Tsig_type (rf, tds) ->
    construct "Tsig_type" [Asttypes.rec_flag rf; list_map type_declaration tds]
  | Tsig_typesubst tds  ->
    construct "Tsig_typesubst" [list_map type_declaration tds]
  | Tsig_typext te ->
    construct "Tsig_typext" [type_extension te]
  | Tsig_exception te ->
    construct "Tsig_exception" [type_exception te]
  | Tsig_module md ->
    construct "Tsig_module" [module_declaration md]
  | Tsig_modsubst ms ->
    construct "Tsig_modsubst" [module_substitution ms]
  | Tsig_recmodule mds ->
    construct "Tsig_recmodule" [list_map module_declaration mds]
  | Tsig_modtype mtd ->
    construct "Tsig_modtype" [module_type_declaration mtd]
  | Tsig_modtypesubst mtd ->
    construct "Tsig_modtypesubst" [module_type_declaration mtd]
  | Tsig_open od ->
    construct "Tsig_open" [open_description od]
  | Tsig_include id ->
    construct "Tsig_include" [include_description id]
  | Tsig_class cds ->
    construct "Tsig_class" [list_map class_description cds]
  | Tsig_class_type ctds ->
    construct "Tsig_class_type" [list_map class_type_declaration ctds]
  | Tsig_attribute attr ->
    construct "Tsig_attribute" [attribute attr]

and module_declaration
    { md_id; md_name; md_presence; md_type; md_attributes; md_loc }
  =
  record [
    "md_id"         , option ident md_id;
    "md_name"       , loc (option string) md_name;
    "md_presence"   , Types.module_presence md_presence;
    "md_type"       , module_type md_type;
    "md_attributes" , attributes md_attributes;
    "md_loc"        , location md_loc;
  ]

and module_substitution
    { ms_id; ms_name; ms_manifest; ms_txt; ms_attributes; ms_loc }
  =
  record [
    "ms_id"         , ident ms_id;
    "ms_name"       , loc string ms_name;
    "ms_manifest"   , path ms_manifest;
    "ms_txt"        , loc longident ms_txt;
    "ms_attributes" , attributes ms_attributes;
    "ms_loc"        , location ms_loc;
  ]

and module_type_declaration
    { mtd_id; mtd_name; mtd_type; mtd_attributes; mtd_loc }
  =
  record [
    "mtd_id"         , ident mtd_id;
    "mtd_name"       , loc string mtd_name;
    "mtd_type"       , option module_type mtd_type;
    "mtd_attributes" , attributes mtd_attributes;
    "mtd_loc"        , location mtd_loc;
  ]

and open_infos : type a. (a -> Cmon.t) -> a open_infos -> Cmon.t =
  fun a { open_expr; open_bound_items; open_override; open_env;
          open_loc; open_attributes } ->
    record [
      "open_expr"        , a open_expr;
      "open_bound_items" , Types.signature open_bound_items;
      "open_override"    , Asttypes.override_flag open_override;
      "open_env"         , Env.env open_env;
      "open_loc"         , location open_loc;
      "open_attributes"  , attributes open_attributes;
    ]

and open_description x = open_infos (pair path (loc longident)) x

and open_declaration x = open_infos module_expr x

and include_infos : type a. (a -> Cmon.t) -> a include_infos -> Cmon.t =
  fun a { incl_mod; incl_type; incl_loc; incl_attributes } ->
  record [
    "incl_mod"        , a incl_mod;
    "incl_type"       , Types.signature incl_type;
    "incl_loc"        , location incl_loc;
    "incl_attributes" , attributes incl_attributes;
  ]

and include_description x = include_infos module_type x

and include_declaration x = include_infos module_expr x

and with_constraint = function
  | Twith_type td ->
    construct "Twith_type" [type_declaration td]
  | Twith_module (p, lil) ->
    construct "Twith_module" [path p; loc longident lil]
  | Twith_modtype mt ->
    construct "Twith_modtype" [module_type mt]
  | Twith_typesubst td ->
    construct "Twith_typesubst" [type_declaration td]
  | Twith_modsubst (p, lil) ->
    construct "Twith_modsubst" [path p; loc longident lil]
  | Twith_modtypesubst mt ->
    construct "Twith_modtypesubst" [module_type mt]

and core_type
    { ctyp_desc; ctyp_type; ctyp_env; ctyp_loc; ctyp_attributes }
  =
  record [
    "ctyp_desc"       , core_type_desc ctyp_desc;
    "ctyp_type"       , Types.type_expr ctyp_type;
    "ctyp_env"        , Env.env ctyp_env;
    "ctyp_loc"        , location ctyp_loc;
    "ctyp_attributes" , attributes ctyp_attributes;
  ]

and core_type_desc = function
  | Ttyp_any ->
    constant "Ttyp_any"
  | Ttyp_var txt ->
    construct "Ttyp_var" [string txt]
  | Ttyp_arrow (lbl, ct1, ct2) ->
    construct "Ttyp_arrow"
      [Asttypes.arg_label lbl; core_type ct1; core_type ct2]
  | Ttyp_tuple cts ->
    construct "Ttyp_tuple" [list_map core_type cts]
  | Ttyp_constr (p, lil, cts) ->
    construct "Ttyp_constr" [path p; loc longident lil; list_map core_type cts]
  | Ttyp_object (ofs, cf) ->
    construct "Ttyp_object" [list_map object_field ofs;
                             Asttypes.closed_flag cf]
  | Ttyp_class (p, lil, cts) ->
    construct "Ttyp_class" [path p; loc longident lil; list_map core_type cts]
  | Ttyp_alias (ct, s) ->
    construct "Ttyp_alias" [core_type ct; string s]
  | Ttyp_variant (rfl, cf, llo) ->
    construct "Ttyp_variant" [list_map row_field rfl; Asttypes.closed_flag cf;
                              option (list_map Asttypes.label) llo]
  | Ttyp_poly (ss, ct) ->
    construct "Ttyp_poly" [list_map string ss; core_type ct]
  | Ttyp_package pt ->
    construct "Ttyp_package" [package_type pt]

and package_type { pack_path; pack_fields ; pack_type; pack_txt } =
  record [
    "pack_path"   , path pack_path;
    "pack_fields" , list_map (pair (loc longident) core_type) pack_fields;
    "pack_type"   , Types.module_type pack_type;
    "pack_txt"    , loc longident pack_txt;
  ]

and row_field { rf_desc; rf_loc; rf_attributes } =
  record [
    "rf_desc"       , row_field_desc rf_desc;
    "rf_loc"        , location rf_loc;
    "rf_attributes" , attributes rf_attributes;
  ]

and row_field_desc = function
  | Ttag (sl, b, cts) ->
    construct "Ttag" [loc string sl; bool b; list_map core_type cts]
  | Tinherit ct ->
    construct "Tinherit" [core_type ct]

and object_field { of_desc; of_loc; of_attributes } =
  record [
    "of_desc"       , object_field_desc of_desc;
    "of_loc"        , location of_loc;
    "of_attributes" , attributes of_attributes;
  ]

and object_field_desc = function
  | OTtag (sl, ct) ->
    construct "OTtag" [loc string sl; core_type ct]
  | OTinherit ct ->
    construct "OTinherit" [core_type ct]

and value_description
    { val_id; val_name; val_desc; val_val; val_prim; val_loc; val_attributes }
  =
  record [
    "val_id"         , ident val_id;
    "val_name"       , loc string val_name;
    "val_desc"       , core_type val_desc;
    "val_val"        , Types.value_description val_val;
    "val_prim"       , list_map string  val_prim;
    "val_loc"        , location val_loc;
    "val_attributes" , attributes val_attributes;
  ]

and type_param x =
  pair core_type (pair Asttypes.variance Asttypes.injectivity) x

and type_declaration
  { typ_id; typ_name; typ_params; typ_type; typ_cstrs; typ_kind; typ_private;
    typ_manifest; typ_loc; typ_attributes }
  =
  record [
    "typ_id"         , ident typ_id;
    "typ_name"       , loc string typ_name;
    "typ_params"     , list_map type_param typ_params;
    "typ_type"       , Types.type_declaration typ_type;
    "typ_cstrs"      , list_map (tup3 core_type core_type location) typ_cstrs;
    "typ_kind"       , type_kind typ_kind;
    "typ_private"    , Asttypes.private_flag typ_private;
    "typ_manifest"   , option core_type typ_manifest;
    "typ_loc"        , location typ_loc;
    "typ_attributes" , attributes typ_attributes;
   ]

and type_kind = function
  | Ttype_abstract ->
    constant "Ttype_abstract"
  | Ttype_variant cds ->
    construct "Ttype_variant" [list_map constructor_declaration cds]
  | Ttype_record ldls ->
    construct "Ttype_record" [list_map label_declaration ldls]
  | Ttype_open ->
    constant "Ttype_open"

and label_declaration
  { ld_id; ld_name; ld_mutable; ld_type; ld_loc; ld_attributes }
  =
  record [
    "ld_id"         , ident ld_id;
    "ld_name"       , loc string ld_name;
    "ld_mutable"    , Asttypes.mutable_flag ld_mutable;
    "ld_type"       , core_type ld_type;
    "ld_loc"        , location ld_loc;
    "ld_attributes" , attributes ld_attributes;
  ]

and constructor_declaration
    { cd_id; cd_name; cd_args; cd_res; cd_loc; cd_attributes }
  =
  record [
    "cd_id", ident cd_id;
    "cd_name", loc string cd_name;
    "cd_args", constructor_arguments cd_args;
    "cd_res", option core_type cd_res;
    "cd_loc", location cd_loc;
    "cd_attributes", attributes cd_attributes;
  ]

and constructor_arguments = function
  | Cstr_tuple cts ->
    construct "Cstr_tuple" [list_map core_type cts]
  | Cstr_record lds ->
    construct "Cstr_record" [list_map label_declaration lds]

and type_extension
    { tyext_path; tyext_txt; tyext_params; tyext_constructors; tyext_private;
      tyext_loc; tyext_attributes }
  =
  record [
    "tyext_path", path tyext_path;
    "tyext_txt", loc longident tyext_txt;
    "tyext_params", list_map type_param tyext_params;
    "tyext_constructors", list_map extension_constructor tyext_constructors;
    "tyext_private", Asttypes.private_flag tyext_private;
    "tyext_loc", location tyext_loc;
    "tyext_attributes", attributes tyext_attributes;
  ]

and type_exception
    { tyexn_constructor; tyexn_loc; tyexn_attributes }
  =
  record [
    "tyexn_constructor", extension_constructor tyexn_constructor;
    "tyexn_loc", location tyexn_loc;
    "tyexn_attributes", attributes tyexn_attributes;
  ]

and extension_constructor
    { ext_id; ext_name; ext_type; ext_kind; ext_loc; ext_attributes }
  =
  record [
    "ext_id", ident ext_id;
    "ext_name", loc string ext_name;
    "ext_type" , Types.extension_constructor ext_type;
    "ext_kind" , extension_constructor_kind ext_kind;
    "ext_loc" , location ext_loc;
    "ext_attributes", attributes ext_attributes;
  ]

and extension_constructor_kind = function
  | Text_decl (ca, cto) ->
    construct "Text_decl" [constructor_arguments ca; option core_type cto]
  | Text_rebind (p, lil) ->
    construct "Text_rebind" [path p; loc longident lil]

and class_type
    { cltyp_desc; cltyp_type; cltyp_env; cltyp_loc; cltyp_attributes }
  =
    record [
     "cltyp_desc"       , class_type_desc cltyp_desc;
     "cltyp_type"       , Types.class_type cltyp_type;
     "cltyp_env"        , Env.env cltyp_env;
     "cltyp_loc"        , location cltyp_loc;
     "cltyp_attributes" , attributes cltyp_attributes;
    ]

and class_type_desc = function
  | Tcty_constr (p, lil, cts) ->
    construct "Tcty_constr" [path p; loc longident lil; list_map core_type cts]
  | Tcty_signature cs ->
    construct "Tcty_signature" [class_signature cs]
  | Tcty_arrow (lbl, ct, clt) ->
    construct "Tcty_arrow"
      [Asttypes.arg_label lbl; core_type ct; class_type clt]
  | Tcty_open (od, clt) ->
    construct "Tcty_open" [open_description od; class_type clt]

and class_signature { csig_self; csig_fields; csig_type } =
  record [
    "csig_self"   , core_type csig_self;
    "csig_fields" , list_map class_type_field  csig_fields;
    "csig_type"   , Types.class_signature csig_type;
  ]

and class_type_field { ctf_desc; ctf_loc; ctf_attributes } =
  record [
    "ctf_desc"       , class_type_field_desc ctf_desc;
    "ctf_loc"        , location ctf_loc;
    "ctf_attributes" , attributes ctf_attributes;
  ]

and class_type_field_desc = function
  | Tctf_inherit ct ->
        construct "Tctf_inherit" [class_type ct]
  | Tctf_val (name, mf, vf, ct) ->
    construct "Tctf_val" [string name; Asttypes.mutable_flag mf;
                          Asttypes.virtual_flag vf; core_type ct]
  | Tctf_method (name, pf, vf, ct) ->
    construct "Tctf_method" [string name; Asttypes.private_flag pf;
                             Asttypes.virtual_flag vf; core_type ct]
  | Tctf_constraint (ct1, ct2) ->
    construct "Tctf_constraint" [core_type ct1; core_type ct2]
  | Tctf_attribute attr ->
    construct "Tctf_attribute" [attribute attr]

and class_declaration x = class_infos class_expr x

and class_description x = class_infos class_type x

and class_type_declaration x = class_infos class_type x

and class_infos : type a. (a -> Cmon.t) -> a class_infos -> Cmon.t =
  fun a { ci_virt; ci_params; ci_id_name; ci_id_class; ci_id_class_type ;
          ci_id_object; ci_id_typehash; ci_expr; ci_decl;
          ci_type_decl; ci_loc; ci_attributes } ->
    record [
      "ci_virt"          , Asttypes.virtual_flag ci_virt;
      "ci_params"        , list_map type_param ci_params;
      "ci_id_name"       , loc string ci_id_name;
      "ci_id_class"      , ident ci_id_class;
      "ci_id_class_type" , ident ci_id_class_type;
      "ci_id_object"     , ident ci_id_object;
      "ci_id_typehash"   , ident ci_id_typehash;
      "ci_expr"          , a ci_expr;
      "ci_decl"          , Types.class_declaration ci_decl;
      "ci_type_decl"     , Types.class_type_declaration ci_type_decl;
      "ci_loc"           , location ci_loc;
      "ci_attributes"    , attributes ci_attributes;
    ]

let implementation { structure=str; coercion; signature=sg } =
  record [
    "structure" , structure str;
    "coercion"  , module_coercion coercion;
    "signature" , Types.signature sg;
  ]
