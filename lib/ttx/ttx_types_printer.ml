open Ttx_types
open Ttx_def_printer

class types_printer ?(table = Type_expr.Table.create 7) () = object (self)
  method type_expr te =
    match Type_expr.Table.find_opt table te with
    | Some cmon -> cmon
    | None ->
      let cmon = Cmon.of_lazy (lazy (Cmon.record [
          "te_id", Cmon.int te.te_id;
          "te_desc", (match te.te_desc with
              | None -> Cmon.constant "None";
              | Some desc ->
                Cmon.constructor "Some" (match desc with
                    | Te_var var ->
                      Cmon.constructor "Te_var"
                        (type_level_variable var)
                    | Te_arrow {lhs; rhs} ->
                      Cmon.crecord "Te_arrow" [
                        "lhs", self#type_expr lhs;
                        "rhs", self#type_expr rhs;
                      ]
                    | Te_tuple ts ->
                      Cmon.constructor "Te_tuple"
                        (Cmon.list_map self#type_expr ts)
                    | Te_const (ts, p) ->
                      Cmon.construct "Te_const"
                        [Cmon.list_map self#type_expr ts; path p]
                  )
            )
        ])) in
      Type_expr.Table.add table te cmon;
      cmon

  method value_desc {vd_loc; vd_attrs; vd_binder; vd_type; vd_kind} =
    Cmon.record [
      "vd_loc"    , location vd_loc;
      "vd_attrs"  , attributes vd_attrs;
      "vd_binder" , binder vd_binder;
      "vd_type"   , binding type_level self#type_expr vd_type;
      "vd_kind"   , self#vd_kind vd_kind;
    ]

  method vd_kind = function
    | Vd_regular -> Cmon.constant "Vd_regular"
    | Vd_primitive -> Cmon.constant "Vd_primitive"

  method variant_constructor {vc_loc; vc_attrs; vc_path; vc_def} =
    Cmon.record [
      "vc_loc"   , location vc_loc;
      "vc_attrs" , attributes vc_attrs;
      "vc_path"  , self#vc_path vc_path;
      "vc_def"   , binding type_level self#vc_arguments_type_expr vc_def;
    ]

  method vc_arguments_type_expr (a, te) =
    Cmon.tuple [self#vc_arguments a; self#type_expr te]

  method vc_path {vc_type; vc_index; vc_name} =
    Cmon.record [
      "vc_name"  , Cmon.string vc_name;
      "vc_type"  , path vc_type;
      "vc_index" , Cmon.int vc_index;
    ]

  method vc_arguments = function
    | Vc_tuple tev -> Cmon.constructor "Vc_tuple" (vector self#type_expr tev)
    | Vc_record rlv -> Cmon.constructor "Vc_record" (vector self#record_label rlv)

  method record_label {rl_loc; rl_attrs; rl_path; rl_mutable; rl_def} =
    Cmon.record [
      "rl_loc"     , location rl_loc;
      "rl_attrs"   , attributes rl_attrs;
      "rl_path"    , self#rl_path rl_path;
      "rl_mutable" , mutable_flag rl_mutable;
      "rl_def"     , binding type_level self#rl_def rl_def;
    ]

  method rl_def {rl_label_type; rl_record_type} =
    Cmon.record [
      "rl_label_type"  , self#type_expr rl_label_type;
      "rl_record_type" , self#type_expr rl_record_type;
    ]

  method rl_path {rl_kind; rl_index; rl_name} =
    Cmon.record [
      "rl_kind"  , self#rl_kind rl_kind;
      "rl_index" , Cmon.int rl_index;
      "rl_name"  , Cmon.string rl_name;
    ]

  method rl_kind = function
    | Rl_normal p   -> Cmon.constructor "Rl_normal" (path p)
    | Rl_inline vcp -> Cmon.constructor "Rl_inline" (self#vc_path vcp)

  (*type*)
  method type_decl {td_loc; td_attrs; td_binder; td_body} =
    Cmon.record [
      "td_loc"    , location td_loc;
      "td_attrs"  , attributes td_attrs;
      "td_binder" , binder td_binder;
      "td_body"   , binding type_level self#td_body td_body;
    ]

  method td_body {td_params; td_manifest; td_desc} =
    Cmon.record [
      "td_params"   , Cmon.list_map self#type_expr td_params;
      "td_manifest" , option self#type_expr td_manifest;
      "td_desc"     , self#td_desc td_desc;
    ]

  method td_desc = function
    | Td_abstract -> Cmon.constant "Td_abstract"
    | Td_open -> Cmon.constant "Td_open"
    | Td_record x ->
      Cmon.constructor "Td_record"  (vector self#record_label x)
    | Td_variant x ->
      Cmon.constructor "Td_variant" (vector self#variant_constructor x)

  method functor_parameter = function
    | Fp_unit -> Cmon.constant "Fp_unit"
    | Fp_named md ->
      Cmon.constructor "Fp_named" (self#module_decl md)
    | Fp_anonymous mt ->
      Cmon.constructor "Fp_anonymous" (self#module_type mt)

  method module_type = function
    | Mt_ident p ->
      Cmon.constructor "Mt_ident" (path p)
    | Mt_signature s ->
      Cmon.constructor "Mt_signature" (self#signature s)
    | Mt_functor b ->
      Cmon.constructor "Mt_functor" (binding self#functor_parameter self#module_type b)
    | Mt_alias p ->
      Cmon.constructor "Mt_alias" (path p)

  method module_decl {md_loc; md_attrs; md_binder; md_type} =
    Cmon.record [
      "md_loc"    , location md_loc;
      "md_attrs"  , attributes md_attrs;
      "md_binder" , binder md_binder;
      "md_type"   , self#module_type md_type;
    ]

  method module_type_decl {mtd_loc; mtd_attrs; mtd_binder; mtd_def} =
    Cmon.record [
      "mtd_loc"    , location mtd_loc;
      "mtd_attrs"  , attributes mtd_attrs;
      "mtd_binder" , binder mtd_binder;
      "mtd_def"    , self#mtd_def mtd_def;
    ]

  method mtd_def = function
    | Mtd_abstract -> Cmon.constant "Mtd_abstract"
    | Mtd_concrete mt -> Cmon.constructor "Mtd_concrete" (self#module_type mt)

  method signature_item {si_visibility; si_desc} =
    Cmon.record [
      "si_visibility" , self#si_visibility si_visibility;
      "si_desc"       , self#si_desc si_desc;
    ]

  method si_visibility = function
    | Si_exported -> Cmon.constant "Si_exported"
    | Si_hidden   -> Cmon.constant "Si_hidden"

  method si_desc = function
    | Si_value vd ->
      Cmon.constructor "Si_value" (self#value_desc vd)
    | Si_type (rf, tdl) ->
      Cmon.construct "Si_type" [rec_flag rf; Cmon.list_map self#type_decl tdl]
    | Si_module (rf, mdl) ->
      Cmon.construct "Si_module" [rec_flag rf; Cmon.list_map self#module_decl mdl]
    | Si_module_type mtd ->
      Cmon.constructor "Si_module_type" (self#module_type_decl mtd)

  method signature = function
    | S_item b -> Cmon.constructor "S_item" (binding self#signature_item self#signature b)
    | S_done -> Cmon.constant "S_done"
end
