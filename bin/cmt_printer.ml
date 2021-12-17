open Cmon
open Ttx_printer
open Print_utils

let pers_flags = function
  | Cmi_format.Rectypes ->
    constant "Rectypes"
  | Cmi_format.Alerts alerts ->
    construct "Alerts" [list_map (pair string string)
                                 (Misc.Stdlib.String.Map.bindings alerts)]
  | Cmi_format.Opaque -> constant "Opaque"
  | Cmi_format.Unsafe_string-> constant "Unsafe_string"

let digest d = string (Digest.to_hex d)

let cmi_infos {
    Cmi_format.
    cmi_name : Misc.modname;
    cmi_sign : Types.signature_item list;
    cmi_crcs : Misc.crcs;
    cmi_flags : Cmi_format.pers_flags list;
  } =
  record [
    "cmi_name"  , string cmi_name;
    "cmi_sign"  , Types_printer.Types.signature cmi_sign;
    "cmi_crcs"  , list_map (pair string (option digest)) cmi_crcs;
    "cmi_flags" , list_map pers_flags cmi_flags;
  ]

module Cmt = struct
  open Cmt_format
  open Types_printer
  open Typedtree_printer

  let binary_part = function
    | Partial_structure str ->
      construct "Partial_structure" [structure str]
    | Partial_structure_item str_item ->
      construct "Partial_structure_item" [structure_item str_item]
    | Partial_expression exp ->
      construct "Partial_expression" [expression exp]
    | Partial_pattern (cat, pat) ->
      construct "Partial_pattern" [pattern_category cat; general_pattern pat]
    | Partial_class_expr ce ->
      construct "Partial_class_expr" [class_expr ce]
    | Partial_signature sg ->
      construct "Partial_signature" [signature sg]
    | Partial_signature_item sg_item ->
      construct "Partial_signature_item" [signature_item sg_item]
    | Partial_module_type mt ->
      construct "Partial_module_type" [module_type mt]

  let binary_annots = function
    | Packed (sg, ss) ->
      construct "Packed" [Types.signature sg; list_map string ss]
    | Implementation str ->
      construct "Implementation" [Typedtree_printer.structure str]
    | Interface sg ->
      construct "Interface" [Typedtree_printer.signature sg]
    | Partial_implementation bps ->
      construct "Partial_implementation" [array_map binary_part bps]
    | Partial_interface bps ->
      construct "Partial_interface" [array_map binary_part bps]

  let cmt_infos
      { Cmt_format. cmt_modname; cmt_annots; cmt_value_dependencies;
        cmt_comments; cmt_args; cmt_sourcefile; cmt_builddir;
        cmt_loadpath; cmt_source_digest; cmt_initial_env; cmt_imports;
        cmt_interface_digest; cmt_use_summaries }
    =
    record [
      "cmt_modname"            , string cmt_modname;
      "cmt_annots"             , binary_annots cmt_annots;
      "cmt_value_dependencies" ,
      list_map (pair Types.value_description Types.value_description)
        cmt_value_dependencies;
      "cmt_comments"           , list_map (pair string location) cmt_comments;
      "cmt_args"               , array_map string cmt_args;
      "cmt_sourcefile"         , option string cmt_sourcefile;
      "cmt_builddir"           , string cmt_builddir;
      "cmt_loadpath"           , list_map string cmt_loadpath;
      "cmt_source_digest"      , option string cmt_source_digest;
      "cmt_initial_env"        , Env.env cmt_initial_env;
      "cmt_imports"            , list_map (pair string (option string)) cmt_imports;
      "cmt_interface_digest"   , option digest cmt_interface_digest;
      "cmt_use_summaries"      , bool cmt_use_summaries;
    ]
end

let infos = Cmt_format.read Sys.argv.(1)

let () =
  PPrint.ToChannel.pretty 0.9 80 stdout
    (Cmon.print (pair (option cmi_infos) (option Cmt.cmt_infos) infos))
