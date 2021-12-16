open Cmon
open Typedtree_printer

let pers_flags = function
  | Cmi_format.Rectypes -> constant "Rectypes"
  | Cmi_format.Alerts alerts ->
    construct "Alerts" [
      list_map (Types_printer.pair string string)
        (Misc.Stdlib.String.Map.bindings alerts)
    ]
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
    "cmi_crcs"  , list_map (Types_printer.pair string (Types_printer.option digest)) cmi_crcs;
    "cmi_flags" , list_map pers_flags cmi_flags;
  ]

let infos = Cmi_format.read_cmi Sys.argv.(1)

let () =
  PPrint.ToChannel.pretty 0.9 80 stdout (Cmon.print (cmi_infos infos))
