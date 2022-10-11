let (%) p t = Dsl.A ([p], t)

let ttx_types : Dsl.decl list = [
  "open Ttx_def",
  Custom [`Intf_header; `Impl_header];
  "type_expr", Decl [
    "desc", Variant [
      "Var"   , Tuple [T"Type_level.variable"];
      "Arrow" , Record ["lhs", T"type_expr"; "rhs", T"type_expr"];
      "Tuple" , Tuple [T"type_expr"%"list"];
      "Const" , Tuple [T"type_expr"%"list"; T"ns_type path"];
    ];
    "val compare : t -> t -> int\n\
     val equal : t -> t -> bool\n\
     val hash : t -> int\n\
     \n\
     val make : desc -> t\n\
     val desc : t -> desc\n\
     \n\
     val make_undefined : unit -> t\n\
     val define : t -> desc -> unit\n\
     exception Already_defined\n\
     exception Undefined",
    Custom [`Intf_make];

    "let gen_id =\n\
    \  let r = ref 0 in\n\
    \  fun () -> incr r; !r\n",
    Custom [`Impl_header];

    "type t = {\n\
    \  id: int;\n\
    \  mutable desc: desc option;\n\
     }\n",
    Custom [`Impl_type];

    "let compare t1 t2 =\n\
    \  if t1 == t2\n\
    \  then 0\n\
    \  else\n\
    \    let c = Int.compare t1.id t2.id in\n\
    \    assert (c <> 0);\n\
    \    c\n\
     \n\
     let equal t1 t2 =\n\
    \  t1 == t2\n\
     \n\
     let hash t = t.id\n\
     \n\
     let make desc = {id = gen_id (); desc = Some desc}\n\
     \n\
     exception Undefined\n\
     \n\
     let desc t =\n\
    \  match t.desc with\n\
    \  | None -> raise Undefined\n\
    \  | Some desc -> desc\n\
     \n\
     let make_undefined () = {id = gen_id (); desc = None}\n\
     \n\
     exception Already_defined\n\
     \n\
     let define t new_desc =\n\
    \  match t.desc with\n\
    \  | None -> t.desc <- Some new_desc\n\
    \  | Some _ -> raise Already_defined\n",
    Custom [`Impl_make];
  ];

  "type_scheme", Decl [
    "forall" , Value (T"type_level");
    "expr"   , Value (T"type_expr");
  ];

  "value_desc", Decl [
    "desc", Variant [
      "Regular"  , Tuple [];
      "Primitive", Tuple [];
    ];
    "loc"    , Value (T"location");
    "attrs"  , Value (T"attributes");
    "binder" , Value (T"ns_value binder");
    "typ"    , Value (T"type_scheme");
    "desc"   , Value (T"desc");
  ];

  "constructor", Decl [
    "path", Record [
      "typ"   , T"ns_type Path.t";
      "index" , T"int";
      "name"  , T"string";
    ];
    "arguments", Variant [
      "Tuple"  , Tuple [T"type_expr"%"vector"];
      "Record" , Tuple [T"label"%"vector"];
    ];

    "loc"       , Value (T"location");
    "attrs"     , Value (T"attributes");
    "path"      , Value (T"path");
    "forall"    , Value (T"type_level");
    "arguments" , Value (T"arguments");
    "result"    , Value (T"type_expr");
  ];

  "label", Decl [
    "kind", Variant [
      "Record"        , Tuple [T"ns_type Path.t"];
      "Inline_record" , Tuple [T"Constructor.path"];
    ];
    "path", Record [
      "kind"  , T"kind";
      "index" , T"int";
      "name"  , T"string";
    ];

    "loc"        , Value (T"location");
    "attrs"      , Value (T"attributes");
    "path"       , Value (T"path");
    "forall"     , Value (T"type_level");
    "mutability" , Value (T"mutable_flag");
    "record"     , Value (T"type_expr");
    "field"      , Value (T"type_expr");
  ];

  "type_decl", Decl [
    "desc", Variant [
      "Abstract" , Tuple [];
      "Record"   , Tuple [T"label"%"list"];
      "Variant"  , Tuple [T"constructor"%"list"];
      "Open"     , Tuple [];
    ];
    "loc"       , Value (T"location");
    "attrs"     , Value (T"attributes");
    "binder"    , Value (T"ns_type binder");
    "forall"    , Value (T"type_level");
    "params"    , Value (T"type_expr"%"list");
    "manifest"  , Value (T"type_expr"%"option");
    "desc"      , Value (T"desc");
  ];

  "functor_parameter", Decl [
    "desc", Variant [
      "Unit"      , Tuple [];
      "Named"     , Tuple [T"module_decl"];
      "Anonymous" , Tuple [T"module_type"];
    ];
    "desc", Value (T"desc");
  ];

  "module_type", Decl [
    "desc", Variant [
      "Ident"     , Tuple [T"ns_module_type path"];
      "Signature" , Tuple [T"signature"];
      "Functor"   , Tuple [T"functor_parameter"; T"module_type"];
      "Alias"     , Tuple [T"ns_module path"];
    ];
    "desc", Value (T"desc");
  ];

  "module_decl", Decl [
    "loc"    , Value (T"location");
    "attrs"  , Value (T"attributes");
    "binder" , Value (T"ns_module binder");
    "typ"    , Value (T"module_type");
  ];

  "module_type_decl", Decl [
    "loc"    , Value (T"location");
    "attrs"  , Value (T"attributes");
    "binder" , Value (T"ns_module_type binder");
    "typ"    , Value (T"module_type"%"option");
  ];

  "signature_item", Decl [
    "visibility", Variant [
      "Exported" , Tuple [];
      "Hidden"   , Tuple [];
    ];
    "desc", Variant [
      "Value"       , Tuple [T"value_desc"];
      "Type"        , Tuple [T"rec_flag"; T"type_decl"%"list"];
      "Module"      , Tuple [T"rec_flag"; T"module_decl"%"list"];
      "Module_type" , Tuple [T"module_type_decl"];
    ];
    "visibility", Value (T"visibility");
    "desc", Value (T"desc");
  ];

  "signature", Decl [
    "items", Value (T"signature_item"%"list");
  ];
]

let usage () =
  Printf.eprintf "Usage: %s <types.ml|types.mli>\n" Sys.argv.(0);
  exit 1

let target =
  if Array.length Sys.argv = 2
  then Sys.argv.(1)
  else usage ()

let () =
  match target with
  | "types.ml"  -> Dsl.gen_impl stdout ttx_types
  | "types.mli" -> Dsl.gen_intf stdout ttx_types
  | _ -> usage ()
