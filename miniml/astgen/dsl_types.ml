let (%) p t = Dsl.A ([p], t)

let ttx_types : Dsl.decl list = [
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
     val make_undefined : unit -> t\n\
     val define : t -> desc -> unit\n\
     exception Already_defined\n\
     exception Undefined",
    Custom [`Intf];
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
  ];

  "module_type", Decl [
    "desc", Variant [
      "Ident"     , Tuple [T"ns_module_type path"];
      "Signature" , Tuple [T"signature"];
      "Functor"   , Tuple [T"functor_parameter"; T"module_type"];
      "Alias"     , Tuple [T"ns_module path"];
    ]
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
      "Hiddent"  , Tuple [];
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

let () = Dsl.gen_intf ttx_types
