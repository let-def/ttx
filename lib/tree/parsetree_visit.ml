type 'a category =
  | Constant                 : constant node category
  | Attribute                : attribute node category
  | Extension                : extension node category
  | Payload                  : payload node category
  | Type_expr                : type_expr node category
  | Expression               : expression node category
  | Pattern                  : pattern node category
  | Object_field             : object_field node category
  | Row_field                : row_field node category
  | Package                  : package node category
  | Structure_item           : structure_item node category
  | Structure                : structure node category
  | Signature_item           : signature_item node category
  | Signature                : signature node category
  | Value_binding            : value_binding node category
  | Module_binding           : module_binding node category
  | Case                     : case node category
  | Module_expr              : module_expr node category
  | Open_declaration         : open_declaration node category
  | Open_description         : open_description node category
  | Include_declaration      : include_declaration node category
  | Include_description      : include_description node category
  | Let_op                   : let_op node category
  | Binding_op               : binding_op node category
  | Value_description        : value_description node category
  | Type_declaration         : type_declaration node category
  | Constructor_declaration  : constructor_declaration node category
  | Label_declaration        : label_declaration node category
  | Type_extension           : type_extension node category
  | Type_exception           : type_exception node category
  | Extension_constructor    : extension_constructor node category
  | Extension_constructor_kind : extension_constructor_kind node category
  | Class_type               : class_type node category
  | Class_type_field         : class_type_field node category
  | Class_description        : class_description node category
  | Class_declaration        : class_declaration node category
  | Class_type_declaration   : class_type_declaration node category
  | Class_signature          : class_signature node category
  | Class_expr               : class_expr node category
  | Class_field              : class_field node category
  | Class_structure          : class_structure node category
  | Module_type              : module_type node category
  | Module_declaration       : module_declaration node category
  | Module_substitution      : module_substitution node category
  | Module_type_declaration  : module_type_declaration node category
  | Directive                : directive node category
  | Directive_argument       : directive_argument node category
  | Phrase                   : phrase node category

module Visitor : sig
  type iter = { visit: 'a. iter -> 'a category -> 'a -> unit }
  val iter : iter

  type map = { visit: 'a. map -> 'a category -> 'a -> 'a }
  val map : map
end

