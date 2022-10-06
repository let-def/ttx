open Asttypes

type location = Location.t
type longident = Longident.t loc

type 'a node

type constant
type attribute
type extension
type payload
type type_expr
type expression
type pattern
type object_field
type row_field
type package
type structure_item
type structure = structure_item node list
type signature_item
type signature = signature_item node list
type value_binding
type module_binding
type case
type module_expr
type open_declaration
type open_description
type include_declaration
type include_description
type let_op
type binding_op
type value_description
type type_declaration
type constructor_declaration
type label_declaration
type type_extension
type type_exception
type extension_constructor
type extension_constructor_kind
type module_type
type module_declaration
type module_substitution
type module_type_declaration
type directive
type directive_argument
type phrase

val loc : _ node -> location
val loc_stack : _ node -> location list
val attributes : _ node -> attribute node list

module Constant : sig
  type desc =
    | Integer of {
        litteral: string;
        suffix: char option;
      }
    | Char of {
        litteral: char;
      }
    | String of {
        litteral: string;
        location: location;
        delim: string option;
      }
    | Float of {
        litteral: string;
        suffix: char option;
      }
  val desc : constant node -> desc
end

module Attribute : sig
  val name : attribute node -> string loc
  val payload : attribute node -> payload node
end

module Extension : sig
  val name : extension node -> string loc
  val payload : extension node -> payload node
end

module Payload : sig
  type desc =
    | Str of structure node
    | Sig of signature node (* : SIG *)
    | Typ of type_expr node (* : T *)
    | Pat of { pattern: pattern node; guard: expression node option }
  val desc : payload node -> desc
end

(** {1 Core language} *)

(* Type expressions *)

module Type_expr : sig
  type desc =
    | Any
    | Var of string
    | Arrow of arg_label * type_expr node * type_expr node
    | Tuple of type_expr node list
    | Constr of longident * type_expr node list
    | Object of object_field node list * closed_flag
    | Class of longident * type_expr node list
    | Alias of type_expr node * string
    | Variant of row_field node list * closed_flag * label list option
    | Poly of string loc list * type_expr node
    | Package of package node
    | Extension of extension node
  val desc : type_expr node -> desc
end

module Package : sig
  val path : package node -> longident
  val constraints : package node -> (longident * type_expr node) list
end
      (*
        (module S)
        (module S with type t1 = T1 and ... and tn = Tn)
       *)

module Row_field : sig
  type desc =
    | Tag of label loc * bool * type_expr node list
    (* [`A]                   ( true,  [] )
           [`A of T]              ( false, [T] )
           [`A of T1 & .. & Tn]   ( false, [T1;...Tn] )
           [`A of & T1 & .. & Tn] ( true,  [T1;...Tn] )

       - The 'bool' field is true if the tag contains a
            constant (empty) constructor.
       - '&' occurs when several types are used for the same constructor
            (see 4.2 in the manual)
    *)
    | Inherit of type_expr node
    (* [ | node ] *)
  val desc : row_field node -> desc
end

module Object_field : sig
  type desc =
    | Tag of label loc * type_expr node
    | Inherit of type_expr node
  val desc : object_field node -> desc
end

(* Patterns *)

module Pattern : sig
  type desc =
    | Any
    (* _ *)
    | Var of string loc
    (* x *)
    | Alias of pattern node * string loc
    (* P as 'a *)
    | Constant of constant node
    (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Interval of constant node * constant node
    (* 'a'..'z'

       Other forms of interval are recognized by the parser
       but rejected by the type-checker. *)
    | Tuple of pattern node list
    (* (P1, ..., Pn)

       Invariant: n >= 2
    *)
    | Construct of
        longident * (string loc list * pattern node) option
    (* C                    None
       C P                  Some ([], P)
       C (P1, ..., Pn)      Some ([], tuple [P1; ...; Pn])
       C (type a b) P       Some ([a; b], P)
    *)
    | Variant of label * pattern node option
    (* `A             (None)
       `A P           (Some P)
    *)
    | Record of (longident * pattern node) list * closed_flag
    (* { l1=P1; ...; ln=Pn }     (flag = Closed)
       { l1=P1; ...; ln=Pn; _}   (flag = Open)

       Invariant: n > 0
    *)
    | Array of pattern node list
    (* [| P1; ...; Pn |] *)
    | Or of pattern node * pattern node
    (* P1 | P2 *)
    | Constraint of pattern node * type_expr node
    (* (P : T) *)
    | Type of longident
    (* #tconst *)
    | Lazy of pattern node
    (* lazy P *)
    | Unpack of string option loc
    (* (module P)        Some "P"
       (module _)        None

       Note: (module P : S) is represented as
       constraint(unpack, Ptyp_package)
    *)
    | Exception of pattern node
    (* exception P *)
    | Extension of extension node
    (* [%id] *)
    | Open of longident * pattern node
    (* M.(P) *)

  val desc : pattern node -> desc
end

(* Value expressions *)

module Expression : sig
  type desc =
    | Ident of longident
    (* x
           M.x
    *)
    | Constant of constant
    (* 1, 'a', "true", 1.0, 1l, 1L, 1n *)
    | Let of rec_flag * value_binding list * expression
    (* let P1 = E1 and ... and Pn = EN in E       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN in E   (flag = Recursive)
    *)
    | Function of case list
    (* function P1 -> E1 | ... | Pn -> En *)
    | Fun of arg_label * expression option * pattern * expression
    (* fun P -> E1                          (Simple, None)
       fun ~l:P -> E1                       (Labelled l, None)
       fun ?l:P -> E1                       (Optional l, None)
       fun ?l:(P = E0) -> E1                (Optional l, Some E0)

       Notes:
       - If E0 is provided, only Optional is allowed.
       - "fun P1 P2 .. Pn -> E1" is represented as nested fun.
       - "let f P = E" is represented using fun.
    *)
    | Apply of expression * (arg_label * expression) list
    (* E0 ~l1:E1 ... ~ln:En
       li can be empty (non labeled argument) or start with '?'
       (optional argument).

       Invariant: n > 0
    *)
    | Match of expression * case list
    (* match E0 with P1 -> E1 | ... | Pn -> En *)
    | Try of expression * case list
    (* try E0 with P1 -> E1 | ... | Pn -> En *)
    | Tuple of expression list
    (* (E1, ..., En)

       Invariant: n >= 2
    *)
    | Construct of longident * expression option
    (* C                None
       C E              Some E
       C (E1, ..., En)  Some (tuple[E1;...;En])
    *)
    | Variant of label * expression option
    (* `A             (None)
       `A E           (Some E)
    *)
    | Record of (longident * expression) list * expression option
    (* { l1=P1; ...; ln=Pn }     (None)
       { E0 with l1=P1; ...; ln=Pn }   (Some E0)

       Invariant: n > 0
    *)
    | Field of expression * longident
    (* E.l *)
    | Set_field of expression * longident * expression
    (* E1.l <- E2 *)
    | Array of expression list
    (* [| E1; ...; En |] *)
    | If_then_else of expression * expression * expression option
    (* if E1 then E2 else E3 *)
    | Sequence of expression * expression
    (* E1; E2 *)
    | While of expression * expression
    (* while E1 do E2 done *)
    | For of
        pattern *  expression * expression * direction_flag * expression
    (* for i = E1 to E2 do E3 done      (flag = Upto)
       for i = E1 downto E2 do E3 done  (flag = Downto)
    *)
    | Constraint of expression * type_expr node
    (* (E : T) *)
    | Coerce of expression * type_expr node option * type_expr node
    (* (E :> T)        (None, T)
       (E : T0 :> T)   (Some T0, T)
    *)
    | Send of expression * label loc
    (*  E # m *)
    | New of longident
    (* new M.c *)
    | Set_instvar of label loc * expression node
    (* x <- 2 *)
    | Override of (label loc * expression node) list
    (* {< x1 = E1; ...; Xn = En >} *)
    | Let_module of string option loc * module_expr node * expression node
    (* let module M = ME in E *)
    | Let_exception of extension_constructor node * expression node
    (* let exception C in E *)
    | Assert of expression node
    (* assert E
       Note: "assert false" is treated in a special way by the
       type-checker. *)
    | Lazy of expression node
    (* lazy E *)
    | Poly of expression node * type_expr node option
    (* Used for method bodies.

       Can only be used as the expression under Cfk_concrete
       for methods (not values). *)
    | Newtype of string loc * expression node
    (* fun (type node) -> E *)
    | Pack of module_expr node
    (* (module ME)

       (module ME : S) is represented as
       constraint(pack, Ptyp_package S) *)
    | Open of open_declaration * expression node
    (* M.(E)
       let open M in E
       let! open M in E *)
    | Let_op of let_op node
    (* let* P = E in E
       let* P = E and* P = E in E *)
    | Extension of extension node
    (* [%id] *)
    | Unreachable
    (* . *)
  val desc : expression node -> desc
end

module Case : sig
  val lhs   : case node -> pattern node
  val guard : case node -> expression node option
  val rhs   : case node -> expression node
end

module Let_op : sig
  val let_ : let_op node -> binding_op node
  val ands : let_op node -> binding_op node list
  val body : let_op node -> expression node
end

module Binding_op : sig
  val op : binding_op node -> string loc
  val pattern : binding_op node -> pattern node
  val expression : binding_op node -> expression node
end

(* Value descriptions *)

module Value_description : sig
  val name : value_description node -> string loc
  val typ  : value_description node -> type_expr node
  val prim : value_description node -> string list
end

(* Type declarations *)

module Type_declaration : sig
  type desc =
    | Abstract
    | Variant of constructor_declaration node list
    | Record of label_declaration node list (* Invariant: non-empty list *)
    | Open

  val name : type_declaration node -> string loc
  val params : type_declaration node -> (type_expr node * variance * injectivity) list
  val constraints : type_declaration node -> (type_expr node * type_expr node * location) list
  val desc : type_declaration node -> desc
  val privacy : type_declaration node -> private_flag
  val manifest : type_declaration node -> type_expr node option
end

module Label_declaration : sig
  val name : label_declaration node -> string loc
  val mutability : label_declaration node -> mutable_flag
  val typ : label_declaration node -> type_expr node
end

module Constructor_declaration : sig
  type arguments =
    | Tuple of type_expr node list
    | Record of label_declaration node list

  val name : constructor_declaration node -> string loc
  val arguments : constructor_declaration node -> arguments list
  val result : constructor_declaration node -> type_expr node option
end

module Type_extension : sig
  val path: type_extension node -> longident
  val params: type_extension node -> (type_expr node * (variance * injectivity)) list
  val constructors: type_extension node -> extension_constructor list
  val privacy: type_extension node -> private_flag
end

module Extension_constructor : sig
  type desc =
    | Declare of Constructor_declaration.arguments * type_expr node option
    | Rebind of longident

  val name : extension_constructor node -> string loc
  val desc : extension_constructor node -> desc
end

module Type_exception : sig
  val constructor : type_exception node -> extension_constructor node
end

(** {1 Module language} *)

(* Type expressions for the module language *)

module Module_type : sig
  type with_constraint =
    | With_type of longident * type_declaration
    (* with type X.t = ...
         Note: the last component of the longident must match
         the name of the type_declaration. *)
    | With_module of longident * longident
    (* with module X.Y = Z *)
    | With_modtype of longident * module_type
    (* with module type X.Y = Z *)
    | With_modtype_subst of longident * module_type
    (* with module type X.Y := sig end *)
    | With_type_subst of longident * type_declaration
    (* with type X.t := ..., same format as [Pwith_type] *)
    | With_module_subst of longident * longident
    (* with module X.Y := Z *)

  type functor_parameter =
    | Unit
    (* () *)
    | Named of string option loc * module_type node
    (* (X : MT)          Some X, MT
       (_ : MT)          None, MT *)

  type desc =
    | Ident of longident
    (* S *)
    | Signature of signature
    (* sig ... end *)
    | Functor of functor_parameter * module_type
    (* functor(X : MT1) -> MT2 *)
    | With of module_type * with_constraint list
    (* MT with ... *)
    | Typeof of module_expr
    (* module type of ME *)
    | Extension of extension
    (* [%id] *)
    | Alias of longident
    (* (module M) *)
  val desc : module_type node -> desc
end

module Signature_item : sig
  type desc =
    | Value of value_description node
        (*
          val x: T
          external x: T = "s1" ... "sn"
         *)
    | Type of rec_flag * type_declaration node list
    (* type t1 = ... and ... and tn  = ... *)
    | Typesubst of type_declaration node list
    (* type t1 := ... and ... and tn := ...  *)
    | Typext of type_extension node
    (* type t1 += ... *)
    | Exception of type_exception node
    (* exception C of T *)
    | Module of module_declaration node
    (* module X = M
       module X : MT *)
    | Modsubst of module_substitution node
    (* module X := M *)
    | Recmodule of module_declaration node list
    (* module rec X1 : MT1 and ... and Xn : MTn *)
    | Modtype of module_type_declaration node
    (* module type S = MT
       module type S *)
    | Modtypesubst of module_type_declaration node
    (* module type S :=  ...  *)
    | Open of open_description node
    (* open X *)
    | Include of include_description node
    (* include MT *)
    | Attribute of attribute node
    (* [@@@id] *)
    | Extension of extension node
    (* [%%id] *)
end

module Module_declaration : sig
  val name : module_declaration node -> string option loc
  val typ : module_declaration node -> module_type node
end

(* S : MT *)

module Module_substitution : sig
  val name : module_substitution node -> string loc
  val manifest : module_substitution node -> longident
end

module Module_type_declaration : sig
  val name : module_type_declaration node -> string loc
  val typ : module_type_declaration node -> module_type node option
end

(* S = MT
   S       (abstract module type declaration, pmtd_type = None)
*)


module Open_description : sig
  val override: open_description node -> override_flag
  val path : open_description node -> longident
end

module Open_declaration : sig
  val override: open_description node -> override_flag
  val body : open_description node -> module_expr node
end

module Include_description : sig
  val desc : include_description node -> module_type node
end

module Include_declaration : sig
  val desc : include_description node -> module_expr node
end

(* Value expressions for the module language *)

module Module_expr : sig
  type desc =
    | Ident of longident
    (* X *)
    | Structure of structure node
    (* struct ... end *)
    | Functor of Module_type.functor_parameter * module_expr node
    (* functor(X : MT1) -> ME *)
    | Apply of module_expr node * module_expr node
    (* ME1(ME2) *)
    | Constraint of module_expr node * module_type node
    (* (ME : MT) *)
    | Unpack of expression node
    (* (val E) *)
    | Extension of extension node
    (* [%id] *)

  val desc : module_expr node -> desc
end

module Structure_item : sig
  type desc =
    | Eval of expression node
    (* E *)
    | Value of rec_flag * value_binding node list
    (* let P1 = E1 and ... and Pn = EN       (flag = Nonrecursive)
       let rec P1 = E1 and ... and Pn = EN   (flag = Recursive)
    *)
    | Primitive of value_description node
    (*  val x: T
        external x: T = "s1" ... "sn" *)
    | Type of rec_flag * type_declaration node list
    (* type t1 = ... and ... and tn = ... *)
    | Typext of type_extension node
    (* type t1 += ... *)
    | Exception of type_exception node
    (* exception C of T
       exception C = M.X *)
    | Module of module_binding node
    (* module X = ME *)
    | Recmodule of module_binding node list
    (* module rec X1 = ME1 and ... and Xn = MEn *)
    | Modtype of module_type_declaration node
    (* module type S = MT *)
    | Open of open_declaration node
    (* open X *)
    | Include of include_declaration node
    (* include ME *)
    | Attribute of attribute node
    (* [@@@id] *)
    | Extension of extension node
    (* [%%id] *)

  val desc : structure_item node -> desc
end

module Value_binding : sig
  val pattern : value_binding node -> pattern node
  val expression : value_binding node -> expression node
end

module Module_binding : sig
  val name : module_binding node -> string option loc
  val expr : module_binding node -> module_expr node
end

(* X = ME *)

module Phrase : sig
  type desc =
    | Definition of structure node
    | Directive of directive node
  val desc : phrase node -> desc
end

module Directive : sig
  val name : directive node -> string loc
  val arg : directive node -> directive_argument node option
end

module Directive_argument : sig
  type desc =
    | String of string
    | Int of string * char option
    | Ident of longident
    | Bool of bool

  val desc : directive_argument node -> desc
end

module Visitor : sig
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
    | Module_type              : module_type node category
    | Module_declaration       : module_declaration node category
    | Module_substitution      : module_substitution node category
    | Module_type_declaration  : module_type_declaration node category
    | Directive                : directive node category
    | Directive_argument       : directive_argument node category
    | Phrase                   : phrase node category
  type iter = { visit: 'a. iter -> 'a category -> 'a -> unit }
  val iter : iter

  type map = { visit: 'a. map -> 'a category -> 'a -> 'a }
  val map : map
end
