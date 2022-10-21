open Ttx_base

type ('a, 'b) binder = ('a, 'b) Binder.t

module Arg_label : sig
  type t =
    | Nolabel
    | Labelled of string
    | Optional of string
end
type arg_label = Arg_label.t

module Type_expr : sig
  include INDEXABLE

  type arg_label =
    | Nolabel
    | Labelled of string
    | Optional of string

  type desc =
    | Tarrow of arg_label * t * t
    | Ttuple of t list
    | Tconstr of t list * path

  val desc : t -> desc

end

module type LOCATED = sig
  include INDEXABLE
  val location :

end

module Value : sig
  include INDEXABLE

end

(* Value descriptions *)

type value_description =
  { val_type: type_expr;                (* Type of the value *)
    val_kind: value_kind;
    val_loc: Location.t;
    val_attributes: Parsetree.attributes;
  }

and value_kind =
    Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of class_signature * self_meths * Ident.t Vars.t * string
                                        (* Self *)
  | Val_anc of class_signature * Ident.t Meths.t * string
                                        (* Ancestor *)

and self_meths =
  | Self_concrete of Ident.t Meths.t
  | Self_virtual of Ident.t Meths.t ref

and class_signature =
  { csig_self: type_expr;
    mutable csig_self_row: type_expr;
    mutable csig_vars: (mutable_flag * virtual_flag * type_expr) Vars.t;
    mutable csig_meths: (private_flag * virtual_flag * type_expr) Meths.t; }

(* Variance *)

module Variance : sig
  type t
  type f =
      May_pos                (* allow positive occurrences *)
    | May_neg                (* allow negative occurrences *)
    | May_weak               (* allow occurrences under a negative position *)
    | Inj                    (* type is injective in this parameter *)
    | Pos                    (* there is a positive occurrence *)
    | Neg                    (* there is a negative occurrence *)
    | Inv                    (* both negative and positive occurrences *)
  val null : t               (* no occurrence *)
  val full : t               (* strictly invariant (all flags) *)
  val covariant : t          (* strictly covariant (May_pos, Pos and Inj) *)
  val unknown : t            (* allow everything, guarantee nothing *)
  val union  : t -> t -> t
  val inter  : t -> t -> t
  val subset : t -> t -> bool
  val eq : t -> t -> bool
  val set : f -> bool -> t -> t
  val mem : f -> t -> bool
  val conjugate : t -> t                (* exchange positive and negative *)
  val get_upper : t -> bool * bool                  (* may_pos, may_neg   *)
  val get_lower : t -> bool * bool * bool * bool    (* pos, neg, inv, inj *)
  val unknown_signature : injective:bool -> arity:int -> t list
  (** The most pessimistic variance for a completely unknown type. *)
end

module Separability : sig
  (** see {!Typedecl_separability} for an explanation of separability
      and separability modes.*)

  type t = Ind | Sep | Deepsep
  val eq : t -> t -> bool
  val print : Format.formatter -> t -> unit

  val rank : t -> int
  (** Modes are ordered from the least to the most demanding:
      Ind < Sep < Deepsep.
      'rank' maps them to integers in an order-respecting way:
      m1 < m2  <=>  rank m1 < rank m2 *)

  val compare : t -> t -> int
  (** Compare two mode according to their mode ordering. *)

  val max : t -> t -> t
  (** [max_mode m1 m2] returns the most demanding mode. It is used to
      express the conjunction of two parameter mode constraints. *)

  type signature = t list
  (** The 'separability signature' of a type assigns a mode for
      each of its parameters. [('a, 'b) t] has mode [(m1, m2)] if
      [(t1, t2) t] is separable whenever [t1, t2] have mode [m1, m2]. *)

  val print_signature : Format.formatter -> signature -> unit

  val default_signature : arity:int -> signature
  (** The most pessimistic separability for a completely unknown type. *)
end

(* Type definitions *)

type type_declaration =
  { type_params: type_expr list;
    type_arity: int;
    type_kind: type_decl_kind;
    type_private: private_flag;
    type_manifest: type_expr option;
    type_variance: Variance.t list;
    (* covariant, contravariant, weakly contravariant, injective *)
    type_separability: Separability.t list;
    type_is_newtype: bool;
    type_expansion_scope: int;
    type_loc: Location.t;
    type_attributes: Parsetree.attributes;
    type_immediate: Type_immediacy.t;
    type_unboxed_default: bool;
    (* true if the unboxed-ness of this type was chosen by a compiler flag *)
    type_uid: Uid.t;
  }

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind =
    Type_abstract
  | Type_record of 'lbl list  * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and record_representation =
    Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)

and variant_representation =
    Variant_regular          (* Constant or boxed constructors *)
  | Variant_unboxed          (* One unboxed single-field constructor *)

and label_declaration =
  {
    ld_id: Ident.t;
    ld_mutable: mutable_flag;
    ld_type: type_expr;
    ld_loc: Location.t;
    ld_attributes: Parsetree.attributes;
    ld_uid: Uid.t;
  }

and constructor_declaration =
  {
    cd_id: Ident.t;
    cd_args: constructor_arguments;
    cd_res: type_expr option;
    cd_loc: Location.t;
    cd_attributes: Parsetree.attributes;
    cd_uid: Uid.t;
  }

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

type extension_constructor =
  {
    ext_type_path: Path.t;
    ext_type_params: type_expr list;
    ext_args: constructor_arguments;
    ext_ret_type: type_expr option;
    ext_private: private_flag;
    ext_loc: Location.t;
    ext_attributes: Parsetree.attributes;
    ext_uid: Uid.t;
  }

and type_transparence =
    Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

type class_type =
    Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

type class_declaration =
  { cty_params: type_expr list;
    mutable cty_type: class_type;
    cty_path: Path.t;
    cty_new: type_expr option;
    cty_variance: Variance.t list;
    cty_loc: Location.t;
    cty_attributes: Parsetree.attributes;
    cty_uid: Uid.t;
  }

type class_type_declaration =
  { clty_params: type_expr list;
    clty_type: class_type;
    clty_path: Path.t;
    clty_variance: Variance.t list;
    clty_loc: Location.t;
    clty_attributes: Parsetree.attributes;
    clty_uid: Uid.t;
  }

(* Type expressions for the module language *)

type visibility =
  | Exported
  | Hidden

type module_type =
    Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Unit
  | Named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item =
    Sig_value of Ident.t * value_description * visibility
  | Sig_type of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module of
      Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype of Ident.t * modtype_declaration * visibility
  | Sig_class of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

and module_declaration =
  {
    md_type: module_type;
    md_attributes: Parsetree.attributes;
    md_loc: Location.t;
    md_uid: Uid.t;
  }

and modtype_declaration =
  {
    mtd_type: module_type option;  (* None: abstract *)
    mtd_attributes: Parsetree.attributes;
    mtd_loc: Location.t;
    mtd_uid: Uid.t;
  }

and rec_status =
    Trec_not                   (* first in a nonrecursive group *)
  | Trec_first                 (* first in a recursive group *)
  | Trec_next                  (* not first in a recursive/nonrecursive group *)

and ext_status =
    Text_first                     (* first constructor in an extension *)
  | Text_next                      (* not first constructor in an extension *)
  | Text_exception


(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description =
  { cstr_name: string;                  (* Constructor name *)
    cstr_res: type_expr;                (* Type of the result *)
    cstr_existentials: type_expr list;  (* list of existentials *)
    cstr_args: type_expr list;          (* Type of the arguments *)
    cstr_arity: int;                    (* Number of arguments *)
    cstr_tag: constructor_tag;          (* Tag for heap blocks *)
    cstr_consts: int;                   (* Number of constant constructors *)
    cstr_nonconsts: int;                (* Number of non-const constructors *)
    cstr_generalized: bool;             (* Constrained return type? *)
    cstr_private: private_flag;         (* Read-only constructor? *)
    cstr_loc: Location.t;
    cstr_attributes: Parsetree.attributes;
    cstr_inlined: type_declaration option;
    cstr_uid: Uid.t;
   }

and constructor_tag =
    Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

(* Constructors are the same *)
val equal_tag :  constructor_tag -> constructor_tag -> bool

(* Constructors may be the same, given potential rebinding *)
val may_equal_constr :
    constructor_description ->  constructor_description -> bool

type label_description =
  { lbl_name: string;                   (* Short name *)
    lbl_res: type_expr;                 (* Type of the result *)
    lbl_arg: type_expr;                 (* Type of the argument *)
    lbl_mut: mutable_flag;              (* Is this a mutable field? *)
    lbl_pos: int;                       (* Position in block *)
    lbl_all: label_description array;   (* All the labels in this type *)
    lbl_repres: record_representation;  (* Representation for this record *)
    lbl_private: private_flag;          (* Read-only field? *)
    lbl_loc: Location.t;
    lbl_attributes: Parsetree.attributes;
    lbl_uid: Uid.t;
  }

(** Extracts the list of "value" identifiers bound by a signature.
    "Value" identifiers are identifiers for signature components that
    correspond to a run-time value: values, extensions, modules, classes.
    Note: manifest primitives do not correspond to a run-time value! *)
val bound_value_identifiers: signature -> Ident.t list

val signature_item_id : signature_item -> Ident.t

(**** Utilities for backtracking ****)

type snapshot
        (* A snapshot for backtracking *)
val snapshot: unit -> snapshot
        (* Make a snapshot for later backtracking. Costs nothing *)
val backtrack: cleanup_abbrev:(unit -> unit) -> snapshot -> unit
        (* Backtrack to a given snapshot. Only possible if you have
           not already backtracked to a previous snapshot.
           Calls [cleanup_abbrev] internally *)
val undo_first_change_after: snapshot -> unit
        (* Backtrack only the first change after a snapshot.
           Does not update the list of changes *)
val undo_compress: snapshot -> unit
        (* Backtrack only path compression. Only meaningful if you have
           not already backtracked to a previous snapshot.
           Does not call [cleanup_abbrev] *)

(** Functions to use when modifying a type (only Ctype?).
    The old values are logged and reverted on backtracking.
 *)

val link_type: type_expr -> type_expr -> unit
        (* Set the desc field of [t1] to [Tlink t2], logging the old
           value if there is an active snapshot *)
val set_type_desc: type_expr -> type_desc -> unit
        (* Set directly the desc field, without sharing *)
val set_level: type_expr -> int -> unit
val set_scope: type_expr -> int -> unit
val set_name:
    (Path.t * type_expr list) option ref ->
    (Path.t * type_expr list) option -> unit
val link_row_field_ext: inside:row_field -> row_field -> unit
        (* Extract the extension variable of [inside] and set it to the
           second argument *)
val set_univar: type_expr option ref -> type_expr -> unit
val link_kind: inside:field_kind -> field_kind -> unit
val link_commu: inside:commutable -> commutable -> unit
val set_commu_ok: commutable -> unit
