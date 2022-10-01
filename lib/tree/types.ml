(** Asttypes exposes basic definitions shared both by Parsetree and Types. *)
open Asttypes

type type_expr = {
  desc: type_desc;
  level: int;
  scope: int;
  id: int;
}

and type_desc =
  | Tvar of string option
  (** [Tvar (Some "a")] ==> ['a] or ['_a]
      [Tvar None]       ==> [_] *)

  | Tarrow of arg_label * type_expr * type_expr * commutable
  (** [Tarrow (Nolabel,      e1, e2, c)] ==> [e1    -> e2]
      [Tarrow (Labelled "l", e1, e2, c)] ==> [l:e1  -> e2]
      [Tarrow (Optional "l", e1, e2, c)] ==> [?l:e1 -> e2]

      See [commutable] for the last argument. *)

  | Ttuple of type_expr list
  (** [Ttuple [t1;...;tn]] ==> [(t1 * ... * tn)] *)

  | Tconstr of Path.t * type_expr list
  (** [Tconstr (`A.B.t', [t1;...;tn], _)] ==> [(t1,...,tn) A.B.t]
      The last parameter keep tracks of known expansions, see [abbrev_memo]. *)

  | Tobject of type_expr * (Path.t * type_expr list) option ref
  (** [Tobject (`f1:t1;...;fn: tn', `None')] ==> [< f1: t1; ...; fn: tn >]
      f1, fn are represented as a linked list of types using Tfield and Tnil
      constructors.

      [Tobject (_, `Some (`A.ct', [t1;...;tn]')] ==> [(t1, ..., tn) A.ct].
      where A.ct is the type of some class.

      There are also special cases for so-called "class-types", cf. [Typeclass]
      and [Ctype.set_object_name]:

        [Tobject (Tfield(_,_,...(Tfield(_,_,rv)...),
                         Some(`A.#ct`, [rv;t1;...;tn])]
             ==> [(t1, ..., tn) #A.ct]
        [Tobject (_, Some(`A.#ct`, [Tnil;t1;...;tn])] ==> [(t1, ..., tn) A.ct]

      where [rv] is the hidden row variable.
  *)

  | Tfield of string * field_kind * type_expr * type_expr
  (** [Tfield ("foo", Fpresent, t, ts)] ==> [<...; foo : t; ts>] *)

  | Tnil
  (** [Tnil] ==> [<...; >] *)

  | Tlink of type_expr
  (** Indirection used by unification engine. *)

  | Tsubst of type_expr * type_expr option
  (** [Tsubst] is used temporarily to store information in low-level
      functions manipulating representation of types, such as
      instantiation or copy.
      The first argument contains a copy of the original node.
      The second is available only when the first is the row variable of
      a polymorphic variant.  It then contains a copy of the whole variant.
      This constructor should not appear outside of these cases. *)

  | Tvariant of row_desc
  (** Representation of polymorphic variants, see [row_desc]. *)

  | Tunivar of string option
  (** Occurrence of a type variable introduced by a
      forall quantifier / [Tpoly]. *)

  | Tpoly of type_expr * type_expr list
  (** [Tpoly (ty,tyl)] ==> ['a1... 'an. ty],
      where 'a1 ... 'an are names given to types in tyl
      and occurrences of those types in ty. *)

  | Tpackage of Path.t * (Longident.t * type_expr) list
  (** Type of a first-class module (a.k.a package). *)

and row_desc = {
  row_fields: (label * row_field) list;
  row_more: type_expr;
  row_bound: unit; (* kept for compatibility *)
  row_closed: bool;
  row_fixed: fixed_explanation option;
  row_name: (Path.t * type_expr list) option;
}

and fixed_explanation =
  | Univar of type_expr (** The row type was bound to an univar *)
  | Fixed_private (** The row type is private *)
  | Reified of Path.t (** The row was reified *)
  | Rigid (** The row type was made rigid during constraint verification *)

and row_field =
    Rpresent of type_expr option
  | Reither of bool * type_expr list * bool * row_field option ref
        (* 1st true denotes a constant constructor *)
        (* 2nd true denotes a tag in a pattern matching, and
           is erased later *)
  | Rabsent

and field_kind =
  | Fvar of field_kind option
  | Fpresent
  | Fabsent

and commutable =
    Cok
  | Cunknown

(* Value descriptions *)

type uid

type value_description = {
  val_type: type_expr;
  val_kind: value_kind;
  val_loc: Location.t;
  val_attributes: Parsetree.attributes;
  val_uid: uid;
}

and value_kind =
  | Val_reg                             (* Regular value *)
  | Val_prim of Primitive.description   (* Primitive *)
  | Val_ivar of mutable_flag * string   (* Instance variable (mutable ?) *)
  | Val_self of (Ident.t * type_expr) list *
                (Ident.t * mutable_flag * virtual_flag * type_expr) list *
                string * type_expr
                                        (* Self *)
  | Val_anc of (string * Ident.t) list * string
                                        (* Ancestor *)

(* Variance *)

type variance

type variance_elt =
  | May_pos                (* allow positive occurrences *)
  | May_neg                (* allow negative occurrences *)
  | May_weak               (* allow occurrences under a negative position *)
  | Inj                    (* type is injective in this parameter *)
  | Pos                    (* there is a positive occurrence *)
  | Neg                    (* there is a negative occurrence *)
  | Inv                    (* both negative and positive occurrences *)

type separability = Ind | Sep | Deepsep

type separability_signature = separability list

(* Type definitions *)

type type_immediacy

type type_declaration = {
  type_params: type_expr list;
  type_arity: int;
  type_kind: type_decl_kind;
  type_private: private_flag;
  type_manifest: type_expr option;
  type_variance: variance list;
  (* covariant, contravariant, weakly contravariant, injective *)
  type_separability: separability list;
  type_is_newtype: bool;
  type_expansion_scope: int;
  type_loc: Location.t;
  type_attributes: Parsetree.attributes;
  type_immediate: type_immediacy;
  type_unboxed_default: bool;
  (* true if the unboxed-ness of this type was chosen by a compiler flag *)
  type_uid: uid;
}

and type_decl_kind = (label_declaration, constructor_declaration) type_kind

and ('lbl, 'cstr) type_kind =
  | Type_abstract
  | Type_record of 'lbl list  * record_representation
  | Type_variant of 'cstr list * variant_representation
  | Type_open

and record_representation =
  | Record_regular                      (* All fields are boxed / tagged *)
  | Record_float                        (* All fields are floats *)
  | Record_unboxed of bool    (* Unboxed single-field record, inlined or not *)
  | Record_inlined of int               (* Inlined record *)
  | Record_extension of Path.t          (* Inlined record under extension *)

and variant_representation =
  | Variant_regular          (* Constant or boxed constructors *)
  | Variant_unboxed          (* One unboxed single-field constructor *)

and label_declaration = {
  ld_id: Ident.t;
  ld_mutable: mutable_flag;
  ld_type: type_expr;
  ld_loc: Location.t;
  ld_attributes: Parsetree.attributes;
  ld_uid: uid;
}

and constructor_declaration = {
  cd_id: Ident.t;
  cd_args: constructor_arguments;
  cd_res: type_expr option;
  cd_loc: Location.t;
  cd_attributes: Parsetree.attributes;
  cd_uid: uid;
}

and constructor_arguments =
  | Cstr_tuple of type_expr list
  | Cstr_record of label_declaration list

type extension_constructor = {
  ext_type_path: Path.t;
  ext_type_params: type_expr list;
  ext_args: constructor_arguments;
  ext_ret_type: type_expr option;
  ext_private: private_flag;
  ext_loc: Location.t;
  ext_attributes: Parsetree.attributes;
  ext_uid: uid;
}

and type_transparence =
  | Type_public      (* unrestricted expansion *)
  | Type_new         (* "new" type *)
  | Type_private     (* private type *)

(* Type expressions for the class language *)

type class_type =
  | Cty_constr of Path.t * type_expr list * class_type
  | Cty_signature of class_signature
  | Cty_arrow of arg_label * type_expr * class_type

and class_signature = {
  csig_self: type_expr;
  csig_vars:
    (string * (Asttypes.mutable_flag * Asttypes.virtual_flag * type_expr)) list;
  csig_concr: Concr.t;
  csig_inher: (Path.t * type_expr list) list;
}

type class_declaration = {
  cty_params: type_expr list;
  mutable cty_type: class_type;
  cty_path: Path.t;
  cty_new: type_expr option;
  cty_variance: Variance.t list;
  cty_loc: Location.t;
  cty_attributes: Parsetree.attributes;
  cty_uid: Uid.t;
}

type class_type_declaration = {
  clty_params: type_expr list;
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
  | Mty_ident of Path.t
  | Mty_signature of signature
  | Mty_functor of functor_parameter * module_type
  | Mty_alias of Path.t

and functor_parameter =
  | Fp_unit
  | Fp_named of Ident.t option * module_type

and module_presence =
  | Mp_present
  | Mp_absent

and signature = signature_item list

and signature_item =
  | Sig_value      of Ident.t * value_description * visibility
  | Sig_type       of Ident.t * type_declaration * rec_status * visibility
  | Sig_typext     of Ident.t * extension_constructor * ext_status * visibility
  | Sig_module     of Ident.t * module_presence * module_declaration * rec_status * visibility
  | Sig_modtype    of Ident.t * modtype_declaration * visibility
  | Sig_class      of Ident.t * class_declaration * rec_status * visibility
  | Sig_class_type of Ident.t * class_type_declaration * rec_status * visibility

and module_declaration = {
  md_type: module_type;
  md_attributes: Parsetree.attributes;
  md_loc: Location.t;
  md_uid: Uid.t;
}

and modtype_declaration = {
  mtd_type: module_type option;  (* None: abstract *)
  mtd_attributes: Parsetree.attributes;
  mtd_loc: Location.t;
  mtd_uid: Uid.t;
}

and rec_status =
  | Trec_not        (* first in a nonrecursive group *)
  | Trec_first      (* first in a recursive group *)
  | Trec_next       (* not first in a recursive/nonrecursive group *)

and ext_status =
  | Text_first      (* first constructor in an extension *)
  | Text_next       (* not first constructor in an extension *)
  | Text_exception


(* Constructor and record label descriptions inserted held in typing
   environments *)

type constructor_description = {
  cstr_name: string;                  (* Constructor name *)
  cstr_res: type_expr;                (* Type of the result *)
  cstr_existentials: type_expr list;  (* list of existentials *)
  cstr_args: type_expr list;          (* Type of the arguments *)
  cstr_arity: int;                    (* Number of arguments *)
  cstr_tag: constructor_tag;          (* Tag for heap blocks *)
  cstr_consts: int;                   (* Number of constant constructors *)
  cstr_nonconsts: int;                (* Number of non-const constructors *)
  cstr_normal: int;                   (* Number of non generalized constrs *)
  cstr_generalized: bool;             (* Constrained return type? *)
  cstr_private: private_flag;         (* Read-only constructor? *)
  cstr_loc: Location.t;
  cstr_attributes: Parsetree.attributes;
  cstr_inlined: type_declaration option;
  cstr_uid: uid;
}

and constructor_tag =
  | Cstr_constant of int                (* Constant constructor (an int) *)
  | Cstr_block of int                   (* Regular constructor (a block) *)
  | Cstr_unboxed                        (* Constructor of an unboxed type *)
  | Cstr_extension of Path.t * bool     (* Extension constructor
                                           true if a constant false if a block*)

type label_description = {
  lbl_name: string;                   (* Short name *)
  lbl_res: type_expr;                 (* Type of the result *)
  lbl_arg: type_expr;                 (* Type of the argument *)
  lbl_mut: mutable_flag;              (* Is this a mutable field? *)
  lbl_pos: int;                       (* Position in block *)
  lbl_all: label_description array;   (* All the labels in this type *)
  lbl_repres: record_representation;  (* Representation for this record *)
  lbl_private: private_flag;          (* Read-only field? *)
  lbl_loc: Location.t;
  lbl_attributes: Parsetree.attributes;
  lbl_uid: uid;
}
