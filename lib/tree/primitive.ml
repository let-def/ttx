(* Description of primitive functions *)

type boxed_integer = Pnativeint | Pint32 | Pint64

(* Representation of arguments/result for the native code version
   of a primitive *)

type native_repr =
  | Same_as_ocaml_repr
  | Unboxed_float
  | Unboxed_integer of boxed_integer
  | Untagged_int

type description = {
  prim_name: string;         (* Name of primitive  or C function *)
  prim_arity: int;           (* Number of arguments *)
  prim_alloc: bool;          (* Does it allocates or raise? *)
  prim_native_name: string;  (* Name of C function for the nat. code gen. *)
  prim_native_repr_args: native_repr list;
  prim_native_repr_res: native_repr;
}
