open Ttx_def

let position {Lexing. pos_fname; pos_lnum; pos_cnum; pos_bol} =
  Cmon.record [
    "pos_fname" , Cmon.string pos_fname;
    "pos_lnum"  , Cmon.int pos_lnum;
    "pos_cnum"  , Cmon.int pos_cnum;
    "pos_bol"   , Cmon.int pos_bol;
  ]

let location {loc_start; loc_end; loc_ghost} =
  Cmon.record [
    "loc_start" , position loc_start;
    "loc_end"   , position loc_end;
    "loc_ghost" , Cmon.bool loc_ghost;
  ]

let option f = function
  | None -> Cmon.constant "None"
  | Some x -> Cmon.constructor "Some" (f x)

let constant = function
  | Const_int x ->
    Cmon.constructor "Const_int" (Cmon.int x)
  | Const_char x ->
    Cmon.constructor "Const_char" (Cmon.char x)
  | Const_string (s1, loc, s2) ->
    Cmon.construct "Const_string"
      [Cmon.string s1; location loc; option Cmon.string s2]
  | Const_float x ->
    Cmon.constructor "Const_float" (Cmon.string x)
  | Const_int32 x ->
    Cmon.constructor "Const_int32" (Cmon.int32 x)
  | Const_int64 x ->
    Cmon.constructor "Const_int64" (Cmon.int64 x)
  | Const_nativeint x ->
    Cmon.constructor "Const_nativeint" (Cmon.nativeint x)

let rec_flag = function
  | Nonrecursive -> Cmon.constant "Nonrecursive"
  | Recursive    -> Cmon.constant "Recursive"

let direction_flag = function
  | Upto   -> Cmon.constant "Upto"
  | Downto -> Cmon.constant "Downto"

let private_flag = function
  | Private -> Cmon.constant "Private"
  | Public  -> Cmon.constant "Public"

let mutable_flag = function
  | Immutable -> Cmon.constant "Immutable"
  | Mutable   -> Cmon.constant "Mutable"

let virtual_flag = function
  | Virtual  -> Cmon.constant "Virtual"
  | Concrete -> Cmon.constant "Concrete"

let override_flag = function
  | Override -> Cmon.constant "Override"
  | Fresh    -> Cmon.constant "Fresh"

let closed_flag = function
  | Closed -> Cmon.constant "Closed"
  | Open   -> Cmon.constant "Open"

let arg_label = function
  | Nolabel    -> Cmon.constant "Nolabel"
  | Labelled x -> Cmon.constructor "Labelled" (Cmon.string x)
  | Optional x -> Cmon.constructor "Optional" (Cmon.string x)

let variance = function
  | Covariant     -> Cmon.constant "Covariant"
  | Contravariant -> Cmon.constant "Contravariant"
  | NoVariance    -> Cmon.constant "NoVariance"

let injectivity = function
  | Injective     -> Cmon.constant "Injective"
  | NoInjectivity -> Cmon.constant "NoInjectivity"

let module_presence = function
  | Present -> Cmon.constant "Present"
  | Absent  -> Cmon.constant "Absent"

let partial = function
  | Partial -> Cmon.constant "Partial"
  | Total   -> Cmon.constant "Total"

let rec longident = function
  | Lident x ->
    Cmon.constructor "Lident" (Cmon.string x)
  | Ldot (l, x) ->
    Cmon.construct "Ldot" [longident l; Cmon.string x]
  | Lapply (l1, l2) ->
    Cmon.construct "Lapply" [longident l1; longident l2]

let attribute _ = Cmon.constant "attribute TODO"
let attributes _ = Cmon.constant "attributes TODO"

let vector f v =
  Cmon.array_map f (Vector.unsafe_to_array v)

let namespace : type a. a namespace -> Cmon.t = function
  | Value       -> Cmon.constant "Value"
  | Type        -> Cmon.constant "Type"
  | Type_level  -> Cmon.constant "Type_level"
  | Module      -> Cmon.constant "Module"
  | Module_type -> Cmon.constant "Module_type"

let name n =
  Cmon.construct "Name" [
    Cmon.string (get_text n);
    namespace (get_namespace n);
    Cmon.int (get_depth n);
  ]

let binder b =
  let n = get_name b in
  Cmon.construct "Binder" [
    Cmon.string (get_text n);
    namespace (get_namespace n);
    Cmon.int (get_depth n);
  ]

let namegroup ng =
  Cmon.constructor "Namegroup"
    (Cmon.list_map (fun (Name n) -> name n) (get_names ng))

let type_level tl =
  Cmon.crecord "Type_level" [
    "binder", binder (Type_level.binder tl);
    "variables", Cmon.array (
      Array.init (Type_level.count tl)
        (fun i -> option Cmon.string (Type_level.get_name tl i))
    )
  ]

let rec path : type ns. ns path -> Cmon.t = function
  | Path.Ident n -> Cmon.constructor "Ident" (name n)
  | Path.Dot (p, s) -> Cmon.construct "Dot" [path p; Cmon.string s]

let type_level_variable tv =
  Cmon.record [
    "level" , name (Type_level.level tv);
    "index" , Cmon.int (Type_level.index tv);
  ]

let binding def body binding =
  Cmon.crecord "Binding" [
    "namegroup" , namegroup (Binding.names binding);
    "def"       , def (Binding.def binding);
    "body"      , body (Binding.body binding);
  ]
