open Cmon

let option f = function
  | None -> construct "None" []
  | Some x -> construct "Some" [f x]

let pair fx fy (x, y) = tuple [fx x; fy y]

let tup3 f1 f2 f3 (v1, v2, v3) =
  tuple [f1 v1; f2 v2; f3 v3]

let tup4 f1 f2 f3 f4 (v1, v2, v3, v4) =
  tuple [f1 v1; f2 v2; f3 v3; f4 v4]

let cmon_ref f {contents} =
  record ["contents", f contents]

module PhTable = Hashtbl.Make(struct
    type t = Obj.t
    let equal = (==)
    let hash = Hashtbl.hash
  end)

module Memo : sig
  type 'a t
  val create : unit -> 'a t
  val find : 'a t -> 'a -> Cmon.t
  val mem : 'a t -> 'a -> bool
  val add : 'a t -> 'a -> Cmon.t -> unit
  val lookup : 'a t -> 'a -> Cmon.t option
end = struct
  type 'a t = Cmon.t PhTable.t
  let create () = PhTable.create 7
  let find t x = PhTable.find t (Obj.repr x)
  let mem t x = PhTable.mem t (Obj.repr x)
  let add t x c = PhTable.replace t (Obj.repr x) c
  let lookup t x = PhTable.find_opt t (Obj.repr x)
end

