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
