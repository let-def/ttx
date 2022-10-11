val string : int -> string -> string

type output
val make : (string -> int -> int -> unit) -> output
val shift : int -> output -> output
val print : output -> string -> unit
val printf : output -> ('a, output, unit) format ->  'a
