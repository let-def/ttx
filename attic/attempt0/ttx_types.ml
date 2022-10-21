module Arg_label : sig
  type t =
    | Nolabel
    | Labelled of string
    | Optional of string
end
type arg_label = Arg_label.t

