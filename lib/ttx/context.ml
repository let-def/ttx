type ('a, 'b) eq = Refl : ('a, 'a) eq
type ('a, 'b) type_ordering = Lt | Eq : ('a, 'a) type_ordering | Gt

module type TYPE = sig
  type 'a t
end

module type NAMESPACE = sig
  include TYPE
  val to_string : 'a t -> string
  val order : 'a t -> 'b t -> ('a, 'b) type_ordering
end

exception Error of string

let () = Printexc.register_printer (function
    | Error msg -> Some ("Context.Error: " ^ msg)
    | _ -> None
  )

module type CONTEXT = sig
  type 'a namespace
  type 'a binder
  type 'a name
  type 'a info
  type namegroup

  type t
  val empty : t
  val enter : t -> namegroup -> t
  val bind : t -> 'a binder -> 'a info -> t
  val lookup : t -> 'a name -> 'a info
  val update : t -> 'a name -> 'a info -> t

  (* Creating new bindings *)

  val empty_group : t -> namegroup
  val extend : namegroup -> 'a namespace -> string -> namegroup * 'a binder
end

module type S = sig
  type 'a namespace
  type 'a binder
  type 'a name
  type namegroup

  val get_name : 'a binder -> 'a name
  val get_text : 'a name -> string
  val get_depth : 'a name -> int
  val get_namespace : 'a name -> 'a namespace

  type a_name = Name : 'a name -> a_name
  val get_names : namegroup -> a_name list

  module Make_context(Info: TYPE) :
    CONTEXT with type 'a info = 'a Info.t
             and type 'a namespace := 'a namespace
             and type 'a binder := 'a binder
             and type 'a name := 'a name
             and type namegroup := namegroup
end

module With_namespace(Namespace: NAMESPACE)
  : S with type 'a namespace = 'a Namespace.t =
struct
  type 'a namespace = 'a Namespace.t

  type 'a name = Name_ : {
      namespace: 'a namespace;
      index: int;
      text: string;
      prev : prev_name;
    } -> 'a name

  and prev_name =
    | Prev_name : 'a name -> prev_name
    | Prev_group : 'a name -> prev_name
    | Prev_origin

  type a_name = Name : 'a name -> a_name

  type 'a binder = 'a name

  type namegroup = {
    group_curr: prev_name;
    group_prev: prev_name;
  }

  let get_name (x : 'a binder) : 'a name = x
  let get_text (Name_ n) = n.text
  let get_depth (Name_ n) = n.index
  let get_namespace (Name_ n) = n.namespace

  let name_prev (Name_ n) = n.prev
  let name_index (Name_ n) = n.index

  let get_names g =
    let rec aux acc = function
      | Prev_name name -> aux (Name name :: acc) (name_prev name)
      | Prev_group _ | Prev_origin -> acc
    in
    aux [] g.group_curr

  module Make_context(Info: TYPE) = struct
    type 'a info = 'a Info.t
    type binding =
      | Binding : {name : 'a name; info : 'a info} -> binding
      | Undefined : {name: 'a name} -> binding

    type t = binding Dbseq.t

    type a_name = A_name : 'a name -> a_name [@@ocaml.unboxed]

    let is_empty = Dbseq.is_empty

    let last_binding t =
      Dbseq.get 0 t

    let last_name t =
      if is_empty t then Prev_origin else
        match last_binding t with
        | Undefined {name} -> Prev_name name
        | Binding {name; _} -> Prev_name name

    let prev_index = function
      | Prev_origin -> -1
      | Prev_group (Name_ {index; _}) | Prev_name (Name_ {index; _}) -> index

    let last_index t =
      prev_index (last_name t)

    let offset_of t (Name_ n) =
      last_index t - n.index

    let rec name_seq : prev_name -> a_name Seq.t = fun pname () ->
      match pname with
      | Prev_origin -> Seq.Nil
      | Prev_name (Name_ n as name) -> Seq.Cons (A_name name, name_seq n.prev)
      | Prev_group (Name_ n as name) -> Seq.Cons (A_name name, name_seq n.prev)

    let binding_name = function
      | Binding {name; _} -> A_name name
      | Undefined {name} -> A_name name

    let dump_name (Name_ name) =
      let ns = Namespace.to_string name.namespace in
      Printf.sprintf "(%d) %s :: %s" name.index name.text ns

    let dump_a_name (A_name name) =
      dump_name name

    let name_end_of_group (Name_ n) = match n.prev with
      | Prev_name _ -> false
      | _ -> true

    let a_name_end_of_group (A_name n) =
      name_end_of_group n

    let dump_binding = function
      | Binding {name; _} -> dump_name name ^ " (defined)"
      | Undefined {name}  -> dump_name name ^ " (undefined)"

    let binding_end_of_group = function
      | Binding {name; _} -> name_end_of_group name
      | Undefined {name}  -> name_end_of_group name

    let dump_group buf dump_elt end_of_group seq =
      let rec loop acc seq () = match seq () with
        | Seq.Nil ->
          if acc = []
          then Seq.Nil
          else Seq.Cons (acc, Seq.empty)
        | Seq.Cons (elt, seq') ->
          if end_of_group elt
          then Seq.Cons (acc, loop [elt] seq')
          else loop (elt :: acc) seq ()
      in
      let groups = loop [] seq in
      Seq.iter (fun elts ->
          Buffer.add_string buf "{";
          List.iteri (fun i elt ->
              if i <> 0 then Buffer.add_string buf ", ";
              Buffer.add_string buf (dump_elt elt);
            ) elts;
          Buffer.add_string buf "}\n";
        ) groups

    let dump_bindings buf seq =
      dump_group buf dump_binding binding_end_of_group seq

    let same_name (type a b) (a : a name) (b : b name) : bool =
      Obj.repr a == Obj.repr b

    let cons' xs x = x :: xs

    let rec sync_seq names bindings sname sbinding =
      match sname (), sbinding () with
      | Seq.Cons (A_name name, sname'),
        Seq.Cons (binding, sbinding') ->
        let A_name bname = binding_name binding in
        if same_name name bname
        then names, bindings, sbinding
        else
          let names, sname =
            if name_index name >= name_index bname
            then (A_name name :: names, sname')
            else (names, sname)
          in
          let bindings, sbinding =
            if name_index bname >= name_index name
            then (binding :: bindings, sbinding')
            else (bindings, sbinding)
          in
          sync_seq names bindings sname sbinding
      | _, _ ->
        (Seq.fold_left cons' names sname,
         Seq.fold_left cons' bindings sbinding,
         Seq.empty)

    let error_invalid_binding caller reason name context =
      let buf = Buffer.create 127 in
      Printf.kprintf (Buffer.add_string buf)
        "%s: name %s is %s in context:\n"
        caller reason (dump_name name);
      dump_bindings buf (Dbseq.to_seq context);
      raise (Error (Buffer.contents buf))

    let error_invalid_context caller kind expected context =
      let buf = Buffer.create 127 in
      let names, bindings, shared =
        sync_seq [] [] (name_seq (Prev_name expected)) (Dbseq.to_seq context)
      in
      begin match kind with
        | `Name_ ->
          Printf.kprintf (Buffer.add_string buf)
            "%s: name %s is used in an invalid context:\n" caller (dump_name expected)
        | `Group ->
          Printf.kprintf (Buffer.add_string buf)
            "%s: group is used in an invalid context:\n" caller
      end;
      Buffer.add_string buf "Expected:\n";
      dump_group buf dump_a_name a_name_end_of_group
        (List.to_seq (List.rev names));
      Buffer.add_string buf "Current context:\n";
      dump_group buf dump_binding binding_end_of_group
        (List.to_seq (List.rev bindings));
      Buffer.add_string buf "Common context:\n";
      dump_bindings buf shared;
      raise (Error (Buffer.contents buf))

    let error_invalid_context' caller kind (A_name name) context =
      error_invalid_context caller kind name context

    let empty = Dbseq.empty

    let empty_group t =
      let prev =
        if Dbseq.is_empty t then Prev_origin
        else match last_binding t with
          | Binding x -> Prev_group x.name
          | Undefined x -> Prev_group x.name
      in
      {group_curr = prev; group_prev = prev}

    let next_name_from_prev namespace text prev =
      Name_ {namespace; index = prev_index prev + 1; text; prev}

    let extend g ns text =
      let name = next_name_from_prev ns text g.group_curr in
      let g = {group_curr = Prev_name name; group_prev = g.group_prev} in
      (g, name)

    let lookup t (type a) (Name_ n as name : a name) : a info =
      let offset = offset_of t name in
      match Dbseq.get offset t with
      | exception Not_found ->
        error_invalid_binding "lookup" "undefined" name t
      | Undefined b ->
        if same_name name b.name then
          error_invalid_binding "lookup" "undefined" name t
        else
          error_invalid_context "lookup" `Name_ name t;
      | Binding b ->
        if not (same_name name b.name) then
          error_invalid_context "lookup" `Name_ name t;
        let Name_ n' = b.name in
        match Namespace.order n'.namespace n.namespace with
        | Lt | Gt ->
          invalid_arg "Context.Make: \
                       Namespace.order does not implement a valid ordering"
        | Eq -> b.info

    let update t (type a) (name : a name) (info : a info) : t =
      let update = function
        | Binding b ->
          if not (same_name name b.name) then
            error_invalid_context "update" `Name_ name t;
          Binding {name; info}
        | Undefined b ->
          if not (same_name name b.name) then
            error_invalid_context "update" `Name_ name t;
          error_invalid_binding "lookup" "undefined" name t
      in
      let offset = offset_of t name in
      if offset < 0 then
        error_invalid_binding "lookup" "undefined" name t;
      Dbseq.update t (offset_of t name) update

    let enter t group =
      let t =
        match group.group_prev with
        | Prev_origin -> empty
        | Prev_name _ -> assert false
        | Prev_group name ->
          let offset = offset_of t name in
          let curr = match group.group_curr with
            | Prev_name name -> A_name name
            | Prev_group name -> A_name name
            | Prev_origin -> assert false
          in
          if offset < 0 then (
            error_invalid_context' "enter" `Group curr t;
          );
          let t = Dbseq.drop offset t in
          match last_name t with
          | Prev_origin | Prev_group _ -> assert false
          | Prev_name name' ->
            if not (same_name name name') then
              error_invalid_context' "enter" `Group curr t;
            t
      in
      let rec loop acc = function
        | Prev_name name -> loop (A_name name :: acc) (name_prev name)
        | Prev_origin | Prev_group _ -> acc
      in
      let names = loop [] group.group_curr in
      let t =
        List.fold_left
          (fun t (A_name name) -> Dbseq.cons (Undefined {name}) t)
          t names
      in
      if is_empty t
      then assert (group.group_curr = Prev_origin)
      else assert (last_index t + 1 = Dbseq.length t);
      t

    let bind t name info =
      let offset = offset_of t name in
      if offset < 0 then (
        error_invalid_binding "bind" "group not entered" name t;
      ) else (
        match Dbseq.get offset t with
        | Binding b ->
          if not (same_name name b.name) then
            error_invalid_binding "bind" "group not entered" name t;
          error_invalid_binding "enter" "already bound" name t
        | Undefined b ->
          if not (same_name name b.name) then
            error_invalid_binding "bind" "group not entered" name t;
          Dbseq.set offset (Binding {name; info}) t
      )
  end
end
