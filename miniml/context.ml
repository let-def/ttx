
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

  type t
  val empty : t
  val enter : t -> 'a binder -> 'a info -> t

  val lookup : t -> 'a name -> 'a info
  val update : t -> 'a name -> 'a info -> t

  (* Creating new bindings *)

  val extend : t -> 'a namespace -> string -> 'a info -> t * 'a binder
  val reserve : t -> 'a namespace -> string -> t * 'a binder
end

module type S = sig
  type 'a namespace
  type 'a binder
  type 'a name

  val get_name : 'a binder -> 'a name
  val get_text : 'a name -> string
  val namespace : 'a name -> 'a namespace

  module Make_context(Info: TYPE) :
    CONTEXT with type 'a namespace := 'a namespace
             and type 'a binder := 'a binder
             and type 'a name := 'a name
             and type 'a info = 'a Info.t
end

module With_namespace(Namespace: NAMESPACE)
  : S with type 'a namespace = 'a Namespace.t =
struct
  type 'a namespace = 'a Namespace.t

  type 'a name = Name : {
      namespace: 'a namespace;
      index: int;
      text: string;
      prev : _ name option;
    } -> 'a name

  type a_name = A_name : 'a name -> a_name [@@ocaml.unboxed]
  type a_name_option = Opt_name : 'a name option -> a_name_option [@@ocaml.unboxed]

  type 'a binder = 'a name

  let get_name (x : 'a binder) : 'a name = x
  let get_text (Name n) = n.text
  let namespace (Name n) = n.namespace

  module Make_context(Info: TYPE) = struct
    type 'a info = 'a Info.t
    type binding =
      | Binding : {name : 'a name; info : 'a info} -> binding
      | Undefined : {name: 'a name} -> binding

    type t = binding Dbseq.t

    let is_empty = Dbseq.is_empty

    let last_binding t =
      Dbseq.get 0 t

    let last_name t =
      if is_empty t then Opt_name None else
        match last_binding t with
        | Undefined {name} -> Opt_name (Some name)
        | Binding {name; _} -> Opt_name (Some name)

    let last_index t =
      match last_name t with
      | Opt_name None -> -1
      | Opt_name (Some (Name {index; _})) -> index

    let offset_of t (Name n) =
      last_index t - n.index

    let name_prev (Name n) =
      Opt_name n.prev

    let name_index (Name n) =
      n.index

    let rec name_seq : type a . a name option -> a_name Seq.t = fun name () ->
      match name with
      | None -> Seq.Nil
      | Some (Name n as name) -> Seq.Cons (A_name name, name_seq n.prev)

    let binding_name = function
      | Binding {name; _} -> A_name name
      | Undefined {name} -> A_name name

    let dump_name (Name name) =
      let ns = Namespace.to_string name.namespace in
      Printf.sprintf "(%d) %s :: %s" name.index name.text ns

    let dump_binding = function
      | Binding {name; _} -> dump_name name ^ " (defined)"
      | Undefined {name}  -> dump_name name ^ " (undefined)"

    let dump_bindings buf seq =
      Seq.iter (fun b -> Buffer.add_string buf (dump_binding b)) seq

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

    let error_invalid_context caller expected context =
      let buf = Buffer.create 127 in
      let names, bindings, shared =
        sync_seq [] [] (name_seq (Some expected)) (Dbseq.to_seq context)
      in
      Printf.kprintf (Buffer.add_string buf)
        "%s: name %s is used in an invalid context.\n"
        caller (dump_name expected);
      Buffer.add_string buf "Expected:\n";
      List.iter
        (fun (A_name name) -> Buffer.add_string buf (dump_name name))
        (List.rev names);
      Buffer.add_string buf "Current context:\n";
      List.iter
        (fun binding -> Buffer.add_string buf (dump_binding binding))
        (List.rev bindings);
      Buffer.add_string buf "Common context:\n";
      dump_bindings buf shared;
      raise (Error (Buffer.contents buf))

    let empty = Dbseq.empty

    let next_name_from_prev namespace text = function
      | None as prev ->
        Name {namespace; index = 0; text; prev}
      | Some (Name {index; _}) as prev ->
        Name {namespace; index = index + 1; text; prev}

    let next_name t namespace text =
      if is_empty t then
        next_name_from_prev namespace text None
      else
        match last_binding t with
        | Binding {name; _} -> next_name_from_prev namespace text (Some name)
        | Undefined {name} -> next_name_from_prev namespace text (Some name)

    let reserve t namespace text =
      let name = next_name t namespace text in
      Dbseq.cons (Undefined {name}) t, name

    let extend t namespace text info =
      let name = next_name t namespace text in
      Dbseq.cons (Binding {name; info}) t, name

    let lookup t (type a) (Name n as name : a name) : a info =
      let offset = offset_of t name in
      match Dbseq.get offset t with
      | exception Not_found ->
        error_invalid_binding "lookup" "undefined" name t
      | Undefined b ->
        if same_name name b.name then
          error_invalid_binding "lookup" "undefined" name t
        else
          error_invalid_context "lookup" name t;
      | Binding b ->
        if not (same_name name b.name) then
          error_invalid_context "lookup" name t;
        let Name n' = b.name in
        match Namespace.order n'.namespace n.namespace with
        | Lt | Gt ->
          invalid_arg "Context.Make: \
                       Namespace.order does not implement a valid ordering"
        | Eq -> b.info

    let update t (type a) (name : a name) (info : a info) : t =
      let update = function
        | Binding b ->
          if not (same_name name b.name) then
            error_invalid_context "update" name t;
          Binding {name; info}
        | Undefined b ->
          if not (same_name name b.name) then
            error_invalid_context "update" name t;
          error_invalid_binding "lookup" "undefined" name t
      in
      let offset = offset_of t name in
      if offset < 0 then
        error_invalid_binding "lookup" "undefined" name t;
      Dbseq.update t (offset_of t name) update

    let rec skip i acc (Opt_name name) =
      if i = 0 then
        (Opt_name name, acc)
      else
        match name with
        | None -> assert false
        | Some (Name n as name) ->
          skip (i - 1) (Undefined {name} :: acc) (Opt_name n.prev)

    let rec sync dropped insert (Opt_name name) (Opt_name name') =
      match name, name' with
      | None, Some _ | Some _, None -> assert false
      | Some name, Some name' when not (same_name name name') ->
        assert (name_index name = name_index name');
        sync
          (dropped + 1)
          (Undefined {name} :: insert)
          (name_prev name) (name_prev name')
      | None, None | Some _, Some _ ->
        dropped, insert

    let retract_and_enter t offset name info =
      let name, to_insert =
        let Name n = name in
        Opt_name n.prev, [Binding {name; info}]
      in
      let t = if offset < 0 then t else Dbseq.drop (offset + 1) t in
      let name, to_insert =
        if offset < 0
        then skip (-offset) to_insert name
        else name, to_insert
      in
      let dropped, to_insert = sync 0 to_insert name (last_name t) in
      let insert t elt = Dbseq.cons elt t in
      List.fold_left insert (Dbseq.drop dropped t) to_insert

    let enter t name info =
      let offset = offset_of t name in
      if offset < 0 then (
        retract_and_enter t offset name info
      ) else (
        match Dbseq.get offset t with
        | Binding b ->
          if same_name name b.name then
            error_invalid_binding "enter" "already bound" name t;
          retract_and_enter t offset name info
        | Undefined b ->
          if same_name name b.name then
            Dbseq.set offset (Binding {name; info}) t
          else
            retract_and_enter t offset name info
      )
  end
end
