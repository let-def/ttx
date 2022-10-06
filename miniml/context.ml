
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

let error_f fmt =
  Printf.ksprintf (fun msg -> raise (Error msg)) fmt

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
  exception Undefined

  val update : t -> 'a name -> 'a info -> t

  (* Creating new bindings *)

  val extend : t -> 'a namespace -> string -> 'a info -> t * 'a binder
  val reserve : t -> 'a namespace -> string -> t * 'a binder
end

module type S = sig
  type 'a namespace
  type 'a binder
  type 'a name
  type binding_group

  val name : 'a binder -> 'a name
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

  type 'a binder = 'a name

  let name (x : 'a binder) : 'a name = x
  let namespace (Name n) = n.namespace

  module Make_context(Info: TYPE) = struct
    type 'a info = 'a Info.t
    type binding =
      | Binding : {
          name : 'a name;
          info : 'a info;
        } -> binding
      | Undefined : 'a name -> binding

    type t = {
      count: int;
      undefined: int;
      seq: binding Dbseq.t;
    }

    let last_binding ctx =
      Dbseq.get 0 ctx.seq

    let last_binding_opt ctx =
      match last_binding ctx with
      | v -> Some v
      | exception Not_found -> None

    let dump_name (Name name) =
      let ns = Namespace.to_string name.namespace in
      Printf.sprintf "%s :: %s" name.text ns

    let rec dump_n_names : type a . int -> a name -> string list -> string list =
      fun n name acc ->
      if n = 0 then acc
      else
        let acc = dump_name name :: acc in
        match name with
        | Name {prev = None; _} -> acc
        | Name {prev = Some name'; _} -> dump_n_names (n - 1) name' acc

    let dump_name_suffix = function
      | None -> "[]"
      | Some (Name {index; _} as name) ->
        let names = dump_n_names 10 name [] in
        let remaining = index + 1 - 10 in
        let names =
          if remaining > 0 then
            Printf.sprintf "...(%d more names)..." remaining :: names
          else names
        in
        "[" ^ String.concat "; " names ^ "]"

    let dump_binding_suffix = function
      | Some (Binding {name; _}) -> dump_name_suffix (Some name)
      | Some (Undefined name) -> dump_name_suffix (Some name)
      | None -> dump_name_suffix None

    let undefined_bindings ctx =
      match ctx.group with
      | None -> []
      | Some (unbound, _) ->
        let rec take_undefined acc n seq =
          if n = 0 then acc else
            match seq () with
            | Seq.Nil -> assert false
            | Seq.Cons (Undefined name, seq') ->
              take_undefined (dump_name name :: acc) (n - 1) seq'
            | Seq.Cons (Binding _, seq') ->
              take_undefined acc n seq'
        in
        take_undefined [] unbound (Dbseq.to_seq ctx.seq)

    let wrong_context caller (Name {prev;_} as name) binding =
      error_f
        "%s: binding %s expects context %s, current context is %s"
        caller
        (dump_name name)
        (dump_name_suffix prev)
        (dump_binding_suffix binding)

    let incomplete_group msg ctx =
      error_f "%s, current binding group is incomplete:\n missing [%s]"
        msg (String.concat "; " (undefined_bindings ctx))

    let empty = { count = 0; seq = Dbseq.empty; group = None }

    let add_binding ctx name info =
      assert (Option.is_none ctx.group);
      let seq = Dbseq.cons (Binding {name; info}) ctx.seq in
      { count = ctx.count + 1; seq; group = None }

    let fresh ctx namespace text =
      if Option.is_some ctx.group then
        incomplete_group "fresh: cannot create fresh binding" ctx;
      match ctx.count with
      | 0 -> Name {namespace; index = 0; text; prev = None}
      | n ->
        match last_binding ctx with
        | Binding {name; _} ->
          Name {namespace; index = n; text; prev = Some name}
        | Undefined _ -> assert false

    let same_name (type a b) (a : a name) (b : b name) : bool =
      Obj.repr a == Obj.repr b

    let internal_retract caller ctx (Name {index; prev; text=_; namespace=_} as name) =
      if Option.is_some ctx.group then
        incomplete_group "retract: cannot retract" ctx;
      match prev with
      | None -> empty
      | Some prev ->
        let seq = Dbseq.drop (ctx.count - index) ctx.seq in
        match Dbseq.get 0 seq with
        | exception Not_found ->
          wrong_context caller name None
        | Binding {name; _} as binding ->
          if not (same_name name prev) then
            wrong_context caller name (Some binding);
          {count = index; seq; group = None}
        | Undefined _ -> assert false

    let retract ctx name =
      internal_retract "retract" ctx name

    let enter_group ctx (Group g as group) =
      if Option.is_some ctx.group then
        incomplete_group "enter_group: cannot enter new group" ctx;
      match g.prev with
      | None when g.size = 0 -> empty
      | None ->
        let rec visit : type a . a binder option -> _ = function
          | None -> Dbseq.empty
          | Some (Name n as name) ->
            let seq = visit n.prev in
            Dbseq.cons (Undefined name) seq
        in
        let seq = visit g.last in
        {count = g.size; seq; group = Some (g.size, group)}
      | Some (Name {index; _} as prev) ->
        if ctx.count <= index then
          wrong_context "enter_group" prev (last_binding_opt ctx);
        let seq = Dbseq.drop (ctx.count - index - 1) ctx.seq in
        match Dbseq.get 0 seq with
        | Undefined _ -> assert false
        | Binding {name; _} as binding ->
          if not (same_name prev name) then
            wrong_context "enter_group" prev (Some binding);
          let rec visit : type a . a binder option -> _ = function
            | None -> seq
            | Some (Name n as name) ->
              let seq = visit n.prev in
              Dbseq.cons (Undefined name) seq
          in
          let seq = visit g.last in
          { count = index + 1 + g.size; seq; group = Some (g.size, group) }

    let check_group_bound t =
      if Option.is_some t.group then
        incomplete_group "check_group_bound: check failed" t

    let enter ctx (Name n as name) info =
      match ctx.group with
      | None ->
        let valid = match n.prev with
          | None -> ctx.count = 0
          | Some prev ->
            match last_binding ctx with
            | Binding {name=prev'; _} -> same_name prev prev'
            | Undefined _ -> assert false
        in
        if not valid then
          wrong_context "enter" name (last_binding_opt ctx);
        add_binding ctx name info
      | Some (remaining, group) ->
        if n.index >= ctx.count then
          error_f "enter: %s is not part of current group, \
                   missing bindings in group:\n%s"
            (dump_name name)
            (String.concat "; " (undefined_bindings ctx));
        let offset = ctx.count - 1 - n.index in
        let seq =
          Dbseq.update ctx.seq offset (fun binding ->
            match binding with
            | Binding {name=name'; _} ->
              if not (same_name name name') then
                wrong_context "enter (in group)" name (Some binding)
              else
                error_f "enter (in group): binding %s is already defined"
                  (dump_name name)
            | Undefined name' ->
              if not (same_name name name') then
                wrong_context "enter (in group)" name (Some binding);
              Binding {name; info}
          )
        in
        let group =
          if remaining = 1
          then None
          else Some (remaining - 1, group)
        in
        {count = ctx.count; seq; group}

    let retract_and_enter t name info =
      if Option.is_some t.group then
        incomplete_group "retract_and_enter: cannot retract" t;
      enter (internal_retract "retract_and_enter" t name) name info

    let lookup t (type a) (Name n as name : a name) : a info =
      if Option.is_some t.group then
        incomplete_group "lookup: cannot lookup binding" t;
      let offset = t.count - 1 - n.index in
      if offset < 0 then
        error_f "lookup: name %s is not bound in context %s"
          (dump_name name) (dump_binding_suffix (last_binding_opt t));
      match Dbseq.get offset t.seq with
      | exception Not_found -> assert false
      | Undefined _ -> assert false
      | Binding {name = (Name n' as name'); info} ->
        if not (same_name name name') then (
          let names =
            match last_binding t with
            | Undefined _ -> assert false
            | Binding {name=last_name; _} -> dump_n_names 10 last_name []
          in
          let names =
            if offset <= 10 then names else dump_n_names 10 name' ("..." :: names)
          in
          let names = if t.count <= 10 then names else "..." :: names in
          error_f "lookup: looking up name %s in invalid context [%s], expecting %s"
            (dump_name name)
            (String.concat "; " names)
            (dump_name_suffix (Some name))
        );
        match Namespace.order n'.namespace n.namespace with
        | Lt | Gt ->
          invalid_arg "Context.Make: \
                       Namespace.order does not implement a total ordering"
        | Eq -> info

    let update t (type a) (Name n as name : a name) (info : a info) : t =
      let offset = t.count - 1 - n.index in
      if offset < 0 then
        error_f "update: %s is not bound in context %s"
          (dump_name name) (dump_binding_suffix (last_binding_opt t));
      if Option.is_some t.group then
        incomplete_group "update: cannot update binding" t;
      let update = function
        | Binding b as binding ->
          if not (same_name name b.name) then
            wrong_context "update" name (Some binding);
          Binding {name; info}
        | Undefined _ ->
          assert false
      in
      match Dbseq.update t.seq offset update with
      | seq -> {t with seq}
      | exception Not_found -> assert false

    let group_empty t =
      if Option.is_some t.group then
        incomplete_group "group_empty: cannot start new group" t;
      match last_binding_opt t with
      | None -> Group {prev = None; last = None; size = 0}
      | Some (Undefined _) -> assert false
      | Some (Binding {name; _}) ->
        Group {prev = Some name; last = Some name; size = 0}

    let group_fresh (Group {prev; last; size}) namespace text =
      let binder = match last with
        | None -> Name {namespace; index = 0; text; prev = None}
        | Some (Name {index; _}) as prev ->
          Name {namespace; index = index + 1; text; prev}
      in
      (Group {prev; last = Some binder; size = size + 1}, binder)
  end
end
