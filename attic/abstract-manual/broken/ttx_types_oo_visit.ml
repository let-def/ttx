open Ttx_def
open Ttx_types

module Visitor : sig
  include module type of struct include Visitor end

  type 'a binding =
    | Value       : value_desc -> ns_value binding
    | Type        : type_decl -> ns_type binding
    | Type_level  : type_level -> ns_type_level binding
    | Module      : module_decl -> ns_module binding
    | Import      : string * Digest.t -> ns_module binding
    | Module_type : module_type_decl -> ns_module_type binding

  val namespace : 'a binding -> 'a namespace

  type ('a, 'b) enter_category =
    | Enter_type_scheme : (unit, type_scheme) enter_category
    | Enter_constructor : (unit, constructor) enter_category
    | Enter_label : (unit, label) enter_category
    | Enter_type_decl : (unit, type_decl) enter_category
    | Enter_functor_parameter : (functor_parameter, module_type) enter_category
    | Enter_signature_items : (unit, signature_item list) enter_category

  class virtual ['env] bind_iter : object
    inherit iter
    method virtual enter: 'a 'b. ('a, 'b) enter_category -> 'a -> 'b -> 'env -> 'env
  end

end = struct
  include Visitor

  type 'a binding =
    | Value       : value_desc -> ns_value binding
    | Type        : type_decl -> ns_type binding
    | Type_level  : type_level -> ns_type_level binding
    | Module      : module_decl -> ns_module binding
    | Import      : string * Digest.t -> ns_module binding
    | Module_type : module_type_decl -> ns_module_type binding

  let namespace (type a) : a binding -> a namespace = function
    | Value  _ -> Value
    | Type   _ -> Type
    | Module _ -> Module
    | Import _ -> Module
    | Type_level _ -> Type_level
    | Module_type _ -> Module_type

  type ('a, 'b) enter_category =
    | Enter_type_scheme : (unit, type_scheme) enter_category
    | Enter_constructor : (unit, constructor) enter_category
    | Enter_label : (unit, label) enter_category
    | Enter_type_decl : (unit, type_decl) enter_category
    | Enter_functor_parameter : (functor_parameter, module_type) enter_category
    | Enter_signature_items : (unit, signature_item list) enter_category

  type 'env bind = { bind: 'a. 'a binding -> 'env -> 'env }
  let enter (type a b env) (bind : env bind)
      (category : (a, b) enter_category) (param: a) (value : b) (env : env) : env =
    let bind d e = bind.bind d e in
    match category with
    | Enter_type_scheme ->
      bind (Type_level (Type_scheme.forall value)) env
    | Enter_constructor ->
      bind (Type_level (Constructor.forall value)) env
    | Enter_label ->
      bind (Type_level (Label.forall value)) env
    | Enter_type_decl ->
      bind (Type_level (Type_decl.forall value)) env
    | Enter_functor_parameter ->
      begin match Functor_parameter.desc param with
        | Unit -> env
        | Anonymous _ -> env
        | Named md -> bind (Module md) env
      end
    | Enter_signature_items ->
      begin match value with
        | [] -> env
        | x :: _ ->
          match Signature_item.desc x with
          | Value v -> bind (Value v) env
          | Type (_, bs) ->
            List.fold_left (fun env t -> bind (Type t) env) env bs
          | Module (_, bs) ->
            List.fold_left (fun env m -> bind (Module m) env) env bs
          | Module_type mt ->
            bind (Module_type mt) env
      end

  class virtual ['env] bind_iter = object(self : 'self)
    inherit iter as parent
    method virtual bind: 'a. 'a binding -> 'self

    method! iter (type k) (k : k category) (v : k) : unit =
      match k with
      | Type_scheme ->
        self#enter Enter_type_scheme () v
      | Constructor ->
        self#enter Enter_constructor () v
      | Label ->
        self#enter Enter_label () v
      | Type_decl ->
        self#enter Enter_type_decl () v
      | Module_type ->
        begin match Module_type.desc v with
          | Functor (fp, mt) ->
            parent#iter Functor_parameter fp;
            self#enter Enter_functor_parameter fp mt
          | _ -> parent#iter Module_type v
        end
      | _ -> parent#iter k v

    method enter: 'a 'b. ('a, 'b) enter_category -> 'a -> 'b -> unit =
      fun (type a b) (category : (a, b) enter_category) (param : a) (value : b) : unit ->
      match category with
      | Enter_type_scheme ->
        self#bind (Type_level (Type_scheme.forall value))
      | Enter_constructor ->
        self#bind (Type_level (Constructor.forall value))
      | Enter_label ->
        self#bind (Type_level (Label.forall value))
      | Enter_type_decl ->
        self#bind (Type_level (Type_decl.forall value))
      | Enter_functor_parameter ->
        begin match Functor_parameter.desc param with
          | Unit -> env
          | Anonymous _ -> env
          | Named md -> bind (Module md) env
        end
      | Enter_signature_items ->
        begin match value with
          | [] -> env
          | x :: _ ->
            match Signature_item.desc x with
            | Value v -> bind (Value v) env
            | Type (_, bs) ->
              List.fold_left (fun env t -> bind (Type t) env) env bs
            | Module (_, bs) ->
              List.fold_left (fun env m -> bind (Module m) env) env bs
            | Module_type mt ->
              bind (Module_type mt) env
        end
  end


  (*: type a. a bind_iter bind_iter = {
    syntax = begin fun (type b) (self : a bind_iter) (k : b category) (a : b) ->
    match k with
    | _ -> iter (self, {iter=bind_iter.syntax}) k a
    end;
    }*)
end
