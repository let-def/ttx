[@@@ocaml.warning "-30"]

type typ =
  | T of string
  | A of typ list * string

type decl = string * decl_desc

and decl_desc =
  | Decl of field list
  | Custom of [`Intf_header|`Intf_footer|`Impl_header|`Impl_footer|`Visit] list

and field = string * field_desc

and field_desc =
  | Value of typ
  | Record of record_field list
  | Variant of variant_case list
  | Custom of [`Intf|`Intf_make|`Impl_header|`Impl_type|`Impl_make|`Accessor] list

and record_field = (string * typ)

and variant_case = string * variant_arg

and variant_arg =
  | Tuple of typ list
  | Record of record_field list

let indent = Indent.shift
let indented n f oc = f (indent n oc)
let printf = Indent.printf
let sprintf = Printf.sprintf

let enter_val oc =
  let in_val = ref false in
  fun in_val' ->
    if in_val' && not !in_val then (
      printf oc "\n";
      in_val := true;
    );
    in_val := in_val'

let gen_typ map_typ t =
  let rec aux = function
    | T name -> map_typ name
    | A ([], name) -> name
    | A ([t], name) -> aux t ^ " " ^ name
    | A (ts, name) ->
      "(" ^ String.concat ", " (List.map aux ts) ^ ") " ^ name
  in
  aux t

let get_dcustoms lbl fields =
  List.filter_map (function
      | (dname, Custom lbls : decl) when List.mem lbl lbls ->
        Some dname
      | _ -> None
    ) fields

let get_customs lbl fields =
  List.filter_map (function
      | (dname, Custom lbls : field) when List.mem lbl lbls ->
        Some dname
      | _ -> None
    ) fields

let gen_record oc (prefix, map_typ, fname, fields) =
  printf oc "%s %s = {\n" prefix fname;
  let oc' = indent 2 oc in
  List.iter
    (fun (lname, ltyp) -> printf oc' "%s: %s;\n" lname (gen_typ map_typ ltyp))
    fields;
  printf oc "}\n"

let gen_variant oc (prefix, map_typ, fname, cases) =
  printf oc "%s %s =\n" prefix fname;
  let oc = indent 2 oc in
  List.iter (fun (cname, cdesc) ->
      match cdesc with
      | Tuple [] -> printf oc "| %s\n" cname
      | Tuple ts ->
        printf oc "| %s of %s\n" cname
          (String.concat " * " (List.map (gen_typ map_typ) ts))
      | Record fs ->
        printf oc "| %s of {\n" cname;
        List.iter (fun (lname, ltyp) ->
            printf (indent 2 oc) "%s: %s;\n" lname (gen_typ map_typ ltyp))
          fs;
        printf oc "}\n"
    ) cases

let gen_sig map_typ dname ~abstract_type fields oc =
  let gen_typ = gen_typ map_typ in
  if abstract_type
  then printf oc "type t\n"
  else printf oc "type t = %s\n" dname;
  let enter_val = enter_val oc in
  List.iter (fun (fname, fdesc) ->
      match fdesc with
      | Value t ->
        enter_val true;
        printf oc "val %s : t -> %s\n" fname (gen_typ t)
      | Record fields ->
        enter_val false;
        printf oc "\n%a" gen_record ("type", map_typ, fname, fields)
      | Variant cases ->
        enter_val false;
        printf oc "\n%a" gen_variant ("type", map_typ, fname, cases)
      | Custom ls ->
        enter_val false;
        if List.mem `Intf ls then
          printf oc "\n%s\n" fname
    ) fields;
  let params =
    List.filter_map (fun (_fname, fdesc) ->
        match fdesc with
        | Record _ | Variant _ | Custom _ -> None
        | Value t -> Some (gen_typ t)
      ) fields
  in
  enter_val true;
  begin match get_customs `Intf_make fields with
    | [] -> printf oc "val make : %s -> t\n" (String.concat " -> " params)
    | cs -> List.iter (Indent.print oc) cs
  end

let gen_struct map_typ dname fields oc =
  let map_typ name =
    if name = dname
    then "t"
    else map_typ name
  in
  let gen_typ = gen_typ map_typ in
  List.iter (Indent.print oc) (get_customs `Impl_header fields);
  begin match get_customs `Impl_type fields with
    | [] ->
      printf oc "type t = {\n";
      List.iter (fun (fname, fdesc) ->
          match fdesc with
          | Value t ->
            printf (indent 2 oc) "%s: %s;\n" fname (gen_typ t);
          | _ -> ()
        ) fields;
      printf oc "}\n"
    | customs -> List.iter (Indent.print oc) customs
  end;
  List.iter (fun (fname, fdesc) ->
      match fdesc with
      | Record fields -> gen_record oc ("and", map_typ, fname, fields)
      | Variant cases -> gen_variant oc ("and", map_typ, fname, cases)
      | _ -> ()
    ) fields;
  printf oc "\n";
  let has_custom_accessor name =
    List.exists (function
        | (k, Custom lbls : field) -> k = name && List.mem `Accessor lbls
        | _ -> false
      ) fields
  in
  List.iter (fun (fname, fdesc) ->
      match fdesc with
      | Value _ when not (has_custom_accessor fname) ->
        printf oc "let %s (t : t) = t.%s\n" fname fname
      | _ -> ()
    ) fields;
  let params =
    List.filter_map (fun (fname, fdesc) ->
        match fdesc with
        | Record _ | Variant _ | Custom _ -> None
        | Value _ -> Some fname
      ) fields
  in
  begin match get_customs `Impl_make fields with
    | [] ->
      printf oc "let make %s = {%s}\n"
        (String.concat " " params)
        (String.concat "; " params)
    | cs -> List.iter (Indent.print oc) cs
  end

let gen_intf oc (decls : decl list) =
  List.iter (function
      | (dname, Decl _) -> printf oc "type %s\n" dname
      | (_, Custom _) -> ()
    ) decls;
  List.iter (printf oc "%s\n") (get_dcustoms `Intf_header decls);
  List.iter (function
      | (dname, Decl fields) ->
        printf oc "\n";
        printf oc "module %s : sig\n%tend\n"
          (String.capitalize_ascii dname)
          (indented 2 @@ gen_sig (fun x -> x) dname ~abstract_type:false fields);
      | (_, Custom _) -> ()
    ) decls;
  List.iter (printf oc "%s\n") (get_dcustoms `Intf_footer decls)

let gen_impl oc (decls : decl list) =
  List.iter (printf oc "%s\n") (get_dcustoms `Impl_header decls);
  let typ_table = Hashtbl.create 7 in
  List.iter (fun (dname, _) ->
      Hashtbl.add typ_table dname (String.capitalize_ascii dname ^ ".t")
    ) decls;
  let map_typ name = try Hashtbl.find typ_table name with Not_found -> name in
  let first_decl = let f = ref true in fun () -> let r = !f in f := false; r in
  List.iter (function
      | (dname, Decl fields) ->
        printf oc "%s %s : sig\n%tend = struct\n%tend\n\n"
          (if first_decl () then "module rec" else "and")
          (String.capitalize_ascii dname)
          (indented 2 @@ gen_sig map_typ dname ~abstract_type:true fields)
          (indented 2 @@ gen_struct map_typ dname fields);
      | (_, Custom _) -> ()
    ) decls;
  List.iter (function
      | (dname, Decl _) ->
        printf oc "type %s = %s.t\n" dname (String.capitalize_ascii dname)
      | (_, Custom _) -> ()
    ) decls;
  List.iter (printf oc "%s\n") (get_dcustoms `Impl_footer decls)

module StringMap = Map.Make(String)

let gen_visitor_type_def oc (decls : decl list) =
  printf oc "type 'a category =\n";
  let print_cat name =
    printf (indent 2 oc) "| %s : %s category\n"
      (String.capitalize_ascii name) name
  in
  List.iter (function
      | (name, Decl _) ->
        print_cat name
      | (name, Custom lbls) ->
        if List.mem `Visit lbls then
          print_cat name
    ) decls;
  printf oc
    "type 'a iter = {iter: 'b. 'a iter -> 'a -> 'b category -> 'b -> unit} \
     [@@ocaml.unboxed]\n"

let gen_visitor_intf oc (decls : decl list) =
  printf oc "module Visitor : sig\n";
  gen_visitor_type_def (indent 2 oc) decls;
  printf (indent 2 oc) "val iter: 'a iter\n";
  printf oc "end\n"

let gen_visitor_impl oc (decls : decl list) =
  let in_recursion =
    List.fold_left
      (fun acc (name, decl) ->
         let add = match decl with
           | Custom lbls -> List.mem `Visit lbls
           | Decl _ -> true
         in
         if add
         then StringMap.add name (`Visit (String.capitalize_ascii name)) acc
         else acc
      )
      StringMap.empty decls
  in
  printf oc "module Visitor = struct\n";
  let oc' = indent 2 oc in
  gen_visitor_type_def oc' decls;
  printf oc' "\n";
  printf oc'
    "let iter = {\n\
    \  iter = begin fun (type a k) (self : a iter) a (k : k category) (v : k) : unit ->\n\
    \  match k with\n";
  let self = "self.iter self a" in
  List.iter (function
      | (_, Custom _) -> ()
      | (name, Decl fields) ->
        let oc = indent 2 oc' in
        printf oc "| %s ->\n" (String.capitalize_ascii name);
        let mod_name = String.capitalize_ascii name in
        let in_recursion =
          List.fold_left (fun acc -> function
              | (name, Record fields) -> StringMap.add name (`Record fields) acc
              | (name, Variant cases) -> StringMap.add name (`Variant cases) acc
              | (_, (Value _ | Custom _)) -> acc
            )
            in_recursion fields
        in
        let rec sub_visit oc value = function
          | A([T t], ("list" | "option" | "vector" as container)) ->
             printf oc "%s.iter (fun x ->\n" (String.capitalize_ascii container);
             sub_visit (indent 2 oc) "x" (T t);
             printf oc ") %s;\n" value;
          | T(t) ->
            begin match StringMap.find_opt t in_recursion with
              | Some (`Visit k) ->
                printf oc "%s %s %s;\n" self k value
              | Some (`Record fields) ->
                printf oc "let _x = %s in\n" value;
                List.iter
                  (fun (nf, t) -> sub_visit oc (sprintf "_x.%s" nf) t)
                  fields
              | Some (`Variant cases) ->
                printf oc "begin match %s with\n" value;
                let oc' = indent 2 oc in
                List.iter (fun (nf, arg : variant_case) ->
                    match arg with
                    | Tuple [] ->
                      printf oc' "| %s -> ()\n" nf;
                    | Tuple ts ->
                      printf oc' "| %s (%s) ->\n" nf
                        (String.concat ", " (List.mapi (fun i _ -> sprintf "_x_%d" i) ts));
                      List.iteri
                        (fun i t -> sub_visit (indent 2 oc') (sprintf "_x_%d" i) t)
                        ts;
                      printf oc' "  ()\n"
                    | Record fields ->
                      printf oc' "| %s _r ->\n" nf;
                      List.iter
                        (fun (name, t) -> sub_visit (indent 2 oc') (sprintf "_r.%s" name) t)
                        fields
                  ) cases;
                printf oc "end;\n"
              | None -> ()
            end
          | typ ->
            prerr_endline ("TODO: Unsupported type " ^ gen_typ (fun x -> x) typ);
            assert false
        in
        List.iter (fun (name, desc) ->
            match desc with
            | Value t -> sub_visit (indent 2 oc) (sprintf "(%s.%s v)" mod_name name) t
            | _ -> ()
          ) fields;
    ) decls;
  printf oc "    end;\n";
  printf oc "  }\n";
  printf oc "end\n"
