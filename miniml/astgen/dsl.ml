[@@@ocaml.warning "-30"]

type typ =
  | T of string
  | A of typ list * string

type decl = string * decl_desc

and decl_desc =
  | Decl of field list
  | Custom of [`Intf_header|`Intf_footer|`Impl_header|`Impl_footer] list

and field = string * field_desc

and field_desc =
  | Value of typ
  | Record of record_field list
  | Variant of variant_case list
  | Custom of [`Intf|`Intf_make|`Impl_header|`Impl_type|`Impl_make] list

and record_field = (string * typ)

and variant_case = string * variant_arg

and variant_arg =
  | Tuple of typ list
  | Record of record_field list

let enter_val oc =
  let in_val = ref false in
  fun in_val' ->
    if in_val' && not !in_val then (
      Printf.fprintf oc "\n";
      in_val := true;
    );
    in_val := in_val'

let wrap_indent n str =
  match String.index_opt str '\n' with
  | None -> str
  | Some i ->
    let b = Buffer.create (String.length str + n * 8) in
    let n = String.make n ' ' in
    Buffer.add_substring b str 0 (i + 1);
    let rec loop i =
      Buffer.add_string b n;
      match String.index_from_opt str i '\n' with
      | None ->
        Buffer.add_substring b str i (String.length str - i)
      | Some i' ->
        Buffer.add_substring b str i (i' - i + 1);
        loop (i' + 1)
    in
    loop (i + 1);
    Buffer.contents b

let fprintf = Printf.fprintf

let gen_typ map_typ t =
  let rec aux = function
    | T name -> map_typ name
    | A ([], name) -> name
    | A ([t], name) -> aux t ^ " " ^ name
    | A (ts, name) ->
      "(" ^ String.concat ", " (List.map aux ts) ^ ") " ^ name
  in
  aux t

let print_custom n oc txt =
  fprintf oc "  %s\n" (wrap_indent n txt)

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

let gen_record prefix map_typ fname fields oc =
  fprintf oc "  %s %s = {\n" prefix fname;
  List.iter (fun (lname, ltyp) ->
      fprintf oc "    %s: %s;\n" lname (gen_typ map_typ ltyp))
    fields;
  fprintf oc "  }\n"

let gen_variant prefix map_typ fname cases oc =
  fprintf oc "  %s %s =\n" prefix fname;
  List.iter (fun (cname, cdesc) ->
      match cdesc with
      | Tuple [] -> fprintf oc "    | %s\n" cname
      | Tuple ts ->
        fprintf oc "    | %s of %s\n" cname
          (String.concat " * " (List.map (gen_typ map_typ) ts))
      | Record fs ->
        fprintf oc "    | %s of {\n" cname;
        List.iter (fun (lname, ltyp) ->
            fprintf oc "      %s: %s;\n" lname (gen_typ map_typ ltyp))
          fs;
        fprintf oc "      }\n"
    ) cases

let gen_sig map_typ dname ~abstract_type fields oc =
  let gen_typ = gen_typ map_typ in
  if abstract_type
  then fprintf oc "  type t\n"
  else fprintf oc "  type t = %s\n" dname;
  let enter_val = enter_val oc in
  List.iter (fun (fname, fdesc) ->
      match fdesc with
      | Value t ->
        enter_val true;
        fprintf oc "  val %s : t -> %s\n" fname (gen_typ t)
      | Record fields ->
        enter_val false;
        fprintf oc "\n%t" (gen_record "type" map_typ fname fields)
      | Variant cases ->
        enter_val false;
        fprintf oc "\n%t" (gen_variant "type" map_typ fname cases)
      | Custom ls ->
        enter_val false;
        if List.mem `Intf ls then
          fprintf oc "\n  %s\n" (wrap_indent 2 fname)
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
    | [] -> fprintf oc "  val make : %s -> t\n" (String.concat " -> " params)
    | cs -> List.iter (print_custom 2 oc) cs
  end

let gen_struct map_typ dname fields oc =
  let map_typ name =
    if name = dname
    then "t"
    else map_typ name
  in
  let gen_typ = gen_typ map_typ in
  List.iter (print_custom 2 oc) (get_customs `Impl_header fields);
  begin match get_customs `Impl_type fields with
    | [] ->
      fprintf oc "  type t = {\n";
      List.iter (fun (fname, fdesc) ->
          match fdesc with
          | Value t ->
            fprintf oc "    %s: %s;\n" fname (gen_typ t);
          | _ -> ()
        ) fields;
      fprintf oc "  }\n"
    | customs -> List.iter (print_custom 2 oc) customs
  end;
  List.iter (fun (fname, fdesc) ->
      match fdesc with
      | Record fields -> gen_record "and" map_typ fname fields oc
      | Variant cases -> gen_variant "and" map_typ fname cases oc
      | _ -> ()
    ) fields;
  fprintf oc "\n";
  List.iter (fun (fname, fdesc) ->
      match fdesc with
      | Value _ -> fprintf oc "  let %s (t : t) = t.%s\n" fname fname
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
      fprintf oc "  let make %s = {%s}\n"
        (String.concat " " params)
        (String.concat "; " params)
    | cs -> List.iter (print_custom 2 oc) cs
  end

let gen_intf oc (decls : decl list) =
  List.iter (fun (dname, _) -> fprintf oc "type %s\n" dname) decls;
  List.iter (fprintf oc "%s\n") (get_dcustoms `Intf_header decls);
  List.iter (function
      | (dname, Decl fields) ->
        fprintf oc "\n";
        fprintf oc "module %s : sig\n%tend\n"
          (String.capitalize_ascii dname)
          (gen_sig (fun x -> x) dname ~abstract_type:false fields);
      | (_, Custom _) -> ()
    ) decls;
  List.iter (fprintf oc "%s\n") (get_dcustoms `Intf_footer decls)

let gen_impl oc (decls : decl list) =
  List.iter (fprintf oc "%s\n") (get_dcustoms `Impl_header decls);
  let typ_table = Hashtbl.create 7 in
  List.iter (fun (dname, _) ->
      Hashtbl.add typ_table dname (String.capitalize_ascii dname ^ ".t")
    ) decls;
  let map_typ name = try Hashtbl.find typ_table name with Not_found -> name in
  let first_decl = let f = ref true in fun () -> let r = !f in f := false; r in
  List.iter (function
      | (dname, Decl fields) ->
        fprintf oc "%s %s : sig\n%tend = struct\n%tend\n\n"
          (if first_decl () then "module rec" else "and")
          (String.capitalize_ascii dname)
          (gen_sig map_typ dname ~abstract_type:true fields)
          (gen_struct map_typ dname fields);
      | (_, Custom _) -> ()
    ) decls;
  List.iter (function
      | (dname, Decl _) ->
        fprintf oc "type %s = %s.t\n" dname (String.capitalize_ascii dname)
      | (_, Custom _) -> ()
    ) decls;
  List.iter (fprintf oc "%s\n") (get_dcustoms `Impl_footer decls)
