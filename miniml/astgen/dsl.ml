[@@@ocaml.warning "-30"]

type typ =
  | T of string
  | A of typ list * string

type decl = string * decl_desc

and decl_desc = Decl of field list

and field = string * field_desc

and field_desc =
  | Value of typ
  | Record of record_field list
  | Variant of variant_case list
  | Custom of [`Intf|`Impl] list

and record_field = (string * typ)

and variant_case = string * variant_arg

and variant_arg =
  | Tuple of typ list
  | Record of record_field list

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

let rec gen_typ = function
  | T name -> name
  | A ([], name) -> name
  | A ([t], name) -> gen_typ t ^ " " ^ name
  | A (ts, name) ->
    "(" ^ String.concat ", " (List.map gen_typ ts) ^ ") " ^ name

let gen_intf (decls : decl list) =
  let printf = Printf.printf in
  List.iter (fun (dname, _) -> printf "type %s\n" dname) decls;
  List.iter (fun (dname, Decl fields) ->
      printf "\n";
      printf "module %s : sig\n" (String.capitalize_ascii dname);
      printf "  type t = %s\n" dname;
      let in_val = ref false in
      let enter_val () =
        if not !in_val then (
          printf "\n";
          in_val := true;
        )
      in
      List.iter (fun (fname, fdesc) ->
          match fdesc with
          | Value t ->
            enter_val ();
            printf "  val %s : t -> %s\n" fname (gen_typ t)
          | Record fields ->
            in_val := false;
            printf "\n";
            printf "  type %s = {\n" fname;
            List.iter (fun (lname, ltyp) ->
                printf "    %s: %s;\n" lname (gen_typ ltyp))
              fields;
            printf "  }\n";
          | Variant args ->
            in_val := false;
            printf "\n";
            printf "  type %s =\n" fname;
            List.iter (fun (cname, cdesc) ->
                match cdesc with
                | Tuple [] -> printf "    | %s\n" cname
                | Tuple ts ->
                  printf "    | %s of %s\n" cname
                    (String.concat " * " (List.map gen_typ ts))
                | Record fs ->
                  printf "    | %s of {\n" cname;
                  List.iter (fun (lname, ltyp) ->
                      printf "      %s: %s;\n" lname (gen_typ ltyp))
                    fs;
                  printf "      }\n"
              ) args
          | Custom ls ->
            in_val := false;
            if List.mem `Intf ls then
              printf "\n  %s\n" (wrap_indent 2 fname)
        ) fields;
      let params =
        List.filter_map (fun (_fname, fdesc) ->
            match fdesc with
            | Record _ | Variant _ | Custom _ -> None
            | Value t -> Some (gen_typ t)
          ) fields
      in
      enter_val ();
      printf "  val make : %s -> t\n" (String.concat " -> " params);
      printf "end\n"
    ) decls
