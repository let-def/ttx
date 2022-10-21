let string n str =
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

type output = {
  out: string -> int -> int -> unit;
  indent: int;
  at_eol: bool ref;
}

let make out =
  {out; indent = 0; at_eol = ref false}

let shift n o =
  {o with indent = o.indent + n}

let spaces = String.make 31 ' '

let rec print_indent o n =
  if n < 31 then
    o.out spaces 0 n
  else (
    o.out spaces 0 31;
    print_indent o (n - 31)
  )

let print_substring o str pos len =
  if len <> 0 then (
    if !(o.at_eol) then (
      o.at_eol := false;
      if str.[pos] <> '\n' then
        print_indent o o.indent
    );
    o.out str pos len;
  )

let chars = String.init 256 Char.chr

let print o str =
  match String.index_opt str '\n' with
  | None ->
    print_substring o str 0 (String.length str)
  | Some i ->
    print_substring o str 0 (i + 1);
    let rec loop i =
      o.at_eol := true;
      match String.index_from_opt str i '\n' with
      | None ->
        print_substring o str i (String.length str - i)
      | Some i' ->
        print_substring o str i (i' - i + 1);
        loop (i' + 1)
    in
    loop (i + 1)

let rec print_acc o : _ CamlinternalFormat.acc -> unit = function
  | Acc_formatting_lit (p, fmting_lit) ->
    let s = CamlinternalFormat.string_of_formatting_lit fmting_lit in
    print_acc o p; print o s;
  | Acc_formatting_gen (p, Acc_open_tag acc') ->
    print_acc o p; print o "@{"; print_acc o acc';
  | Acc_formatting_gen (p, Acc_open_box acc') ->
    print_acc o p; print o "@["; print_acc o acc';
  | Acc_string_literal (p, s)
  | Acc_data_string (p, s) -> print_acc o p; print o s
  | Acc_char_literal (p, c)
  | Acc_data_char (p, c) -> print_acc o p;
    if c = '\n' then
      print o "\n"
    else
      print_substring o chars (Char.code c) 1
  | Acc_delay (p, f)         -> print_acc o p; f o
  | Acc_flush p              -> print_acc o p;
  | Acc_invalid_arg (p, msg) -> print_acc o p; invalid_arg msg;
  | End_of_acc               -> ()

let printf o (CamlinternalFormatBasics.Format (fmt, _)) =
  CamlinternalFormat.make_printf (fun acc -> print_acc o acc) End_of_acc fmt
