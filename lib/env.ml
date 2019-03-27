(* env.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

(** get the first line of output of the command [c] *)
let get_command_output c =
  let tmp = Filename.temp_file "opamcheck" "" in
  ignore (Sys.command (Printf.sprintf "%s >%s" c tmp));
  let ic = open_in tmp in
  let res = input_line ic in
  close_in ic;
  begin try Sys.remove tmp with _ -> () end;
  res

(** Note: this is not exactly what OPAM does for exotic Unix variants *)
let get_os () =
  let s =
    if Sys.os_type = "Unix" then get_command_output "uname -s" else Sys.os_type
  in
  String.lowercase_ascii s

let compiler_to_ocaml_version s =
  match String.index s '+' with
  | n -> String.sub s 0 n
  | exception Not_found -> s

let predefined = [
  ("arch", [get_command_output "uname -m"]);
  ("false", ["false"]);
  ("ocaml-native", ["true"]);
  ("opam-version", [get_command_output "opam --version"]);
  ("os", [get_os ()]);
  ("preinstalled", ["false"]);
]

let get ocaml_versions =
  let vers =
    List.sort_uniq compare (List.map compiler_to_ocaml_version ocaml_versions)
  in
  ("ocaml", ocaml_versions)
  :: ("*ocaml-version*", vers)
  :: predefined

let is_package (name, _) =
  not (List.exists (fun (n, _) -> n = name)
         (("*ocaml-version*", []) :: predefined))
