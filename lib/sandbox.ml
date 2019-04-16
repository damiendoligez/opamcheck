(* sandbox.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

let get_opam_version () : [`Opam1 | `Opam2] =
  try
    let cin = Unix.open_process_in "opam --version" in
    let c = input_char cin in
    ignore (Unix.close_process_in cin);
    match c with
    | '1' -> `Opam1
    | '2' -> `Opam2
    | _ -> Log.fatal "Unsupported opam version"
  with _ -> Log.fatal "Cannot get opam version"

let run ?(env="") cmd =
  Log.log "# %s\n" cmd;
  Sys.command (env ^ cmd)

let run0 ?(retry=0) ?(env="") cmd =
  let rec loop i =
    let res = run ~env cmd in
    if res <> 0 then
      if i <= 0 then
        failwith (sprintf "command failed with result %d: %s%s" res env cmd)
      else
        loop (i-1)
  in
  loop retry

type result = OK | Failed of (string * string) list

(** encoding must be done in the right order: "..x" before ".x" *)
let encode_compare s1 s2 =
  let rec loop i =
    if i >= String.length s1 then
      if i >= String.length s2 then 0 else -1
    else if i >= String.length s2 then 1
    else if s1.[i] = s2.[i] then loop (i + 1)
    else Pervasives.compare s1.[i] s2.[i]
  in
  loop 0

(** rename all ".xxx" to "..xxx" *)
let rec encode dir =
  let f x =
    let xx = Filename.concat dir x in
    Unix.(match (lstat xx).st_kind with
    | S_REG | S_LNK -> ()
    | S_DIR -> encode xx
    | exception Unix_error _ -> ()
    | _ -> ()
    );
    if x.[0] = '.' then begin
      try Sys.rename xx (Filename.concat dir ("." ^ x))
      with Unix.Unix_error _ -> ()
    end;
  in
  let entries = Sys.readdir dir in
  Array.sort encode_compare entries;
  Array.iter f entries

(* decoding must be done in the reverse order of encoding *)
let decode_compare s1 s2 = encode_compare s2 s1

(** remove all ".[^.]xxx" and rename all "..xxx" to ".xxx" *)
let rec decode dir =
  let f x =
    let xx = Filename.concat dir x in
    if x.[0] = '.' && x.[1] <> '.' then begin
      run0 (sprintf "/bin/rm -rf %s" xx)
    end else begin
      Unix.(match (lstat xx).st_kind with
      | S_REG | S_LNK -> ()
      | S_DIR -> decode xx
      | exception Unix_error _ -> ()
      | _ -> ()
      );
      if x.[0] = '.' then begin
        let newname = String.sub x 1 (String.length x - 1) in
        try Sys.rename xx (Filename.concat dir newname)
        with Unix.Unix_error _ -> ()
      end
    end
  in
  let entries = Sys.readdir dir in
  Array.sort decode_compare entries;
  Array.iter f entries

let get_tag l =
  let f (n, v) = sprintf " %s.%s" n v in
  let packs = String.concat "" (List.map f l) in
  ("st_" ^ Digest.to_hex (Digest.string packs), packs)

let rec parse_failure_file ic acc =
  match Version.split_name_version (input_line ic) with
  | (name, Some vers) -> parse_failure_file ic ((name, vers) :: acc)
  | (_name, None) -> assert false
  | exception End_of_file -> List.rev acc

let read_result file =
  let ic = open_in file in
  let res =
    match input_line ic with
    | "Failure" -> Failed (parse_failure_file ic [])
    | "Success" -> OK
    | _ -> Failed []
    | exception _ -> Failed []
  in
  close_in ic;
  res

let write_failure file l =
  let oc = open_out file in
  fprintf oc "Failure\n";
  List.iter (fun (p, v) -> fprintf oc "%s.%s\n" p v) l;
  close_out oc

let write_success file =
  let oc = open_out file in
  fprintf oc "Success\n";
  close_out oc

let save ~gitdir ~opamroot ~result_file l =
  let status = match read_result result_file with OK -> "ok" | _ -> "failed" in
  encode opamroot;
  let (tag, list) = get_tag l in
  run0 ~retry:3 (sprintf "git -C %s checkout -b %s" gitdir tag);
  run0 ~retry:3 (sprintf "git -C %s add -A" gitdir);
  run0 ~retry:3 (sprintf "git -C %s commit --allow-empty -m '(%s) %s [%s ]'"
                         gitdir status tag list);
  decode opamroot

let restore ~gitdir ~opamroot l =
  let (tag, _) = get_tag l in
  run0 (sprintf "git -C %s checkout -f %s" gitdir tag);
  run0 (sprintf "git -C %s clean -d -f -x" gitdir);
  decode opamroot

let restore_result ~gitdir l =
  let (tag, _) = get_tag l in
  let rc =
    run (sprintf "git -C %s checkout -f %s -- opamcheck-result" gitdir tag)
  in
  rc = 0

let print_command_to_log log_file cmd =
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o666 log_file in
  fprintf oc "\n================\n$ %s\n\n" cmd;
  close_out oc

let play_solution ~sandbox rl =
  let compvers = snd (List.hd (List.rev rl)) in
  let total = List.length rl in
  let bin = Filename.concat sandbox "bin" in
  let path = sprintf "%s:%s" bin (Sys.getenv "PATH") in
  let fetch = "fetch %{checksum}% %{url}% %{out}%" in
  let opamstatedir = Filename.concat sandbox "opamstate" in
  let gitdir = Filename.concat opamstatedir compvers in
  let opamroot = Filename.concat gitdir "dotopam" in
  let repo = Filename.concat sandbox "opam-repository" in
  let result_file = Filename.concat gitdir "opamcheck-result" in
  let log_file = Filename.concat gitdir "opamcheck-log" in
  let opam_env =
    sprintf "export PATH='%s' OPAMFETCH='%s' OPAMROOT='%s' OPAMNO=true \
             OPAMCOLOR=never OPAMUTF8=never OPAMUTF8MSGS=false \
             OPAMVERBOSE=1; eval $(opam config env); "
      path fetch opamroot in

  let rec find_start l acc =
    Status.(
      cur.step <- Install { stored = true; cur = List.length l; total;
                            cur_pack = "" };
    );
    if restore_result ~gitdir l then begin
      Status.show ~sandbox ();
      Some (acc, l)
    end else begin
      match l with
      | [] ->
          Status.show ~sandbox ();
          None
      | h :: t -> find_start t (h :: acc)
    end
  in
  let rec play l acc =
    match read_result result_file with
    | Failed l -> Failed l
    | OK ->
       begin match l with
       | [] -> OK
       | (pack, vers) :: t ->
         let packvers = sprintf "%s.%s" pack vers in
          Status.(
            let count = List.length acc in
            cur.step <- Install { stored = false; total; cur = count;
                                  cur_pack = packvers };
            show ~sandbox ();
          );
          let cmd =
            if pack = "ocaml" then
              "true"
            else
              sprintf "opam install %s" packvers
          in
          let packs_done = ((pack, vers) :: acc) in
          print_command_to_log log_file cmd;
          let rc = run ~env:opam_env (sprintf "%s > %s 2>&1" cmd log_file) in
          if rc <> 0 then begin
            Status.show_result '#';
            write_failure result_file packs_done;
          end else begin
            Status.show_result '+';
            write_success result_file;
          end;
          save ~gitdir ~opamroot ~result_file packs_done;
          play t packs_done
       end
  in
  match find_start rl [] with
  | None ->
    let opam_version = get_opam_version () in
     run0 (sprintf "/bin/rm -rf %s" gitdir);
     run0 (sprintf "/bin/mkdir -p %s" opamroot);
     begin match opam_version with
       | `Opam1 ->
         run0 ~env:opam_env
           (sprintf "opam init --comp=%s --no-setup default %s" compvers repo)
       | `Opam2 ->
         run0 ~env:opam_env
           (sprintf "opam init --disable-sandboxing --compiler=%s --no-setup -y default %s" compvers repo)
     end;
     run0 (sprintf "git -C %s init" gitdir);
     run0 (sprintf "echo '!*' >%s" (Filename.concat gitdir ".gitignore"));
     write_success result_file;
     save ~gitdir ~opamroot ~result_file [];
     play (List.rev rl) []
  | Some (todo, cached) ->
     begin match read_result result_file with
     | Failed l -> Status.show_result '#'; Failed l
     | OK when todo = [] -> OK
     | OK ->
       restore ~gitdir ~opamroot cached;
       play todo cached;
     end
