(* sandbox.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

let sandbox = Sys.getenv "OPCSANDBOX"
let bin = Filename.concat sandbox "bin"
let tmp = Filename.concat sandbox "tmp"
let path = sprintf "%s:%s" bin (Sys.getenv "PATH")
let fetch = "fetch %{checksum}% %{url}% %{out}%"
let opamstatedir = Filename.concat sandbox "opamstate"
let gitdir v = Filename.concat opamstatedir v
let opamroot v = Filename.concat (gitdir v) "dotopam"
let repo = Filename.concat sandbox "opam-repository"
let result_file v = Filename.concat (gitdir v) "opamcheck-result"
let log_file v = Filename.concat (gitdir v) "opamcheck-log"
let opam_env v =
  sprintf "export PATH='%s' OPAMFETCH='%s' OPAMROOT='%s' OPAMNO=true \
           OPAMCOLOR=never OPAMUTF8=never OPAMUTF8MSGS=false \
           OPAMVERBOSE=1; eval $(opam config env); "
    path fetch (opamroot v)
let tmp_opam_out = Filename.concat tmp "opam_out"

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

let read_result v =
  let ic = open_in (result_file v) in
  let res =
    match input_line ic with
    | "Failure" -> Failed (parse_failure_file ic [])
    | "Success" -> OK
    | _ -> Failed []
    | exception _ -> Failed []
  in
  close_in ic;
  res

let write_failure v l =
  let oc = open_out (result_file v) in
  fprintf oc "Failure\n";
  List.iter (fun (p, v) -> fprintf oc "%s.%s\n" p v) l;
  close_out oc

let write_success v =
  let oc = open_out (result_file v) in
  fprintf oc "Success\n";
  close_out oc

let save v l =
  let dir = gitdir v in
  let root = opamroot v in
  let status = match read_result v with OK -> "ok" | _ -> "failed" in
  encode root;
  let (tag, list) = get_tag l in
  run0 ~retry:3 (sprintf "git -C %s checkout -b %s" dir tag);
  run0 ~retry:3 (sprintf "git -C %s add -A" dir);
  run0 ~retry:3 (sprintf "git -C %s commit --allow-empty -m '(%s) %s [%s ]'"
                         dir status tag list);
  decode root

let restore v l =
  let dir = gitdir v in
  let root = opamroot v in
  let (tag, _) = get_tag l in
  run0 (sprintf "git -C %s checkout -f %s" dir tag);
  run0 (sprintf "git -C %s clean -d -f -x" dir);
  decode root

let restore_result v l =
  let (tag, _) = get_tag l in
  let rc =
    run (sprintf "git -C %s checkout -f %s -- opamcheck-result" (gitdir v) tag)
  in
  rc = 0

let print_command_to_log v cmd =
  let oc =
    open_out_gen [Open_creat; Open_append; Open_text] 0o666 (log_file v)
  in
  fprintf oc "\n================\n$ %s\n\n" cmd;
  close_out oc

let play_solution rl =
  let compvers = snd (List.hd (List.rev rl)) in
  let env = opam_env compvers in
  let total = List.length rl in
  let rec find_start l acc =
    Status.(
      cur.step <- Install { stored = true; cur = List.length l; total;
                            cur_pack = "" };
    );
    if restore_result compvers l then begin
      Status.show ();
      Some (acc, l)
    end else begin
      match l with
      | [] ->
          Status.show ();
          None
      | h :: t -> find_start t (h :: acc)
    end
  in
  let rec play l acc =
    match read_result compvers with
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
            show ();
          );
          let cmd =
            if pack = "compiler" then
              sprintf "opam switch %s" vers
            else
              sprintf "opam install %s" packvers
          in
          let packs_done = ((pack, vers) :: acc) in
          print_command_to_log compvers cmd;
          let rc = run ~env (sprintf "%s > %s 2>&1" cmd (log_file compvers)) in
          if rc <> 0 then begin
            Status.show_result '#';
            write_failure compvers packs_done;
          end else begin
            Status.show_result '+';
            write_success compvers;
          end;
          save compvers packs_done;
          play t packs_done
       end
  in
  match find_start rl [] with
  | None ->
     let dir = gitdir compvers in
     run0 (sprintf "/bin/rm -rf %s" dir);
     run0 (sprintf "/bin/mkdir -p %s" (opamroot compvers));
     run0 ~env
          (sprintf "opam init --comp=%s --no-setup default %s" compvers repo);
     run0 (sprintf "git -C %s init" dir);
     run0 (sprintf "echo '!*' >%s" (Filename.concat dir ".gitignore"));
     write_success compvers;
     save compvers [];
     play (List.rev rl) []
  | Some (todo, cached) ->
     begin match read_result compvers with
     | Failed l -> Status.show_result '#'; Failed l
     | OK when todo = [] -> OK
     | OK ->
       restore compvers cached;
       play todo cached;
     end

let prefix = "-> installed "
let prefix_len = String.length prefix

let is_prefixed s =
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let rec parse_opam_schedule ic accu =
  match input_line ic with
  | s ->
     if is_prefixed s then begin
       let pack = String.sub s prefix_len (String.length s - prefix_len) in
       match Version.split_name_version pack with
       | (name, Some vers) -> parse_opam_schedule ic ((name, vers) :: accu)
       | _ -> assert false
     end else
       parse_opam_schedule ic accu
  | exception End_of_file -> accu

let ask_opam comp name vers =
  restore comp [("compiler", comp)];
  let cmd =
    sprintf "%s OPAMVERBOSE=0 opam install -y --dry-run %s.%s >%s"
      (opam_env comp) name vers tmp_opam_out
  in
  begin match Sys.command cmd with
  | 0 ->
     let ic = open_in tmp_opam_out in
     let res = parse_opam_schedule ic [("compiler", comp)] in
     close_in ic;
     res
  | _ ->
     Log.warn "opam install failed for %s %s.%s\n" comp name vers;
     []
  end
