(* opamcheck.ml -- main program for opamcheck
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Opamchecklib
open Printf

open Util

let retries = ref 5
let compilers = ref []
let sandbox = ref None
let show_all = ref false
let verbose = ref false
let header = ref ""
let smoke = ref false

let parse_opam file =
  try Parser.opam file with
  | Parser.Ill_formed_file (file, line, col) ->
    Log.warn "\"%s\":%d.%d:\n  Ill-formed opam file\n" file line col;
    []

let parse_file dir file =
  let res =
    try
      let res = parse_opam file in
      res
    with _ -> []
  in
  (dir, res, Digest.to_hex (Digest.file file))

let fold_opam_files f accu dir =
  let rec dig dir accu name =
    let fullname = Filename.concat dir name in
    if name = "opam" then begin
      f accu (Filename.basename dir) fullname
    end else if Sys.is_directory fullname then
      Array.fold_left (dig fullname) accu (Sys.readdir fullname)
    else
      accu
  in
  dig dir accu "."

type status =
  | Try of int * int  (* number of fails, number of depfails *)
  | OK
  | Uninst
  | Fail

let read_lines file =
  if Sys.file_exists file then begin
    let ic = open_in file in
    let rec loop set =
      match input_line ic with
      | s -> loop (s :: set)
      | exception End_of_file -> set
    in
    let result = loop [] in
    close_in ic;
    result
  end else
    []

let cache = ref (SPLS.singleton [])

type progress = {
  mutable statuses : (string * status) list SPM.t;
}

let get_status p name vers comp =
  try
    snd (List.find (fun (c, _) -> c = comp) (SPM.find (name, vers) p.statuses))
  with Not_found -> Try (0, 0)

let set_status p name vers comp st =
  let l =
    try
      let l = SPM.find (name, vers) p.statuses in
      let f (c, _) = c <> comp in
      (comp, st) :: List.filter f l
    with Not_found -> [(comp, st)]
  in
  p.statuses <- SPM.add (name, vers) l p.statuses

let print_solution chan l =
  fprintf chan "[";
  List.iter (fun (n, v) -> fprintf chan " %s.%s" n v) l;
  fprintf chan " ]"

let record_ok p comp l =
  let (tag, list) = Sandbox.get_tag l in
  Log.res "ok %s [%s ]\n" tag list;
  let add_ok (name, vers) =
    match get_status p name vers comp with
    | OK -> ()
    | Try _ ->
       set_status p name vers comp OK;
    | Fail | Uninst -> assert false
  in
  let rec loop l =
    cache := SPLS.add l !cache;
    match l with
    | [] -> ()
    | h :: t -> add_ok h; loop t
  in
  loop l

let forbid_solution u l =
  let f (n, v) = Minisat.Lit.neg (Package.find_lit u n v) in
  Minisat.add_clause_l u.Package.sat (List.map f l)

let record_failed u p comp l =
  let (tag, list) = Sandbox.get_tag l in
  Log.res "fail %s [%s ]\n" tag list;
  forbid_solution u l;
  match l with
  | [] -> assert false
  | (name, vers) :: t ->
     begin match get_status p name vers comp with
     | OK -> ()
     | Try (f, d) ->
        if f >= !retries then begin
          forbid_solution u [("ocaml", comp); (name, vers)];
          set_status p name vers comp Fail
        end else begin
          set_status p name vers comp (Try (f + 1, d))
        end
     | Uninst ->
       Log.warn "Warning: uninstallable package was attempted and failed:\n\
                   %s\n" list;
     | Fail ->
       Log.warn "Warning: failed package was attempted again and failed:\n\
                   %s\n" list;
     end;
     record_ok p comp t

let record_uninst p comp name vers =
  Log.res "uninst ocaml.%s %s.%s\n" comp name vers;
  match get_status p name vers comp with
  | Try (0, 0) | Uninst ->
     set_status p name vers comp Uninst
  | _ -> assert false

let record_depfail p comp name vers l =
  match get_status p name vers comp with
  | OK -> ()
  | Try (f, d) ->
     let (tag, list) = Sandbox.get_tag l in
     Log.res "depfail %s %s.%s [%s ]\n" tag name vers list;
     set_status p name vers comp (Try (f, d + 1))
  | Uninst | Fail -> assert false

let find_sol ~sandbox u comp name vers attempt forbid prev =
  let result = ref None in
  let n = ref 0 in
  let f forb pp =
    match Solver.solve u ~forbid:(pp :: forb) [] ~ocaml:comp ~pack:name ~vers
    with
    | None -> forb
    | Some _ -> pp :: forb
  in
  let forbid = List.fold_left f forbid prev in
  let check cached =
    incr n;
    Status.(cur.step <- Solve (!n, List.length cached));
    match Solver.solve ~forbid u cached ~ocaml:comp ~pack:name ~vers with
    | None -> ()
    | Some raw_sol ->
       let sol = List.filter Env.is_package raw_sol in
       begin try
         result := Some (Solver.schedule u cached sol (name, vers));
         raise Exit
       with Solver.Schedule_failure (partial, remain) ->
         Log.warn "schedule failed, partial = ";
         print_solution (Log.warn_chan ()) partial;
         Log.warn "\nremain = ";
         print_solution (Log.warn_chan ()) remain;
         Log.warn "\n";
         forbid_solution u raw_sol;
       end
  in
  (* Look for a solution in an empty environment before trying to solve
     with cached states. If there is none, the package is uninstallable. *)
  Status.(cur.step <- Solve (0, 0));
  let empty_sol = Solver.solve u [] ~ocaml:comp ~pack:name ~vers in
  if empty_sol = None then begin
    result := None
  end else begin
    (* On first attempt, use cache. *)
    let cached =
      match attempt with
      | 0 -> SPLS.elements !cache
      | _ -> [ [] ]
    in
    (try List.iter check cached with Exit -> ());
  end;
  Status.show ~sandbox ();
  Status.show_result (if !result = None then '#' else '+');
  (!result, forbid)

(*
   test each package, first with a cached solution, then with
   successive minimal solutions
*)
let test_comp_pack ~sandbox u progress comp pack =
  let name = pack.Package.name in
  let vers = pack.Package.version in
  let rec loop forbid prev attempt =
    if attempt >= !retries then () else begin
      let st = get_status progress name vers comp in
      if st <> OK then begin
        Status.(
          cur.ocaml <- comp;
          cur.pack_cur <- sprintf "%s.%s" name vers;
        );
        Log.log "testing: %s.%s (attempt %d)\n" name vers attempt;
        match find_sol ~sandbox u comp name vers attempt forbid prev with
        | None, _ ->
           Log.log "no solution\n";
           (* make sure attempt gets incremented *)
           begin match get_status progress name vers comp with
           | Try (f, d) -> set_status progress name vers comp (Try (f, d + 1))
           | _ -> ()
           end
        | Some sched, forbid ->
           Log.log "solution: ";
           print_solution (Log.log_chan ()) sched;
           Log.log "\n";
           begin match Sandbox.play_solution ~sandbox sched with
           | Sandbox.OK -> record_ok progress comp sched
           | Sandbox.Failed l ->
              record_failed u progress comp l;
              if List.hd l <> (name, vers) then
                record_depfail progress comp name vers l;
              loop forbid sched (attempt + 1);
           end
      end
    end
  in
  loop [] [] 0


let register_exclusion u s =
  let (name, vers) = Version.split_name_version s in
  try
    match vers with
    | Some v -> forbid_solution u [(name, v)]
    | None ->
       let f (v, _) = forbid_solution u [(name, v)] in
       List.iter f (SM.find name u.Package.lits)
  with Not_found ->
    Log.warn "Warning in excludes: %s not found\n" s

let print_version () =
  printf "2.2.0\n";
  exit 0

let spec = [
  "-all", Arg.Set show_all, " Show all results (summarize)";
  "-head", Arg.Set_string header,
        "<s>  Insert <s> at top of body in index file (summarize)";
  "-retries", Arg.Set_int retries,
           "<n>  Retry failed packages <n> times (default 5) (run)";
  "-sandbox", Arg.String (fun s -> sandbox := Some s),
    "<path>  Set the location of the sandbox directory to <path> (mandatory; \
     alternatively can be specified by setting the OPCSANDBOX \
     environment variable)";
  "-smoke", Arg.Set smoke,
         " Smoke test mode: compile only a few packages (run)";
  "-v", Arg.Set verbose, " Activate verbose mode (summarize)";
  "-version", Arg.Unit print_version, " Print version number and exit";
]

let usage =
  Format.sprintf "usage: @[<v>%s@,%s@]"
    "opamcheck [-sandbox <path>] [-retries <n>] run version..."
    "opamcheck [-sandbox <path>] [-all] [-v] [-head <s>] summarize version"

let command = ref None

let arg_anon =
  fun s ->
    match !command, s with
    | None, "run" -> command := Some `Run
    | None, "summarize" -> command := Some `Summarize
    | None, cmd -> raise (Arg.Bad ("Invalid command " ^ cmd))
    | Some `Run, ver -> compilers := ver :: !compilers
    | Some `Summarize, ver ->
      if !compilers <> [] then
        raise (Arg.Bad "too many arguments: \
                        only one version can be provided to summarize")
      else compilers := [ver]

let get_sandbox () =
  match !sandbox with
  | Some s -> s
  | None ->
    try Sys.getenv "OPCSANDBOX"
    with Not_found ->
      eprintf "opamcheck: missing -sandbox option and \
               environment variable OPCSANDBOX is undefined\n";
      exit 5

(* Not tail-rec, only use with n < 100 *)
let rec list_truncate l n =
  match l, n with
  | _, 0 -> []
  | [], _ -> []
  | x :: t, n -> x :: list_truncate t (n-1)

let main_run sandbox =
  Log.init ~sandbox ();
  Log.log "reading packages files\n";
  let repo = Filename.concat sandbox "opam-repository" in
  let asts = fold_opam_files (fun acc dir name ->
    if List.mem (fst (Version.split_name_version dir))
                ["ocaml-base-compiler"; "ocaml-variants"] then
      acc
    else
      parse_file dir name :: acc
  ) [] repo in
  let u = Package.make !compilers asts in

  let oc = open_out (Filename.concat sandbox "weights") in
  let p_deps p d = fprintf oc "%d %s\n" (SS.cardinal d) p in
  SM.iter p_deps u.Package.revdeps;
  close_out oc;

  let excludes = read_lines (Filename.concat sandbox "exclude") in
  List.iter (register_exclusion u) excludes;
  let p = {
    statuses = SPM.empty;
  } in
  let comp, comps =
    match !compilers with
    | [] -> Arg.usage spec usage; exit 1
    | comp :: comps -> (comp, comps)
  in
  let cmp p1 p2 =
    Package.(
      let c = Pervasives.compare p1.name p2.name in
      if c = 0 then Version.compare p2.version p1.version else begin
        let w1 = SS.cardinal (SM.find p1.name u.Package.revdeps) in
        let w2 = SS.cardinal (SM.find p2.name u.Package.revdeps) in
        if w1 = w2 then c else w2 - w1
      end
    )
  in
  let packs = List.sort cmp u.Package.packs in
  (* Start by recording truly uninstallable packages. Anything that
     becomes uninstallable after that, is in fact a depfail.
  *)
  let check_inst comp pack =
    let name = pack.Package.name in
    let vers = pack.Package.version in
    match Solver.solve u [] ~ocaml:comp ~pack:name ~vers with
    | None -> record_uninst p comp name vers
    | Some _ -> ()
  in
  Status.(cur.step <- Solve (0, 0); show ~sandbox ());
  Log.log "checking for uninstallable packages\n";
  List.iter (fun comp -> List.iter (check_inst comp) packs) !compilers;
  let is_done c pack fail_done =
    match get_status p pack.Package.name pack.Package.version c with
    | OK | Uninst -> true
    | Try _ -> false
    | Fail -> fail_done
  in
  (* First pass: try each package with the latest compiler. *)
  let packs = List.filter (fun p -> not (is_done comp p false)) packs in
  Status.(
    cur.pass <- 1;
    cur.pack_done <- 0;
    cur.pack_total <- List.length packs
  );
  let f pack =
    test_comp_pack ~sandbox u p comp pack;
    Status.(cur.pack_done <- cur.pack_done + 1)
  in
  Log.log "## first pass (%d packages)\n" Status.(cur.pack_total);
  let packs = if !smoke then list_truncate packs 5 else packs in
  List.iter f packs;
  (* Second pass: try failing packages with every other compiler.
     Stop as soon as it installs OK with some configuration.
  *)
  let packs = List.filter (fun p -> not (is_done comp p false)) packs in
  Status.(
    cur.pass <- 2;
    cur.pack_done <- 0;
    cur.pack_total <- List.length packs
  );
  let f pack =
    let rec loop comps =
      match comps with
      | [] -> ()
      | h :: t ->
         test_comp_pack ~sandbox u p h pack;
         if get_status p pack.Package.name pack.Package.version h <> OK then
           loop t
    in
    loop comps;
    Status.(cur.pack_done <- cur.pack_done + 1)
  in
  Log.log "## second pass (%d packages)\n" Status.(cur.pack_total);
  List.iter f packs;
  Status.message "\nDONE\n"

let main_summarize sandbox =
  let version = match !compilers with [v] -> v | _ -> assert false in
  Summarize.summarize ~show_all:!show_all ~verbose:!verbose ~header:!header
                      ~sandbox ~version ()

let main () =
  Arg.parse spec arg_anon usage;
  if !compilers = [] then begin
    Arg.usage spec usage;
    exit 1;
  end;
  let sandbox = get_sandbox () in
  match !command with
  | None -> assert false
  | Some `Run -> main_run sandbox
  | Some `Summarize -> main_summarize sandbox

;; main ()
