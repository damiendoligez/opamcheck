(* summarize.ml -- display opamcheck results in HTML
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

open Util

let verbose = ref false
let show_all = ref false
let header = ref ""
let version = ref ""

let results_file = Filename.concat Util.sandbox "results"
let weights_file = Filename.concat Util.sandbox "weights"
let summary_dir = Filename.concat Util.sandbox "summary"
let data_dir = Filename.concat summary_dir "data"
let index_file = Filename.concat summary_dir "index.html"
let fullindex_file = Filename.concat summary_dir "fullindex.html"
let state_dir = Filename.concat Util.sandbox "opamstate"
let mystate_dir = Filename.concat Util.sandbox "opamstate.tmp"
let tmp_dir = Filename.concat Util.sandbox "tmp"
let log_file = Filename.concat mystate_dir "opamcheck-log"

let command ?(ignore_errors=false) s =
  if !verbose then begin
    eprintf "+ %s\n" s;
    flush stderr;
  end;
  match Sys.command s with
  | 0 -> ()
  | n ->
     if not ignore_errors then
       failwith (sprintf "command `%s` failed with code %d\n" s n)

type status = OK | Uninst | Fail | Depfail | Unknown

let get m p =
  try SM.find p m with Not_found -> (Unknown, Unknown, [])

let merge x y =
  match x, y with
  | OK, _ | _, OK -> OK
  | Fail, _ | _, Fail -> Fail
  | Depfail, _ | _, Depfail -> Depfail
  | Uninst, _ | _, Uninst -> Uninst
  | Unknown, Unknown -> Unknown

let add status line comp m p =
  let comp =
    match Version.split_name_version comp with
    | ("compiler", Some v) -> v
    | _ -> assert false
  in
  let (st_old, st_new, lines) = get m p in
  let lines = if List.mem line lines then lines else line :: lines in
  let st =
    if comp = !version then begin
      (st_old, merge st_new status, lines)
    end else begin
      (merge st_old status, st_new, lines)
    end
  in
  SM.add p st m

let rec find_comp p l accu =
  match l with
  | [] -> failwith "missing close bracket"
  | [ "]" ] -> assert (accu = []); (p, p, [])
  | [ comp; "]" ] -> (comp, p, List.rev accu)
  | h :: t -> find_comp p t (h :: accu)

let parse_list l =
  match l with
  | [] -> failwith "missing close bracket"
  | h :: t -> find_comp h t []

let parse_line s m =
  let words = String.split_on_char ' ' s in
  match words with
  | ["ok"; tag; "["; "]"] -> m
  | "ok" :: tag :: "[" :: l ->
     let (comp, pack, deps) = parse_list l in
     let m = add OK s comp m pack in
     List.fold_left (add OK (" " ^ s) comp) m deps
  | ["uninst"; comp; pack] ->
     add Uninst s comp m pack
  | "depfail" :: tag :: pack :: "[" :: l ->
     let (comp, _, deps) = parse_list l in
     let m = add Depfail s comp m pack in
     List.fold_left (add OK (" " ^ s) comp) m deps
  | "fail" :: tag :: "[" :: l ->
     let (comp, pack, deps) = parse_list l in
     let m = add Fail s comp m pack in
     List.fold_left (add OK (" " ^ s) comp) m deps
  | _ -> failwith "syntax error in results file"

let parse chan =
  let rec loop lnum m =
    match input_line chan with
    | l ->
      begin
        match parse_line l m with
        | pl -> loop (lnum+1) pl
        | exception e ->
          eprintf "error at line %d: %s\n" lnum (Printexc.to_string e);
          failwith "error in results file";
      end
    | exception End_of_file -> m
  in
  loop 1 SM.empty

let same_pack p1 p2 =
  let (name1, _) = Version.split_name_version p1 in
  let (name2, _) = Version.split_name_version p2 in
  name1 = name2

let rec group_packs l accu =
  match l with
  | [] -> List.rev accu
  | (pack, _) as h :: t -> group_packs_with pack t [h] accu
and group_packs_with p l accu1 accu2 =
  match l with
  | (pack, _) as h :: t when same_pack p pack ->
     group_packs_with p t (h :: accu1) accu2
  | _ -> group_packs l (accu1 :: accu2)

let color status =
  match status with
  | _, OK, _ -> ("ok", "o")
  | OK, Fail, _ -> ("new_fail", "X")
  | Fail, Fail, _ -> ("old_fail", "x")
  | _, Fail, _ -> ("fail", "x")
  | OK, Uninst, _ -> ("new_uninst", "U")
  | _, Uninst, _ -> ("uninst", "u")
  | OK, Depfail, _ -> ("new_depfail", "D")
  | _, Depfail, _ -> ("depfail", "d")
  | _, Unknown, _ -> ("unknown", "?")

let summary_hd title = sprintf "\
<!DOCTYPE html>\n<html><head>\n\
<style>\n\
.keyfail {color: #bb0000; font-weight: bold;}\n\
.keyok {color: #008800; font-weight: bold;}\n\
.keydepfail {color: #bb5500; font-weight: bold;}\n\
.keyuninst {color: #bb5500; font-weight: bold;}\n\
.curpack {font-weight:bold;}\n\
</style>\n\
<title>%s</title>\n\
</head><body>\n"
title

let summary_tl = "</body></html>\n"

let print_detail_list oc packvers l =
  let rec loop l =
    match l with
    | [] -> ()
    | pv :: ll when pv = packvers ->
      fprintf oc " <span class=\"curpack\">%s</span>%s" pv
              (if ll = [] then "" else " ...")
    | pv :: ll -> fprintf oc " <a href=\"%s.html\">%s</a>" pv pv; loop ll
  in
  match List.rev l with
  | "]" :: h :: t -> fprintf oc " %s" h; loop t
  | l -> loop l

let print_detail_line oc pack vers line =
  let packvers = sprintf "%s.%s" pack vers in
  match String.split_on_char ' ' line with
  | "fail" :: tag :: "[" :: (pv :: _ as l) when pv = packvers ->
     let f = sprintf "%s.%s-%s.txt" pack vers tag in
     let absf = Filename.quote (Filename.concat data_dir f) in
     begin match Version.split_name_version (List.nth (List.rev l) 1) with
     | (_, Some v) ->
       let stdir = Filename.quote (Filename.concat mystate_dir v) in
       let cmd =
         sprintf "git -C %s show remotes/origin/%s:opamcheck-log > %s"
                 stdir tag absf
       in
       command ~ignore_errors:true cmd;
     | _ -> ()
     | exception _ -> ()
     end;
     fprintf oc "<a href=\"%s\" class=\"keyfail\">fail</a> %s<br>[" f tag;
     print_detail_list oc packvers l;
     fprintf oc " ]\n<hr>\n"
  | "fail" :: tag :: "[" :: l ->
     fprintf oc "<span class=\"keyok\">ok</span> %s<br>[" tag;
     print_detail_list oc packvers l;
     fprintf oc " ]\n<hr>\n"
  | "ok" :: tag :: "[" :: l ->
     fprintf oc "<span class=\"keyok\">ok</span> %s<br>[" tag;
     print_detail_list oc packvers l;
     fprintf oc " ]\n<hr>\n"
  | "depfail" :: tag :: pv :: "[" :: l ->
     fprintf oc "<span class=\"keydepfail\">depfail</span> %s" tag;
     fprintf oc " <span class=\"curpack\">%s</span><br>[" pv;
     print_detail_list oc packvers l;
     fprintf oc " ]\n<hr>\n"
  | ["uninst"; compiler; pv] ->
     fprintf oc "<span class=\"keyuninst\">uninst</span> %s %s" compiler pv;
     fprintf oc "\n<hr>\n"
  | "" :: _ -> ()
  | _ -> fprintf oc "'%s'\n<hr>\n" line

let group_details l =
  let get_group s =
    let key = " compiler." in
    let keylen = String.length key in
    match string_search key s with
    | None -> ""
    | Some i ->
      let ik = i + keylen in
      begin match String.index_from_opt s ik ' ' with
      | None -> ""
      | Some j -> String.sub s ik (j - ik)
      end
  in
  let f accu s =
    let g = get_group s in
    let prev = try SM.find g accu with Not_found -> [] in
    SM.add g (s :: prev) accu
  in
  SM.bindings (List.fold_left f SM.empty l)

let sort_details l =
  let prio s =
    match s.[0] with
    | 'f' -> 0
    | 'd' -> 1
    | 'u' -> 2
    | 'o' -> 3
    | ' ' -> 4
    | _ -> assert false
  in
  let cmp s1 s2 = compare (prio s1) (prio s2) in
  List.sort cmp l

let print_details file pack vers (_, _, lines) =
  let oc = open_out (Filename.concat summary_dir file) in
  fprintf oc "%s" (summary_hd (sprintf "%s.%s" pack vers));
  fprintf oc "<h1>%s.%s</h1>\n" pack vers;
  let print_group (key, l) =
    fprintf oc "<h3>%s</h3><hr>\n" key;
    List.iter (print_detail_line oc pack vers) (sort_details l)
  in
  List.iter print_group (group_details lines);
  fprintf oc "%s" summary_tl;
  close_out oc

let print_result b (p, st) =
  let (pack, vers) = Version.split_name_version p in
  match vers with
  | None ->
     eprintf "warning: missing version number in results file: %s\n" p
  | Some vers ->
     let auxfile = Filename.concat "data" (p ^ ".html") in
     print_details auxfile pack vers st;
     let (col, txt) = color st in
     bprintf b "  <td class=\"%s\"><div class=\"tt\"><a href=\"%s\">%s\
                  </a><span class=\"ttt\">%s %s</span></div></td>\n"
             col auxfile txt vers col

let compare_vers (p1, _) (p2, _) =
  match (Version.split_name_version p1, Version.split_name_version p2) with
  | (_, Some v1), (_, Some v2) -> Version.compare v2 v1
  | _ -> assert false

let is_interesting l =
  let f (pack, st) =
    fst (Version.split_name_version pack) <> "compiler"
    && match color st with
       | ("ok" | "uninst" | "new_uninst" | "unknown"), _ -> !show_all
       | _ -> true
  in
  List.exists f l

let print_result_line oc fulloc l =
  match l with
  | [] -> assert false
  | (p, _) :: _ ->
    let b = Buffer.create 1000 in
    let (name, _) = Version.split_name_version p in
    bprintf b "<tr><th>%s</th>\n" name;
    List.iter (print_result b) (List.sort compare_vers l);
    bprintf b "</tr>\n";
    Buffer.output_buffer fulloc b;
    if is_interesting l then Buffer.output_buffer oc b

let spec = Arg.[
  "-all", Set show_all, " Show all results";
  "-v", Set verbose, " Activate verbose mode";
  "-head", Set_string header, "<s> Insert <s> at top of body in index file";
]

let anon v =
  if !version <> "" then raise (Arg.Bad "too many arguments");
  version := v

let usage = "usage: summarize [-all] <version>"

let html_header = "\
<!DOCTYPE html>\n\
<html><head>\n\
<style>\n\
.ok {background-color: #66ff66;}\n\
.new_uninst {background-color: #ffff30;}\n\
.uninst {background-color: #cccccc;}\n\
.new_depfail {background-color: #ff8800;}\n\
.depfail {background-color: #ffe0cc;}\n\
.new_fail {background-color: #ff3030;}\n\
.old_fail {background-color: #eb99ff;}\n\
.fail {background-color: #ffcccc;}\n\
.unknown {background-color: #bbbbff;}\n\
.tt {\n\
    position: relative;\n\
    display: inline-block;\n\
}\n\
.tt .ttt {\n\
    visibility: hidden;\n\
    width: 120px;\n\
    background-color: #ffeedd;\n\
    text-align: center;\n\
    padding: 5px 5px;\n\
    position: absolute;\n\
    z-index: 1;\n\
    top: 120%;\n\
    left: 50%;\n\
    margin-left: -60px;\n\
}\n\
.tt:hover .ttt { visibility: visible; }\n\
th { text-align: right; }\n\
td { text-align: center; }\n\
</style>\n\
</head>\n\
"

let html_body_start = ("<body>\n%s<table>\n" : _ format)
let html_body_end = "</table></body></html>\n"

let read_results () =
  let ic = open_in results_file in
  let res = parse ic in
  close_in ic;
  res

let read_weights () =
  let ic = Scanf.Scanning.from_channel (open_in weights_file) in
  let rec loop m =
    match Scanf.bscanf ic "%d %s " (fun w p -> SM.add p w m) with
    | m2 -> loop m2
    | exception End_of_file -> m
  in
  loop SM.empty

let main () =
  Arg.parse spec anon usage;
  if !version = "" then (Arg.usage spec usage; exit 2);
  let weigths = read_weights () in
  let results = SM.bindings (read_results ()) in
  let groups = group_packs results [] in
  let get_weight group =
    match group with
    | (pv, _) :: _ ->
      let p, _ = Version.split_name_version pv in
      SM.find p weigths
    | [] -> assert false
  in
  let cmp g1 g2 = compare (get_weight g2) (get_weight g1) in
  let groups = List.sort cmp groups in
  let cmd = sprintf "mkdir -p %s" (Filename.concat summary_dir "data") in
  command cmd;
  let cmd = sprintf "mkdir -p %s" (Filename.quote tmp_dir) in
  command cmd;
  command (sprintf "rm -rf %s" (Filename.quote mystate_dir));
  command (sprintf "mkdir -p %s" (Filename.quote mystate_dir));
  let f d =
    let origin = Filename.(quote (concat state_dir d)) in
    let dest = Filename.(quote (concat mystate_dir d)) in
    command (sprintf "git clone %s %s" origin dest);
  in
  Array.iter f (Sys.readdir state_dir);
  let index = open_out index_file in
  let fullindex = open_out fullindex_file in
  fprintf index "%s" html_header;
  fprintf fullindex "%s" html_header;
  fprintf index html_body_start !header;
  fprintf fullindex html_body_start !header;
  List.iter (print_result_line index fullindex) groups;
  fprintf index "%s" html_body_end;
  fprintf fullindex "%s" html_body_end;

;; main ()
