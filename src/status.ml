(* status.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

type step =
  | Read of string
  | Cache
  | Solve of int
  | Install of { stored : bool; total : int; cur : int; cur_pack : string }

type t = {
  mutable ocaml : string;
  mutable pack_ok : int;
  mutable pack_done : int;
  mutable pack_total : int;
  mutable pack_cur : string;
  mutable step : step;
}

let cur = {
  ocaml = "";
  pack_ok = 0;
  pack_done = 0;
  pack_total = 0;
  pack_cur = "";
  step = Read "";
}

let sandbox =
  try Sys.getenv "OPCSANDBOX"
  with Not_found -> begin
    eprintf "opamcheck: environment variable OPCSANDBOX is undefined\n";
    exit 1;
  end

let stchan = open_out (Filename.concat sandbox "status")

let spaces = String.make 80 ' '

let stopfile = Filename.concat sandbox "stop"

let show () =
  if Sys.file_exists stopfile then begin
    (try Sys.remove stopfile with _ -> ());
    fprintf stchan "\nSTOPPED BY USER\n";
    Pervasives.exit 10;
  end;
  let s1 =
    sprintf "%s %d/%d/%d %s "
      cur.ocaml cur.pack_ok cur.pack_done cur.pack_total cur.pack_cur
  in
  let s2 =
    match cur.step with
    | Read s -> sprintf "Read %s" s
    | Cache -> "Cache"
    | Solve n -> sprintf "Solve %d" n
    | Install { stored = true; cur; total; cur_pack } ->
       sprintf "Checkout %d/%d" cur total
    | Install { stored = false; cur; total; cur_pack } ->
       sprintf "Install %d/%d %s" cur total cur_pack
  in
  let s = s1 ^ s2 in
  let len = String.length s in
  let line_length = 78 in
  let s =
    if len < line_length then
      s ^ (String.make (line_length - len) ' ')
    else
      sprintf "%s##%s" (String.sub s 0 (line_length - 12))
        (String.sub s (String.length s - 10) 10)
  in
  fprintf stchan "\r%s" s;
  flush stchan

let show_result c = fprintf stchan "%c" c; flush stchan

let printf fmt (* args *) = fprintf stchan fmt (* args *)

let flush () = Pervasives.flush stchan
