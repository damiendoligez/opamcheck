(* log.ml
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Printf

type loggers = {
  log : out_channel;
  results : out_channel;
  status : out_channel;
  warnings : out_channel;
  trace : out_channel;
}

let loggers = ref None

let init ~sandbox () =
  let results = open_out (Filename.concat sandbox "results") in
  let status = open_out (Filename.concat sandbox "status") in
  let warnings = open_out (Filename.concat sandbox "warnings") in
  let trace = open_out (Filename.concat sandbox "trace") in
  loggers :=
    Some { log = Pervasives.stdout; results; status; warnings; trace }

let get_loggers () =
  match !loggers with
  | None -> failwith "Logging was not setup properly. Call [Log.init] first."
  | Some l -> l

let log_chan () = (get_loggers ()).log
let res_chan () = (get_loggers ()).results
let status_chan () = (get_loggers ()).status
let warn_chan () = (get_loggers ()).warnings
let trace_chan () = (get_loggers ()).trace

let log_msg chan s =
  fprintf chan "%s" s; flush chan

let log fmt (* args *) =
  let l = get_loggers () in
  kprintf (log_msg l.log) fmt (* args *)

let res fmt (* args *) =
  let l = get_loggers () in
  kprintf (log_msg l.results) fmt (* args *)

let status fmt (* args *) =
  let l = get_loggers () in
  kprintf (log_msg l.status) fmt (* args *)

let warn fmt (* args *) =
  let l = get_loggers () in
  kprintf (log_msg l.warnings) fmt (* args *)

let trace fmt (* args *) =
  let l = get_loggers () in
  kprintf (log_msg l.trace) fmt (* args *)

let fatal fmt (* args *) =
  let l = get_loggers () in
  let f s = fprintf l.log "%s" s; exit 5 in
  kprintf f fmt (* args *)
