(* summarize.mli -- display opamcheck results in HTML
   Copyright 2017 Inria
   author: Damien Doligez
*)

val summarize :
  show_all:bool ->
  verbose:bool ->
  header:string ->
  sandbox:string ->
  version:string ->
  unit ->
  unit
