(* package.mli -- package data structure and functions
   Copyright 2017 Inria
   author: Damien Doligez
*)

type t = {
  name : string;
  version : string;
  checksum : string; (* readable version of MD5 of opam file *)
  lit : Minisat.Lit.t;
  dep_opt : string list;
  deps : Ast.package Ast.formula;
}
(** A record that gives information on a given package at a given version. *)

type u = {
  sat : Minisat.t;
  packs : t list;
  pack_map : t list Util.SM.t;
  lits : (string * Minisat.Lit.t) list Util.SM.t;
  revdeps : Util.SS.t Util.SM.t;
}
(** A universe of packages. *)

val find : u -> string -> string -> t
(** Find the given package in the universe.
    @raise Not_found
*)

val find_lit : u -> string -> string -> Minisat.Lit.t
(** Find the literal that represents the given package and version.
    @raise Not_found
*)

val make : string list -> (string * Ast.opam list * string) list -> u
(** [make ocaml_versions asts]
    Create the Minisat instance from a list of OCaml versions and a list of
    ASTs, populate the Minisat instance with all the package constraints
    (dependencies, conflicts, availability).
    Return the package universe.
*)

val show : t -> unit
(** Display the package's contents to stdout *)
