(* ast.ml -- abstract syntax tree for OPAM's opam and url files
   Copyright 2017 Inria
   author: Damien Doligez
*)

type opam =
  | Name of string
  | Version of string
  | Depends of package formula
  | Depopts of package formula
  | Conflicts of package list
  | Available of filter formula
  | Ocaml_version of constrain formula
  | Skip

and 'a formula =
  | And of 'a formula * 'a formula
  | List of 'a formula list
  | Or of 'a formula * 'a formula
  | Not of 'a formula
  | Atom of 'a

and package = string * constrain formula option

and filter = string * constrain option

and comp = [ `Eq | `Lt | `Gt | `Leq | `Geq | `Neq ]

and constrain = comp * version

and version =
  | V of string
  | Same_version

type url =
  | Mirrors of string list
  | Key of string * string

val comp_to_string : comp -> string

val show_item : opam -> unit
