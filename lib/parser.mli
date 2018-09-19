(* parser.mli -- parser for OPAM's [opam] files

   This is a wrapper on to of the [opam-file-format] library, that extracts the
   information relevant for this tool (mainly, versions and version
   constraints).

   Copyright 2018 Inria
   author: Armaël Guéneau
*)

(* Carries the location of the error. *)
exception Ill_formed_file of (string * int * int)

(* [opam file] parses an opam file given its filename.

   Raises [Ill_formed_file] if it encountered an AST it did not know how to
   handle. *)
val opam : string -> Ast.opam list
