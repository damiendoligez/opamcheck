(* util.mli -- utilities
   Copyright 2017 Inria
   author: Damien Doligez
*)

module SM : Map.S with type key = string
module SPM : Map.S with type key = string * string

module SS : Set.S with type elt = string
module SPS : Set.S with type elt = string * string
module SPLS : Set.S with type elt = (string * string) list

val string_search : string -> string -> int option
(** [string_search key s]
    Returns the position of the first occurrence of [key] in [s], or
    [None] if there is no such occurrence.
*)
