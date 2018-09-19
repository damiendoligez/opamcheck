(* parser.ml -- parser for OPAM's [opam] files

   This is a wrapper on to of the [opam-file-format] library, that extracts the
   information relevant for this tool (mainly, versions and version
   constraints).

   Copyright 2018 Inria
   author: Armaël Guéneau
*)

open OpamParserTypes

exception Ill_formed_file of (string * int * int)

let rec filter_map f = function
  | [] -> []
  | x :: xs ->
    match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

let omap o f = match o with
  | None -> None
  | Some x -> Some (f x)

let lift f = fun x -> Some (f x)

(* The AST produced by opam-file-format is quite lax, more than [Ast.opam] and
   (probably) the opam files actually accepted by opam.

   The [ast_of_*] functions below convert an expression of the generic [value]
   type to the expected expression in [Ast] -- or raise [Ill_formed_file].
*)

let pos_of_value (v: value): string * int * int =
  match v with
  | Bool (p, _) -> p
  | Int (p, _) -> p
  | String (p, _) -> p
  | Relop (p, _, _, _) -> p
  | Prefix_relop (p, _, _) -> p
  | Logop (p, _, _, _) -> p
  | Pfxop (p, _, _) -> p
  | Ident (p, _) -> p
  | List (p, _) -> p
  | Group (p, _) -> p
  | Option (p, _, _) -> p
  | Env_binding (p, _, _, _) -> p

let str_of_value = function
  | Ident (_, s) | String (_, s) -> s
  | v -> raise (Ill_formed_file (pos_of_value v))

(* [atom] is called at the leaves of the formula, and can return [None] if an
   atom is to be ignored. *)
let rec ast_of_formula
    (atom: value -> 'a option) (v: value):
  'a Ast.formula option
  =
  match v with
  | List (_, l) | Group (_, l) ->
    begin match filter_map (ast_of_formula atom) l with
      | [] -> None
      | [f] -> Some f
      | l' -> Some (Ast.List l')
    end
  | Logop (_, `And, v1, v2) ->
    begin match ast_of_formula atom v1, ast_of_formula atom v2 with
      | Some f1, Some f2 -> Some (Ast.And (f1, f2))
      | Some f, None | None, Some f ->
        (* "a & b" get similified to "a" if "b" is ignored (and symmetrically) *)
        Some f
      | None, None -> None
    end
  | Logop (p, `Or, v1, v2) ->
    begin match ast_of_formula atom v1, ast_of_formula atom v2 with
      | Some f1, Some f2 -> Some (Ast.Or (f1, f2))
      | Some _, None | None, Some _ ->
        (* For "a | b", ignoring one of the elements is currently not supported *)
        raise (Ill_formed_file p)
      | None, None -> None
    end
  | Pfxop (_, `Not, v) ->
    omap (ast_of_formula atom v) (fun x -> Ast.Not x)
  | Pfxop (p, `Defined, _v) ->
    (* "? a" is currently not supported *)
    raise (Ill_formed_file p)
  | _ ->
    omap (atom v) (fun x -> Ast.Atom x)

let ast_of_constrain (v: value): Ast.constrain option =
  match v with
  | Prefix_relop (_, op, String (_, s)) ->
    Some (op, s)
  | Relop (_, op, _id, String (_, s)) ->
    (* Note: an identifier [id] gets ignored in constraints *)
    Some (op, s)
  | Ident _ ->
    None
  | v -> raise (Ill_formed_file (pos_of_value v))

let ast_of_package (v: value): Ast.package =
  match v with
  | String (_, s) -> (s, None)
  | Option (loc, String (_, s), vs) ->
      (s, ast_of_formula ast_of_constrain (List (loc, vs)))
  | v -> raise (Ill_formed_file (pos_of_value v))

let ast_of_depend (v: value): Ast.package Ast.formula =
  match ast_of_formula (lift ast_of_package) v with
  | None -> Ast.List []
  | Some ast -> ast

let ast_of_filter (v: value): Ast.filter =
  match v with
  | Relop (_, op, id1, id2) ->
    (str_of_value id1, Some (op, str_of_value id2))
  | Ident (_, id) | String (_, id) ->
    (id, None)
  | Bool (_, b) ->
    (string_of_bool b, None)
  | v -> raise (Ill_formed_file (pos_of_value v))

let ast_of_available (v: value): Ast.filter Ast.formula =
  match ast_of_formula (lift ast_of_filter) v with
  | Some f -> f
  | None -> raise (Ill_formed_file (pos_of_value v))

let ast_of_conflicts (v: value): Ast.package list =
  match v with
  | List (_, l) -> List.map ast_of_package l
  | _ -> [ast_of_package v]

let ast_of_ocaml_version (v: value): Ast.constrain Ast.formula =
  match ast_of_formula ast_of_constrain v with
  | Some f -> f
  | None -> raise (Ill_formed_file (pos_of_value v))

let ast_of_variable (name: string) (v: value): Ast.opam =
  match name, v with
  | "name", String (_, s) -> Ast.Name s
  | "version", String (_, s) -> Ast.Version s
  | ("name" | "version"), _ -> raise (Ill_formed_file (pos_of_value v))
  | "depends", v -> Ast.Depends (ast_of_depend v)
  | "depopts", v -> Ast.Depopts (ast_of_depend v)
  | "conflicts", v -> Ast.Conflicts (ast_of_conflicts v)
  | "available", f -> Ast.Available (ast_of_available f)
  | "ocaml-version", v -> Ast.Ocaml_version (ast_of_ocaml_version v)
  | _, _ -> Ast.Skip

let opam (file: string): Ast.opam list =
  let opamfile = OpamParser.file file in
  List.map (function
    | Section (_pos, _sec) -> Ast.Skip
    | Variable (_pos, name, v) -> ast_of_variable name v
  ) opamfile.file_contents
