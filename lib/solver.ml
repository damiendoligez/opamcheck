(* solver.ml -- find installable subsets of OPAM packages
   Copyright 2017 Inria
   author: Damien Doligez
*)

open Package
open Util

let rec extract_solution u bindings accu =
  match bindings with
  | [] -> accu
  | (name, l) :: t ->
     let is_true (_, lit) = Minisat.value u.sat lit = Minisat.V_true in
     let acc2 =
       match List.find is_true l with
       | exception Not_found -> accu
       | (vers, _) -> (name, vers) :: accu
     in
     extract_solution u t acc2

let find_depopts u l =
  let f (packs, depopts) (pack, vers) =
    let packs = SS.add pack packs in
    let f p = p.version = vers in
    let p = List.find f (SM.find pack u.Package.pack_map) in
    let f accu name = SS.add name accu in
    let depopts = List.fold_left f depopts p.Package.dep_opt in
    (packs, depopts)
  in
  let (packs, depopts) = List.fold_left f (SS.empty, SS.empty) l in
  SS.diff depopts packs

let negate u l =
  let f accu name =
    match SM.find name u.Package.lits with
    | exception Not_found -> accu
    | vers ->
      List.map (fun (_, lit) -> Minisat.Lit.neg lit) vers @ accu
  in
  List.fold_left f [] l

let solve u ?(forbid=[]) prev ~ocaml ~pack ~vers =
  let f (n, vs) =
    match vs with
    | [v] -> (n, v)
    | _ -> assert false
  in
  let env = List.map f (Env.get [ocaml]) in
  let query = env @ (pack, vers) :: prev in
  let f (name, vers) = Package.find_lit u name vers in
  let asm1 = List.map f query in
  let depopts = find_depopts u prev in
  let asm2 = negate u (SS.elements depopts) in
  let f (name, vers) = Minisat.Lit.neg (Package.find_lit u name vers) in
  let asm3 = List.map f forbid in
  let assumptions = Array.of_list (asm3 @ asm1 @ asm2) in
  match Minisat.solve ~assumptions u.sat with
  | exception Minisat.Unsat ->
    None
  | () ->
    Some (extract_solution u (SM.bindings u.lits) [])

exception Schedule_failure of (string * string) list * (string * string) list

let rec eval atom env form =
  match form with
  | Ast.And (f1, f2) -> eval atom env f1 && eval atom env f2
  | Ast.List fl -> List.for_all (eval atom env) fl
  | Ast.Or (f1, f2) -> eval atom env f1 || eval atom env f2
  | Ast.Not f -> not (eval atom env f)
  | Ast.Atom a -> atom env a

let compare_version ctxv v vers =
  match vers with
  | Ast.V s -> Version.compare v s
  | Ast.Same_version -> Version.compare v ctxv

let eval_op op compare_result =
  match op with
  | `Eq -> compare_result = 0
  | `Lt -> compare_result < 0
  | `Gt -> compare_result > 0
  | `Leq -> compare_result <= 0
  | `Geq -> compare_result >= 0
  | `Neq -> compare_result <> 0

let eval_constraint ctxv name env (op, vers) =
  let check (n, v) =
    n = name
    && eval_op op (compare_version ctxv v vers)
  in
  List.exists check env

let eval_package ctxv env (name, fo) =
  match fo with
  | None -> List.exists (fun (n, _) -> n = name) env
  | Some f -> eval (eval_constraint ctxv name) env f

let eval_deps ctxv env f = eval (eval_package ctxv) env f

let compat u (name, vers) sol remain1 remain2 =
  let f p = p.version = vers in
  let p = List.find f (SM.find name u.Package.pack_map) in
  let check_opt name =
    not (List.exists (fun (n, _) -> n = name) remain1)
    && not (List.exists (fun (n, _) -> n = name) remain2)
  in
  eval_deps vers sol p.Package.deps && List.for_all check_opt p.Package.dep_opt

let schedule u prev sol target =
  let todo = SPS.diff (SPS.of_list sol) (SPS.of_list prev) in
  let todo = SPS.elements todo in
  let rec find_next pr todo postponed =
    match todo with
    | [] -> raise (Schedule_failure (pr, postponed))
    | h :: t ->
       if compat u h pr t postponed then
         (h, List.rev_append postponed t)
       else
         find_next pr t (h :: postponed)
  in
  let rec loop pr todo =
    match todo with
    | [] -> pr
    | _ ->
       let (h, t) = find_next pr todo [] in
       loop (h :: pr) (if h = target then [] else t)
  in
  let check_comp (n, _) = n = "ocaml" in
  if List.exists check_comp todo then begin
    let comp = List.find check_comp todo in
    let rest = List.filter (fun pv -> not (check_comp pv)) todo in
    assert (prev = []);
    loop [comp] rest
  end else
    loop prev todo
