(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Util
open Pp
open CErrors
open Names
open Proofview.Notations
open Tac2expr
open Tac2ffi
open Tac2bt
open Tac2debug
open Tac2valtype

exception LtacError = Tac2ffi.LtacError

type environment = Tac2env.environment = {
  env_ist : Tac2env.typed_valexpr Id.Map.t;
  (* stack frames (valid when debugger is enabled) *)
  locs : Loc.t option list;
  stack : (string * Loc.t option) list option;
  (* variable value maps for each stack frame *)
  varmaps : Tac2env.typed_valexpr Id.Map.t list;
  prev_chunks : DebugCommon.chunk list;
}

let empty_environment () = {
  env_ist = Id.Map.empty;
  locs = [];
  stack = if DebugCommon.get_debug () then Some [] else None;
  varmaps = [];
  prev_chunks = [];
}

type closure = {
  mutable clos_env : Tac2env.typed_valexpr Id.Map.t;
  (** Mutable so that we can implement recursive functions imperatively *)
  clos_var : Name.t list;
  (** Bound variables *)
  clos_types: Obj.t list option;
  (** Types of bound variables (really Tac2typing_env.TVar.t Tac2expr.glb_typexpr) *)
  clos_exp : glb_tacexpr;
  (** Body *)
  clos_ref : ltac_constant option;
  (** Global constant from which the closure originates *)
}

let push_id ist id v = { ist with env_ist = Id.Map.add id v ist.env_ist }

let push_name ist id v = match id with
| Anonymous -> ist
| Name id -> push_id ist id v

let get_var ist id =
  try (Id.Map.find id ist.env_ist).e with Not_found ->
    anomaly (str "Unbound variable " ++ Id.print id)

let get_ref ist kn =
  try
    let data = Tac2env.interp_global kn in
    data.Tac2env.gdata_expr
  with Not_found ->
    anomaly (str "Unbound reference" ++ KerName.print kn)

let return = Proofview.tclUNIT

exception NoMatch

let match_ctor_against ctor v =
  let v = v.e in
  match ctor, v with
  | { cindx = Open ctor }, ValOpn (ctor', vs) ->
    if KerName.equal ctor ctor' then vs
    else raise NoMatch
  | { cindx = Open _ }, _ -> assert false
  | { cnargs = 0; cindx = Closed i }, ValInt i' ->
    if Int.equal i i' then [| |]
    else raise NoMatch
  | { cnargs = 0; cindx = Closed _ }, ValBlk _ -> raise NoMatch
  | _, ValInt _ -> raise NoMatch
  | { cindx = Closed i }, ValBlk (i', vs) ->
    if Int.equal i i' then vs
    else raise NoMatch
  | { cindx = Closed _ }, ValOpn _ -> assert false
  | _, (ValStr _ | ValCls _ | ValExt _) -> assert false

let check_atom_against atm v =
  match atm, v with
  | AtmInt n, ValInt n' -> if not (Int.equal n n') then raise NoMatch
  | AtmStr s, ValStr s' -> if not (String.equal s (Bytes.unsafe_to_string s')) then raise NoMatch
  | (AtmInt _ | AtmStr _), _ -> assert false

let rec match_pattern_against ist pat v =
  match pat with
  | GPatVar (x,t) ->
    push_name ist x {e=v.e; t}
  | GPatAtm atm -> check_atom_against atm v.e; ist
  | GPatAs (p,x) -> match_pattern_against (push_name ist (Name x) v) p v
  | GPatRef (ctor,pats) ->
    let vs = match_ctor_against ctor v in
    List.fold_left_i (fun i ist pat -> match_pattern_against ist pat {e=vs.(i); t=None } ) 0 ist pats
  | GPatOr pats -> match_pattern_against_or ist pats v

and match_pattern_against_or ist pats v =
  match pats with
  | [] -> raise NoMatch
  | pat :: pats ->
    try match_pattern_against ist pat v
    with NoMatch -> match_pattern_against_or ist pats v

let eval_glb_ext ist (Tac2dyn.Arg.Glb (tag,e)) =
  let tpe = Tac2env.interp_ml_object tag in
  with_frame (FrExtn (tag, e)) (tpe.Tac2env.ml_interp ist e)

let rec interp (ist : environment) = function
| GTacAtm (AtmInt n) -> return (Tac2ffi.of_int n)
| GTacAtm (AtmStr s) -> return (Tac2ffi.of_string s)
| GTacVar id -> return (get_var ist id)
| GTacRef kn ->
  begin match Tac2env.get_compiled_global kn with
  | Some (_info,v) -> return v
  | None ->
    let data = get_ref ist kn in
    return (eval_pure ist Id.Map.empty (Some kn) data)
  end
| GTacFun (ids, ts, e) ->
  let cls = { clos_ref = None; clos_env = ist.env_ist; clos_var = ids; clos_types=ts; clos_exp = e } in
  let f = interp_closure ist cls in
  return f
| GTacAls (e, loc, fn) ->
  let ist1 =
    if DebugCommon.get_debug () then
      { ist with locs = push_locs loc ist;
        stack = push_stack (fn, loc) ist;
        varmaps = ist.env_ist :: ist.varmaps }
    else ist
  in
  DebugCommon.save_goals loc (fun () -> maybe_stop ist loc) () >>=
  (fun () -> interp ist1 e)
| GTacApp (f, args, loc) ->
  let step = step_GTacApp ist f args loc in
  step >>= fun f ->
    Proofview.Monad.List.map (fun e -> interp ist e) args >>=
    DebugCommon.save_goals loc (fun () -> maybe_stop ist loc) >>=
    fun args -> Tac2ffi.apply (Tac2ffi.to_closure f) args
| GTacLet (false, el, e) ->
  let fold accu (na, e, t) =
    interp ist e >>= fun e ->
    return (push_name accu na {e; t})
  in
  Proofview.Monad.List.fold_left fold ist el >>= fun ist ->
  interp ist e
| GTacLet (true, el, e) ->
  let map (na, e, t) = match e with
  | GTacFun (ids, ts, e) ->
    let cls = { clos_ref = None; clos_env = ist.env_ist; clos_var = ids; clos_types=ts; clos_exp = e } in
    let f = interp_closure ist cls in
    na, cls, {e=f; t=None }
  | _ -> anomaly (str "Ill-formed recursive function")
  in
  let fixs = List.map map el in
  let fold accu (na, _, cls) = match na with
  | Anonymous -> accu
  | Name id -> { accu with env_ist = Id.Map.add id cls accu.env_ist }
  in
  let ist = List.fold_left fold ist fixs in
  (* Hack to make a cycle imperatively in the environment *)
  let iter (_, e, _) = e.clos_env <- ist.env_ist in
  let () = List.iter iter fixs in
  interp ist e
| GTacCst (_, n, []) -> return (Valexpr.make_int n)
| GTacCst (_, n, el) ->
  Proofview.Monad.List.map (fun e -> interp ist e) el >>= fun el ->
  return (Valexpr.make_block n (Array.of_list el))
| GTacCse (e, _, cse0, cse1) ->
  interp ist e >>= fun e -> interp_case ist e cse0 cse1
| GTacWth { opn_match = e; opn_branch = cse; opn_default = def } ->
  interp ist e >>= fun e -> interp_with ist e cse def
| GTacFullMatch (e,brs) ->
  interp ist e >>= fun e -> interp_full_match ist e brs
| GTacPrj (_, e, p) ->
  interp ist e >>= fun e -> interp_proj ist e p
| GTacSet (_, e, p, r) ->
  interp ist e >>= fun e ->
  interp ist r >>= fun r ->
  interp_set ist e p r
| GTacOpn (kn, el) ->
  Proofview.Monad.List.map (fun e -> interp ist e) el >>= fun el ->
  return (Tac2ffi.of_open (kn, Array.of_list el))
| GTacPrm ml ->
  return (Tac2env.interp_primitive ml)
| GTacExt (tag, e) -> eval_glb_ext ist (Glb (tag,e))

and step_GTacApp ist f args loc =
  let fname = match f with
  | GTacRef kn -> KerName.to_string kn
  | GTacExt (tag,_) -> (Tac2dyn.Arg.repr tag)   (* for ltac1val: *)
  | _ -> "???"
  in
  let is_primitive kn =
    match f with
    | GTacRef kn ->
      (match get_ref ist kn with
      | GTacFun (_, _, GTacPrm _) -> true
      | _ -> false)
    | _ -> false
  in
  let ist =
    if DebugCommon.get_debug () && (not (is_primitive fname)) then
      { ist with locs = push_locs loc ist;
        stack = push_stack (fname, loc) ist;
        varmaps = ist.env_ist :: ist.varmaps;
        prev_chunks = ist.prev_chunks }
    else ist
  in
  interp ist f

and interp_closure ist0 f =
  let ans = fun args ->
    let { clos_env = ist; clos_var = ids; clos_exp = e; clos_types = ts; clos_ref = kn } = f in
    let frame = match kn with
    | None -> FrAnon e
    | Some kn -> FrLtac kn
    in
    let ist = { ist0 with env_ist = ist } in
    let args = match ts with
    | Some ts -> List.map2 (fun e t -> { e; t=Some t}) args ts
    | None -> List.map (fun e -> { e; t=None }) args
    in
    let ist = List.fold_left2 push_name ist ids args in
    with_frame frame (interp ist e)
  in
  Tac2ffi.(of_closure (abstract (List.length f.clos_var) ans))

and interp_case ist e cse0 cse1 =
  if Valexpr.is_int e then
    interp ist cse0.(Tac2ffi.to_int e)
  else
    let (n, args) = Tac2ffi.to_block e in
    let (ids, e) = cse1.(n) in
    let ist = CArray.fold_left2 (fun ist (id,t) e -> push_name ist id { e; t }) ist ids args in
    interp ist e

and interp_with ist e cse def =
  let (kn, args) = Tac2ffi.to_open e in
  let br = try Some (KNmap.find kn cse) with Not_found -> None in
  begin match br with
  | None ->
    let (self, def) = def in
    let ist = push_name ist self { e; t=None } in
    interp ist def
  | Some (self, ids, p) ->
    let ist = push_name ist self { e; t=None } in
    let args = Array.map (fun e -> { e; t=None }) args in
    let ist = CArray.fold_left2 push_name ist ids args in
    interp ist p
  end

and interp_full_match ist e = function
  | [] -> CErrors.anomaly Pp.(str "ltac2 match not exhaustive")
  | (pat,br) :: rest ->
    begin match match_pattern_against ist pat { e; t=None } with
    | exception NoMatch -> interp_full_match ist e rest
    | ist -> interp ist br
    end

and interp_proj ist e p =
  return (Valexpr.field e p)

and interp_set ist e p r =
  let () = Valexpr.set_field e p r in
  return (Valexpr.make_int 0)

and eval_pure ist bnd kn = function
| GTacVar id -> Id.Map.get id bnd
| GTacAtm (AtmInt n) -> Valexpr.make_int n
| GTacRef kn ->
  begin match Tac2env.get_compiled_global kn with
  | Some (_info,v) -> v
  | None ->
    let { Tac2env.gdata_expr = e } =
      try Tac2env.interp_global kn
      with Not_found -> assert false
    in
    eval_pure ist bnd (Some kn) e
  end
| GTacFun (na, ts, e) ->
  let bnd = Id.Map.map (fun e -> { e; t=None }) bnd in
  let cls = { clos_ref = kn; clos_env = bnd; clos_var = na; clos_types=ts; clos_exp = e } in
  interp_closure ist cls
| GTacCst (_, n, []) -> Valexpr.make_int n
| GTacCst (_, n, el) -> Valexpr.make_block n (eval_pure_args ist bnd el)
| GTacOpn (kn, el) -> Tac2ffi.of_open (kn, eval_pure_args ist bnd el)
| GTacLet (isrec, vals, body) ->
  let () = assert (not isrec) in
  let fold accu (na, e, t) = match na with
  | Anonymous ->
    (* No need to evaluate, we know this is a value *)
    accu
  | Name id ->
    let v = eval_pure ist bnd None e in
    Id.Map.add id v accu
  in
  let bnd = List.fold_left fold bnd vals in
  eval_pure ist bnd kn body

| GTacPrm ml -> Tac2env.interp_primitive ml

| GTacAtm (AtmStr _) | GTacSet _
| GTacApp _ | GTacCse _ | GTacPrj _
| GTacExt _ | GTacWth _
| GTacFullMatch _ | GTacAls _ ->
  anomaly (Pp.str "Term is not a syntactical value")

and eval_pure_args ist bnd args =
  let map e = eval_pure ist bnd None e in
  Array.map_of_list map args

let interp_value ist tac =
  let env = Id.Map.map (fun e -> e.e) ist.env_ist in
  eval_pure ist env None tac

(* todo: this is unused, OK to remove? *)
(* let eval_global kn = eval_pure ist (Id.Map.empty) (Some kn) (Tac2env.interp_global kn).gdata_expr *)

(** Cross-boundary hacks. *)

open Geninterp

let val_env : environment Val.typ = Val.create "ltac2:env"
let env_ref = Id.of_string_soft "@@ltac2_env@@"

let extract_env (Val.Dyn (tag, v)) : environment =
match Val.eq tag val_env with
| None -> assert false
| Some Refl -> v

let get_env ist =
  try extract_env (Id.Map.find env_ref ist)
  with Not_found -> empty_environment ()

let set_env env ist =
  Id.Map.add env_ref (Val.Dyn (val_env, env)) ist
