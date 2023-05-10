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

exception LtacError = Tac2ffi.LtacError

let backtrace : backtrace Evd.Store.field = Evd.Store.field ()

let print_ltac2_backtrace = ref false

let get_backtrace =
  Proofview.tclEVARMAP >>= fun sigma ->
  match Evd.Store.get (Evd.get_extra_data sigma) backtrace with
  | None -> Proofview.tclUNIT []
  | Some bt -> Proofview.tclUNIT bt

let set_backtrace bt =
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = Evd.get_extra_data sigma in
  let store = Evd.Store.set store backtrace bt in
  let sigma = Evd.set_extra_data store sigma in
  Proofview.Unsafe.tclEVARS sigma

let with_frame frame tac =
  if !print_ltac2_backtrace then
    get_backtrace >>= fun bt ->
    set_backtrace (frame :: bt) >>= fun () ->
    tac >>= fun ans ->
    set_backtrace bt >>= fun () ->
    Proofview.tclUNIT ans
  else tac

type environment = Tac2env.environment = {
  env_ist : valexpr Id.Map.t;
  (* todo: mutable needed? *)
  (* location of next code to execute, is not in stack *)
  mutable cur_loc : Loc.t option;
  (* yields the call stack *)
  mutable stack : (string * Loc.t option) list option;
  (* variable value maps for each stack frame *)
  mutable varmaps : valexpr Id.Map.t list;
}  (* todo: don't need to repeat definition here *)

let empty_environment () = {
  env_ist = Id.Map.empty;
  cur_loc = None;
  stack = if DebugCommon.get_debug () then Some [] else None;
  varmaps = [Id.Map.empty];
}

type closure = {
  mutable clos_env : valexpr Id.Map.t;
  (** Mutable so that we can implement recursive functions imperatively *)
  clos_var : Name.t list;
  (** Bound variables *)
  clos_exp : glb_tacexpr;
  (** Body *)
  clos_ref : ltac_constant option;
  (** Global constant from which the closure originates *)
}

let push_name ist id v = match id with
| Anonymous -> ist
| Name id -> { ist with env_ist = Id.Map.add id v ist.env_ist }

let get_var ist id =
  try Id.Map.find id ist.env_ist with Not_found ->
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
  | _, (ValStr _ | ValCls _ | ValExt _ | ValUint63 _ | ValFloat _) -> assert false

let check_atom_against atm v =
  match atm, v with
  | AtmInt n, ValInt n' -> if not (Int.equal n n') then raise NoMatch
  | AtmStr s, ValStr s' -> if not (String.equal s (Bytes.unsafe_to_string s')) then raise NoMatch
  | (AtmInt _ | AtmStr _), _ -> assert false

let rec match_pattern_against ist pat v =
  match pat with
  | GPatVar x -> push_name ist x v
  | GPatAtm atm -> check_atom_against atm v; ist
  | GPatAs (p,x) -> match_pattern_against (push_name ist (Name x) v) p v
  | GPatRef (ctor,pats) ->
    let vs = match_ctor_against ctor v in
    List.fold_left_i (fun i ist pat -> match_pattern_against ist pat vs.(i)) 0 ist pats
  | GPatOr pats -> match_pattern_against_or ist pats v

and match_pattern_against_or ist pats v =
  match pats with
  | [] -> raise NoMatch
  | pat :: pats ->
    try match_pattern_against ist pat v
    with NoMatch -> match_pattern_against_or ist pats v

type debugger_state = {
  (* location of next code to execute, is not in stack *)
  mutable cur_loc : Loc.t option;
  (* yields the call stack *)
  mutable stack : (string * Loc.t option) list;
  (* variable value maps for each stack frame *)
  mutable varmaps : Tac2ffi.valexpr Names.Id.Map.t list;
}

let debugger_state = { cur_loc=None; stack=[]; varmaps=[] }

let get_stack () = DebugCommon.get_stack2 debugger_state.stack debugger_state.cur_loc (* todo *)

let to_str = function (* can't get Tac2ffi.to_pp to work *)
| ValInt i -> string_of_int i
| ValStr bytes -> Printf.sprintf "\"%s\"" (Bytes.to_string bytes)  (* no quote escaping *)
| ValUint63 ui -> Uint63.to_string ui
| ValFloat f -> Float64.to_string f
| ValBlk _ -> "ValBlk ???"  (* todo *)
| ValCls _ -> "ValCls ???"
| ValOpn (kn, args) -> Printf.sprintf "ValOpn %s [???]" (KerName.to_string kn)
| ValExt _ -> "ValExt ???"

let get_vars framenum =
  let open Names in
(*  Printf.eprintf "framenum = %d varmaps len = %d\n%!" framenum (List.length debugger_state.varmaps);*)
  let vars = List.nth debugger_state.varmaps framenum in
  List.map (fun b ->
      let (id, v) = b in
(*      (Id.to_string id, Tac2print.pr_valexpr (Global.env ()) vars v ???)*)
      (Id.to_string id, Pp.str (to_str v))
    ) (Id.Map.bindings vars)


let rec read_loop () =
  let nl = if (*Util.(!batch)*) false then "\n" else "" in
(*  Comm.print_deferred () >>*)
  let hook = Option.get (DebugHook.Intf.get ()) in
  hook.submit_answer (Prompt (tag "message.prompt" @@ fnl () ++ str ("TcDebug > " ^ nl)));
(*  Printf.eprintf "read_loop\n%!";*)
  DebugCommon.action := DebugCommon.read get_stack get_vars;
(*    (fun () -> (*Printf.eprintf "get_stack\n%!";*)*)
(*        hook.submit_answer (Stack (DebugCommon.format_stack (get_stack ()))))*)
(*    (fun n -> Printf.eprintf "get_vars\n%!"; []);*)
  let open DebugHook.Action in
  match !DebugCommon.action with
  | Continue
  | StepIn
  | StepOver
  | StepOut -> ()
  | Skip -> failwith "Skip not in Ltac2"
  | Interrupt -> failwith "Interrupt"  (* todo: exit the debugger, for Ctrl C *)
  | Help -> failwith "Help not in Ltac2"
  | UpdBpts updates -> failwith "UpdBpts"  (* handled in init() loop *)
  | Configd -> failwith "Configd" (* handled in init() loop *)
  | GetStack -> failwith "GetStack" (* handled in read() loop *)
  | GetVars _ -> failwith "GetVars" (* handled in read() loop *)
  | RunCnt num -> failwith "RunCnt not in Ltac2"
  | RunBreakpoint s -> failwith "RunBreakpoint not in Ltac2"
  | Command _ -> failwith "Command"  (* not possible *)
  | Failed -> read_loop ()
  | Ignore -> failwith "Ignore" (* not possible *)

let rec dump_expr2 ?(indent=0) e =
  let printloc loc =
    let loc = match loc with
    | None -> "None"
    | Some loc -> Pp.string_of_ppcmds (Loc.pr loc)
    in
    Printf.eprintf "loc =  %s\n%!" loc
  in
  let print s = Printf.eprintf "%s\n" s in
  Printf.eprintf "%s" (String.make indent ' ');
  let indent = indent + 2 in
  match e with
  | GTacAtm _ -> print "GTacAtm"
  | GTacVar _ -> print "GTacVar"
  | GTacRef kn -> print "GTacRef";
    Printf.eprintf "%s> %s\n%!" (String.make indent ' ') (Names.KerName.to_string kn);
  | GTacFun (_, e) -> print "GTacFun";
    dump_expr2 ~indent e
  | GTacApp (e, el, loc) -> print (Printf.sprintf "GTacApp el len = %d" (List.length el));
    printloc loc;
    dump_expr2 ~indent e
  | GTacLet (_, _, e) -> print "GTacLet";
    dump_expr2 ~indent e
  | GTacCst _ -> print "GTacCst"
  | GTacCse _ -> print "GTacCse"
  | GTacPrj _ -> print "GTacPrj"
  | GTacSet _ -> print "GTacSet"
  | GTacOpn _ -> print "GTacOpn"
  | GTacWth _ -> print "GTacWth"
  | GTacFullMatch _ -> print "GTacFullMatch"
  | GTacExt _ -> print "GTacExt"
  | GTacPrm  _ -> print "GTacPrm"

[@@@ocaml.warning "-32"]
let getname e =
  match e with
  | GTacAtm _ -> "GTacAtm"
  | GTacVar _ -> "GTacVar"
  | GTacRef kn -> "GTacRef"
  | GTacFun (_, e) -> "GTacFun"
  | GTacApp (e, el, loc) -> "GTacApp"
  | GTacLet (_, _, e) -> "GTacLet"
  | GTacCst _ -> "GTacCst"
  | GTacCse _ -> "GTacCse"
  | GTacPrj _ -> "GTacPrj"
  | GTacSet _ -> "GTacSet"
  | GTacOpn _ -> "GTacOpn"
  | GTacWth _ -> "GTacWth"
  | GTacFullMatch _ -> "GTacFullMatch"
  | GTacExt _ -> "GTacExt"
  | GTacPrm  _ -> "GTacPrm"

let stacks_info stack p_stack =
  let st = Option.default [] stack in
  let st_prev = Option.default [] p_stack in
  let l_cur, l_prev = List.length st, List.length st_prev in
  st, st_prev, l_cur, l_prev

let prev_stack = ref (Some [])  (* previous stopping point in debugger *)

let init () =
  debugger_state.stack <- [];
  prev_stack := Some []

let push_stack item ist =
  match ist with
  | Some s -> Some (item :: s)
  | None -> ist

let rec interp (ist : environment) = function
| GTacAtm (AtmInt n) -> return (Tac2ffi.of_int n)
| GTacAtm (AtmStr s) -> return (Tac2ffi.of_string s)
| GTacVar id -> return (get_var ist id)
| GTacRef kn ->
  let data = get_ref ist kn in
  let rv = return (eval_pure ist Id.Map.empty (Some kn) data) in
  rv
| GTacFun (ids, e) ->
  let cls = { clos_ref = None; clos_env = ist.env_ist; clos_var = ids; clos_exp = e } in
  let f = interp_closure ist cls in
  return f
| GTacApp (f, args, loc) ->
  let indent = 2 in
  if false then dump_expr2 ~indent f;
  let fname = match f with
  | GTacRef kn -> let s = KerName.to_string kn in Printf.eprintf "kn = %s\n%!" s; s
  | _ -> "???"
  in
  let stop = DebugCommon.get_debug () &&
    (DebugCommon.breakpoint_stop loc ||
     DebugCommon.stepping_stop stacks_info ist.stack !prev_stack !DebugCommon.action)
  in
  if stop then begin
    prev_stack := ist.stack;
    debugger_state.cur_loc <- loc;
    debugger_state.stack <- Option.default [] ist.stack;
    debugger_state.varmaps <- ist.env_ist :: ist.varmaps;
  end;
  let ist =
    if DebugCommon.get_debug () then
      { ist with stack = push_stack (fname, loc) ist.stack;
        varmaps = ist.env_ist :: ist.varmaps }
    else ist
  in
  let (>=) = Proofview.tclBIND in
  (if stop then (DebugCommon.db_pr_goals ()) >= fun () -> read_loop (); interp ist f  else  interp ist f)   >>= fun f ->
  Proofview.Monad.List.map (fun e -> interp ist e) args >>= fun args ->
  Tac2ffi.apply (Tac2ffi.to_closure f) args
| GTacLet (false, el, e) ->
  let fold accu (na, e) =
    interp ist e >>= fun e ->
    return (push_name accu na e)
  in
  Proofview.Monad.List.fold_left fold ist el >>= fun ist ->
  interp ist e
| GTacLet (true, el, e) ->
  let map (na, e) = match e with
  | GTacFun (ids, e) ->
    let cls = { clos_ref = None; clos_env = ist.env_ist; clos_var = ids; clos_exp = e } in
    let f = interp_closure ist cls in
    na, cls, f
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
| GTacPrm (ml, el) ->
  Printf.eprintf "GTacPrm %s. %s\n%!" ml.mltac_plugin ml.mltac_tactic;
(*  let fname = match f with*)
(*  | GTacRef kn -> let s = KerName.to_string kn in Printf.eprintf "kn = %s\n%!" s; s*)
(*  | _ -> "???"*)
(*  in*)
(*  let ist = (* in monad or not? *)*)
(*    if DebugCommon.get_debug () then begin*)
(*      { ist with stack = push_stack (fname, loc) ist.stack;*)
(*        varmaps = ist.env_ist :: ist.varmaps }*)
(*    end else ist in*)
  Proofview.Monad.List.map (fun e -> interp ist e) el >>= fun el ->
  with_frame (FrPrim ml) (Tac2ffi.apply (Tac2env.interp_primitive ml) el)
| GTacExt (tag, e) ->
  let tpe = Tac2env.interp_ml_object tag in
  with_frame (FrExtn (tag, e)) (tpe.Tac2env.ml_interp ist e)

and interp_closure ist0 f =
  let ans = fun args ->
    let { clos_env = ist; clos_var = ids; clos_exp = e; clos_ref = kn } = f in
    let frame = match kn with
    | None -> FrAnon e
    | Some kn -> FrLtac kn
    in
    let ist = { ist0 with env_ist = ist } in
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
    let ist = CArray.fold_left2 push_name ist ids args in
    interp ist e

and interp_with ist e cse def =
  let (kn, args) = Tac2ffi.to_open e in
  let br = try Some (KNmap.find kn cse) with Not_found -> None in
  begin match br with
  | None ->
    let (self, def) = def in
    let ist = push_name ist self e in
    interp ist def
  | Some (self, ids, p) ->
    let ist = push_name ist self e in
    let ist = CArray.fold_left2 push_name ist ids args in
    interp ist p
  end

and interp_full_match ist e = function
  | [] -> CErrors.anomaly Pp.(str "ltac2 match not exhaustive")
  | (pat,br) :: rest ->
    begin match match_pattern_against ist pat e with
    | exception NoMatch -> interp_full_match ist e rest
    | ist -> interp ist br
    end

and interp_proj ist e p =
  return (Valexpr.field e p)

and interp_set ist e p r =
  let () = Valexpr.set_field e p r in
  return (Valexpr.make_int 0)

and eval_pure ist bnd kn x = (*Printf.eprintf "enter eval_pure\n%!";*)
let rv = match x with
| GTacVar id -> Id.Map.get id bnd
| GTacAtm (AtmInt n) -> Valexpr.make_int n
| GTacRef kn ->
  let { Tac2env.gdata_expr = e } =
    try Tac2env.interp_global kn (* ?? *)
    with Not_found -> assert false
  in
  eval_pure ist bnd (Some kn) e
| GTacFun (na, e) ->
  let cls = { clos_ref = kn; clos_env = bnd; clos_var = na; clos_exp = e } in
  interp_closure ist cls
| GTacCst (_, n, []) -> Valexpr.make_int n
| GTacCst (_, n, el) -> Valexpr.make_block n (eval_pure_args ist bnd el)
| GTacOpn (kn, el) -> Tac2ffi.of_open (kn, eval_pure_args ist bnd el)
| GTacLet (isrec, vals, body) ->
  let () = assert (not isrec) in
  let fold accu (na, e) = match na with
  | Anonymous ->
    (* No need to evaluate, we know this is a value *)
    accu
  | Name id ->
    let v = eval_pure ist bnd None e in
    Id.Map.add id v accu
  in
  let bnd = List.fold_left fold bnd vals in
  eval_pure ist bnd kn body

| GTacAtm (AtmStr _) | GTacSet _
| GTacApp _ | GTacCse _ | GTacPrj _
| GTacPrm _ | GTacExt _ | GTacWth _
| GTacFullMatch _ ->
  anomaly (Pp.str "Term is not a syntactical value")
in (*Printf.eprintf "exit eval_pure\n%!";*) rv

and eval_pure_args ist bnd args =
  let map e = eval_pure ist bnd None e in
  Array.map_of_list map args

let interp_value ist tac =
  eval_pure ist ist.env_ist None tac

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
