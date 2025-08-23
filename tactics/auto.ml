(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

open Pp
open Util
open Names
open Termops
open Tactics
open Proofview.Notations
open Hints

(**************************************************************************)
(*                           Automatic tactics                            *)
(**************************************************************************)

(**************************************************************************)
(*          tactics with a trace mechanism for automatic search           *)
(**************************************************************************)

let fwd_intern_foreach = ref ((fun x -> failwith "fwd_intern_foreach") :
    Hints.foreach_info -> (Id.t * Id.t) list -> Id.t list ->
    Gentactic.glob_generic_tactic)

let compute_secvars gl =
  let hyps = Proofview.Goal.hyps gl in
  secvars_of_hyps hyps

(* Tell auto not to reuse already instantiated metas in unification (for
   compatibility, since otherwise, apply succeeds more often). *)

open Unification

let auto_core_unif_flags_of st1 st2 = {
  modulo_conv_on_closed_terms = Some st1;
  use_metas_eagerly_in_conv_on_closed_terms = true;
  use_evars_eagerly_in_conv_on_closed_terms = false;
  modulo_delta = st2;
  modulo_delta_types = TransparentState.full;
  check_applied_meta_types = false;
  use_pattern_unification = false;
  use_meta_bound_pattern_unification = true;
  allowed_evars = Evarsolve.AllowedEvars.all;
  restrict_conv_on_strict_subterms = false; (* Compat *)
  modulo_betaiota = false;
  modulo_eta = true;
}

let auto_unif_flags_of st1 st2 =
  let flags = auto_core_unif_flags_of st1 st2 in {
  core_unify_flags = flags;
  merge_unify_flags = flags;
  subterm_unify_flags = { flags with modulo_delta = TransparentState.empty };
  allow_K_in_toplevel_higher_order_unification = false;
  resolve_evars = true
}

let auto_unif_flags =
  auto_unif_flags_of TransparentState.full TransparentState.empty

(* Try unification with the precompiled clause, then use registered Apply. *)

let unify_resolve flags h = Hints.hint_res_pf ~flags h
let unify_resolve_nodelta h = Hints.hint_res_pf ~flags:auto_unif_flags h

let exact h =
  Proofview.Goal.enter begin fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let sigma, c = Hints.fresh_hint env sigma h in
    let sigma, t = Typing.type_of env sigma c in
    let concl = Proofview.Goal.concl gl in
    if occur_existential sigma t || occur_existential sigma concl then
      try
        let _, sigma = Unification.w_unify env sigma CONV ~flags:auto_unif_flags concl t in
        Proofview.Unsafe.tclEVARSADVANCE sigma <*>
        exact_no_check c
      with e when CErrors.noncritical e -> Proofview.tclZERO e
    else Proofview.Unsafe.tclEVARS sigma <*> exact_check c
  end

(* Util *)

(* Would it be possible to compile the tactic first and then perform the
substitution without going through bdize, whose purpose is to prepare a
term for display? (HH) *)

(* If we remove the last argument (gl), conclPattern is calculated once
and for all: in particular, if Pattern.somatch produces a UserError
This means that if the conclusion doesn't match the pattern, Auto fails, even
if after Intros the conclusion matches the pattern.
*)

(* conclPattern must fail with an error because it is caught by tclFIRST *)

let conclPattern concl pat ?(recc=false) tac =
  let constr_bindings env sigma =
    match pat with
    | _ when recc -> Proofview.tclUNIT Id.Map.empty  (* bindings for rec patterns not implemented *)
    | None -> Proofview.tclUNIT Id.Map.empty
    | Some pat ->
        try
          Proofview.tclUNIT (Constr_matching.matches env sigma pat concl)
        with Constr_matching.PatternMatchingFailure as exn ->
          let _, info = Exninfo.capture exn in
          Tacticals.tclZEROMSG ~info (str "pattern-matching failed")
  in
  Proofview.Goal.enter begin fun gl ->
     let env = Proofview.Goal.env gl in
     let sigma = Proofview.Goal.sigma gl in
     constr_bindings env sigma >>= fun constr_bindings ->
     let inj c = Geninterp.Val.inject (Geninterp.val_tag (Genarg.topwit Stdarg.wit_constr)) c in
     let fold id c accu = Id.Map.add id (inj c) accu in
     let lfun = Id.Map.fold fold constr_bindings Id.Map.empty in
     Gentactic.interp ~lfun tac
  end

(***********************************************************)
(** A debugging / verbosity framework for trivial and auto *)
(***********************************************************)

(** The following options allow to trigger debugging/verbosity
    without having to adapt the scripts.
    Note: if Debug and Info are both activated, Debug take precedence. *)

let global_debug_trivial = ref false
let global_debug_auto = ref false
let global_info_trivial = ref false
let global_info_auto = ref false

let add_option ls refe =
  Goptions.(declare_bool_option
    { optstage = Summary.Stage.Interp;
      optdepr  = None;
      optkey   = ls;
      optread  = (fun () -> !refe);
      optwrite = (:=) refe })

let () =
  add_option ["Debug";"Trivial"] global_debug_trivial;
  add_option ["Debug";"Auto"] global_debug_auto;
  add_option ["Info";"Trivial"] global_info_trivial;
  add_option ["Info";"Auto"] global_info_auto

type debug_kind = ReportForTrivial | ReportForAuto

let no_dbg (_,whatfor,_,_) = (Off,whatfor,0,ref [])

let mk_trivial_dbg debug =
  let d =
    if debug == Debug || !global_debug_trivial then Debug
    else if debug == Info || !global_info_trivial then Info
    else Off
  in (d,ReportForTrivial,0,ref [])

let mk_auto_dbg debug =
  let d =
    if debug == Debug || !global_debug_auto then Debug
    else if debug == Info || !global_info_auto then Info
    else Off
  in (d,ReportForAuto,0,ref [])

let incr_dbg = function (dbg,whatfor,depth,trace) -> (dbg,whatfor,depth+1,trace)

module PSHash = Hashtbl.Make(struct
        type t = (EConstr.named_context * EConstr.constr) list
        let equal = (=)
        let hash o = Hashtbl.hash_param 256 256 o
      end)
let pshash = PSHash.create 13

type stats = { tries: int; successes: int; dups: int; goals: int; time: float }
(* success = non-duplicate successes *)

let init_stats = {tries=0; successes=0; dups=0; goals=0; time=0.}

let auto_stats = ref init_stats

module HintCounts = Map.Make(String)
let hintCounts = ref HintCounts.empty

let get_counts tacstr =
  try HintCounts.find tacstr !hintCounts
  with Not_found -> init_stats

(* exception when the proof state has already been seen in the "auto" search *)
exception DuplicateProofState

let get_time () =
  let open Unix in
(* gettimeofday() *)
  let t = times () in
  t.tms_utime +. t.tms_stime

let proof_info = Evd.Store.field "proof_info"

let save_proof_info sigma info =
  let open Evd in
  Proofview.tclEVARMAP >>= fun sigma ->
  let store = get_extra_data sigma in
  let oinfo = match Store.get store proof_info with
    | None -> []
    | Some info -> info
  in
  let store = Store.set store proof_info (info :: oinfo) in
  Proofview.Unsafe.tclEVARS (set_extra_data store sigma)

let get_proof_info sigma =
  let open Evd in
  let store = get_extra_data sigma in
  let info = Store.get store proof_info in
(*  let len = match info with | None -> 0 | Some l -> List.length l in *)
(*  Printf.eprintf "length of info = %d\n%!" len; *)
  info

(** A tracing tactic for debug/info trivial/auto *)
let tclLOG (dbg,pr,depth,trace) pp tac =
  (* TODO: check hashcode for non-info cases *)
    let newhash goals pp =
    let pp = match goals with
    | gl :: _ -> pp (Proofview.Goal.env gl) (Proofview.Goal.sigma gl);
    | _ -> Pp.mt ()
    in
    if (List.length goals) = 0 then begin
      let _ = pp in
(*      if CList.test then Printf.eprintf "applied %s\n%!" (Pp.string_of_ppcmds pp); *)
      true, 0
    end else begin
      let key = List.fold_left (fun acc g -> (Proofview.Goal.hyps g, Proofview.Goal.concl g) :: acc) [] goals in
      let hash = Hashtbl.hash_param 256 256 key in
      let is_new = not (PSHash.mem pshash key) in
      if is_new then PSHash.replace pshash key ();
(*      if CList.test then Printf.eprintf "newhash %b %d %d %d %s\n%!" is_new hash !successes !dups (Pp.string_of_ppcmds pp); *)
      is_new, hash
    end
  in
  let tacstr = ref "" in
  match dbg with
    | Off ->
      Proofview.(tclIFCATCH
        (tac >>= fun v ->
          auto_stats := { !auto_stats with tries=(!auto_stats.tries+1); successes=(!auto_stats.successes+1) };
          tclUNIT v)
        tclUNIT
        (fun (exn, info) ->
            auto_stats := { !auto_stats with tries=(!auto_stats.tries+1) };
            tclZERO ~info exn) )
    | Debug ->
      (* For "debug (trivial/auto)", we directly output messages *)
      let s = String.make (depth+1) '*' in
      Proofview.(tclIFCATCH (
          Proofview.Goal.goals >>=
          fun gl -> Monad.List.map (fun x -> x) gl >>= fun from_goals ->
          tclEVARMAP >>= fun sigma0 ->
          tclENV >>= fun env0 ->
          let _ = get_proof_info sigma0 in
          tac >>= fun v ->
          tclENV >>= fun env ->
          tclEVARMAP >>= fun sigma ->
          save_proof_info sigma "x" >>= fun () ->
          Proofview.Goal.goals >>=
          fun gl -> Monad.List.map (fun x -> x) gl >>= fun goals ->
          Feedback.msg_notice (str s ++ spc () ++ pp env sigma ++ str ". (*success*)");
          auto_stats := { !auto_stats with tries=(!auto_stats.tries+1); successes=(!auto_stats.successes+1) };
          if from_goals <> [] then begin
            let from_goal = (Proofview.Goal.goal (List.hd from_goals)) in
            Feedback.msg_notice (str "From " ++ int (Evar.repr from_goal) ++ str " " ++
              (Printer.pr_econstr_env env0 sigma0 (Proofview.Goal.concl (List.hd from_goals))));
            List.iteri (fun gnum gl -> Feedback.msg_notice (str "Goal " ++ int (gnum+1) ++ str ": "
              ++ int (Evar.repr (Proofview.Goal.goal gl)) ++ str " " ++
              (Printer.pr_econstr_env env sigma (Proofview.Goal.concl gl)) )) goals;
          end;
          tclUNIT v
        ) tclUNIT
          (fun (exn, info) ->
             tclENV >>= fun env ->
             tclEVARMAP >>= fun sigma ->
             Feedback.msg_notice (str s ++ spc () ++ pp env sigma ++ str ". (*fail*)");
             auto_stats := { !auto_stats with tries=(!auto_stats.tries+1) };
             tclZERO ~info exn))
    | Info ->
      let env = Global.env () in
      let sigma = Evd.from_env env in
      tacstr := Pp.string_of_ppcmds (pp env sigma);
      auto_stats := { !auto_stats with tries=(!auto_stats.tries+1) };
      let counts = get_counts !tacstr in
(*      let saved_tries = counts.tries+1 in *)
      hintCounts := HintCounts.add !tacstr { counts with tries = counts.tries+1} !hintCounts;
      (* For "info (trivial/auto)", we store a log trace *)
      let from_gls = ref [] in
      let concl = ref [] in
      let goals_to_ints gls =
        List.map (fun gl -> Evar.repr (Proofview.Goal.goal gl)) gls
      in
      let start = ref 0. in
      Proofview.(tclIFCATCH (
          Proofview.tclEVARMAP >>= fun sigma ->
          Proofview.Goal.goals >>=
          fun gl -> Monad.List.map (fun x -> x) gl >>= fun goals ->
            (* to suppress duplicate plus_comm when starting a new subgoal
            let (_,hashbefore) = newhash goals pp in
            *)
            from_gls := goals_to_ints goals;
(*            begin try *)
(*              let _ = Str.search_forward (Str.regexp "simple apply plus_Sn_m") tacstr 0 in *)
(*              let concl = Proofview.Goal.concl (List.hd goals) in *)
(*              let pc = Printer.pr_econstr_env env sigma concl in *)
(*              Feedback.msg_notice (int saved_tries ++ spc () ++ str tacstr ++ fnl () ++ str "goal is " ++ pc) *)
(*            with Not_found -> (); *)
(*            end; *)
          concl := if goals = [] then [] else [Proofview.Goal.concl (List.hd goals)];
          let _ = get_proof_info sigma in
          start := get_time ();
          tac >>= fun v ->
          let delta_t = get_time () -. !start in
          save_proof_info sigma "x" >>= fun () ->
          Proofview.Goal.goals >>=
          fun gl ->
            let numgoals = List.length gl in
            if numgoals != 1 then PSHash.reset pshash;  (* start over *)
            let fst = ref true in  (* ICK *)
            let is_new = ref false in
            let hash = ref 0 in
            Monad.List.map (fun x -> x) gl >>= fun goals ->
              if !fst then begin
                fst := false;
                let (n,h) = newhash goals pp in
                is_new := n;
                hash := h
              end;
              let pp env sigma =
                pp env sigma ++ Pp.spc () (* ++ Pp.int !hash *)
              in
              if !is_new then begin
                auto_stats := { !auto_stats with successes=(!auto_stats.successes+1); goals=(!auto_stats.goals+numgoals);
                  time = !auto_stats.time +. delta_t };
                let counts = get_counts !tacstr in
                hintCounts := HintCounts.add !tacstr { counts with
                    successes = counts.successes+1; goals = counts.goals+numgoals;
                    time = counts.time +. delta_t }
                    !hintCounts;
                trace := (depth, Some pp, !from_gls, goals_to_ints goals) :: !trace;
                tclUNIT v
              end else begin
  (*              if CList.test then Printf.eprintf "DuplicateProofState\n%!"; *)
                auto_stats := { !auto_stats with dups=(!auto_stats.dups+1);
                  time = !auto_stats.time +. delta_t };
                let counts = get_counts !tacstr in
                hintCounts := HintCounts.add !tacstr { counts with dups = counts.dups+1;
                time = counts.time +. delta_t } !hintCounts;
                tclZERO DuplicateProofState
              end
      ) Proofview.tclUNIT
        (fun (exn, info) ->
          let delta_t = get_time () -. !start in
          auto_stats := { !auto_stats with time = !auto_stats.time +. delta_t };
          let counts = get_counts !tacstr in
          hintCounts := HintCounts.add !tacstr { counts with time = counts.time +. delta_t } !hintCounts;
          begin match exn with
          | DuplicateProofState -> ()
          | _ ->
            let tacs_regexp = Str.regexp (String.concat {|\||} [
(*              "simple apply mult_n_Sm"; *)
              "dont match this";
              ]) in
            let exnstr = Pp.string_of_ppcmds (CErrors.print exn) in
            if (try let _ = Str.search_forward tacs_regexp !tacstr 0 in true with Not_found -> false) then begin
              Printf.eprintf "exception in '%s': %s\n%!" !tacstr exnstr;
              if !concl <> [] then Printf.eprintf "concl is %s\n%!"
                (Pp.string_of_ppcmds (Printer.pr_leconstr_env env sigma (List.hd !concl)));
            end;
            ()
          end;
          tclZERO ~info exn))

let format_trace ?(indent=0) ?(bullets=[]) env sigma trace =
  (* A goal may appear in from_gls for multiple trace entries.
     Use the last entry in the map. *)
  let map = List.fold_left (fun map (_,pp,from_gls,to_gls) ->
      match pp with
      | None -> map
      | Some pp ->
        List.fold_left (fun map from_gl ->
            Int.Map.add from_gl (to_gls,pp) map
          ) map from_gls
    ) Int.Map.empty trace
  in

  let rec dfs indent ?(bulletnum=(1,3,0)) ?(bulletmap=Int.Map.empty) from_gl =
    (* get the next bullet that's not in bullets *)
    let next_bullet (dig,lim,n) =
      let rec int_to_bullet ?(rv=[]) (dig,lim,n) =
        if dig = 0 then String.concat "" rv
        else
          let rv = String.make 1 ("-+*".[n - 3*(n/3)]) :: rv in
          int_to_bullet (dig-1,lim,n/3) ~rv
      in
      let next (dig,lim,n) =
        let n = n + 1 in
        if lim = n then dig+1,lim*3,0
        else dig,lim,n
      in
      let rec aux n =
        let bullet = int_to_bullet n in
        if List.mem bullet bullets then aux (next n) else bullet ^ " ", (next n)
      in
      aux bulletnum
    in
    let to_gls,pp = Int.Map.find from_gl map in
    let bullet = try (Int.Map.find from_gl bulletmap) with Not_found -> "" in
    let nindent = if bullet <> "" then indent + 2 else indent in
    let nindent, bulletnum, bulletmap =
      match List.length to_gls with
      | 0 -> nindent-2, bulletnum, bulletmap
      | 1 -> nindent, bulletnum, bulletmap
      | _ ->
        let nbullet, bulletnum = next_bullet bulletnum in
        nindent, bulletnum, List.fold_left (fun acc to_gl -> Int.Map.add to_gl nbullet acc) bulletmap to_gls
    in

    let indentstr = (String.make indent ' ') in
    Feedback.msg_notice (str indentstr ++ str bullet ++ pp env sigma);
    List.iter (fun to_gl -> dfs nindent ~bulletnum ~bulletmap to_gl) to_gls;
  in
  match trace with
  (* should always be a single item in from_gls *)
  | (_,_,[from_gl],_) :: _ -> dfs indent from_gl
  | _ -> failwith "format_trace"

let pr_info_trace env sigma trace =
  match trace with
  | (Info,_,_,{contents}) ->
    format_trace env sigma (List.rev contents)
  | _ -> ()

let pr_info_nop = function
  | (Info,_,_,_) -> Feedback.msg_notice (str "idtac.")
  | _ -> ()

let pr_dbg_header = function
  | (Off,_,_,_) -> ()
  | (Debug,ReportForTrivial,_,_) -> Feedback.msg_notice (str "(* debug trivial: *)")
  | (Debug,ReportForAuto,_,_) -> Feedback.msg_notice (str "(* debug auto: *)")
  | (Info,ReportForTrivial,_,_) -> Feedback.msg_notice (str "(* info trivial: *)")
  | (Info,ReportForAuto,_,_) -> Feedback.msg_notice (str "(* info auto: *)")

let tclTRY_dbg d tac =
  let delay f = Proofview.tclUNIT () >>= fun () -> f () in
  let tac =
    delay (fun () -> pr_dbg_header d; tac) >>= fun () ->
      Proofview.tclENV >>= fun env ->
      Proofview.tclEVARMAP >>= fun sigma ->
      pr_info_trace env sigma d;
      Proofview.tclUNIT () in
  let after = delay (fun () -> pr_info_nop d; Proofview.tclUNIT ()) in
  Proofview.tclTHEN
    (Proofview.tclORELSE tac (fun (e,info) ->
      (* todo: OK for use by trivial? *)
      match e with
      | Logic_monad.Tac_Timeout -> Proofview.tclZERO ~info e
      | _ -> Proofview.tclUNIT ()))
    after

(**************************************************************************)
(*                           The Trivial tactic                           *)
(**************************************************************************)

(* local_db is a Hint database containing the hypotheses of current goal *)
(* Papageno : cette fonction a été pas mal simplifiée depuis que la base
  de Hint impérative a été remplacée par plusieurs bases fonctionnelles *)

let auto_flags_of_state st =
  auto_unif_flags_of TransparentState.full st

let hintmap_of env sigma secvars hdc concl =
  match hdc with
  | None -> Hint_db.map_none ~secvars
  | Some hdc ->
      if occur_existential sigma concl then
        (fun db -> match Hint_db.map_eauto env sigma ~secvars hdc concl db with
                   | ModeMatch (_, l) -> l
                   | ModeMismatch -> [])
      else Hint_db.map_auto ~auto:true env sigma ~secvars hdc concl

let exists_evaluable_reference env = function
  | Evaluable.EvalConstRef _ -> true
  | Evaluable.EvalProjectionRef _ -> true
  | Evaluable.EvalVarRef v -> try ignore(Environ.lookup_named v env); true with Not_found -> false

let as_tac (lev,_,_,_) =
  if lev = Info then str "." else mt ()

let dbg_intro dbg = tclLOG dbg (fun _ _ -> str "intro" ++ (as_tac dbg)) intro
let dbg_assumption dbg = tclLOG dbg (fun _ _ -> str "assumption" ++ (as_tac dbg)) assumption

let intro_register dbg kont db =
  Proofview.tclTHEN (dbg_intro dbg) @@
    Proofview.Goal.enter begin fun gl ->
      let extend_local_db decl db =
        let env = Proofview.Goal.env gl in
        let sigma = Proofview.Goal.sigma gl in
        push_resolve_hyp env sigma (Context.Named.Declaration.get_id decl) db
      in
      Tacticals.onLastDecl (fun decl -> kont (extend_local_db decl db))
    end

exception No_match

let rec trivial_fail_db dbg db_list local_db =
  Proofview.tclINDEPENDENT @@
    Tacticals.tclORELSE0 (dbg_assumption dbg) @@
    Tacticals.tclORELSE0 (intro_register dbg (trivial_fail_db dbg db_list) local_db) @@
    Proofview.Goal.enter begin fun gl ->
      let env = Proofview.Goal.env gl in
      let sigma = Proofview.Goal.sigma gl in
      let concl = Proofview.Goal.concl gl in
      let secvars = compute_secvars gl in
      let hdc = try Some (decompose_app_bound sigma concl) with Bound -> None in
      let hintmap = hintmap_of env sigma secvars hdc concl in
      let hinttac = tac_of_hint dbg db_list local_db concl [] in
      (local_db::db_list)
      |> List.map_append (fun db -> try hintmap db with Not_found -> [])
      |> List.filter_map begin fun h ->
           if Int.equal (FullHint.priority h) 0 then
             Some (Tacticals.tclCOMPLETE (hinttac h))
           else None
         end
      |> Tacticals.tclFIRST
    end

and pr_hint ?(vals=[]) dbg h env sigma =
   let (lev,_,_,_) = dbg in
   let forinfo = lev = Info in
   let origin = match FullHint.database h with
    | None -> mt ()
    | Some n -> if forinfo then str "  (* in " ++ str n ++ str " *)"
                  else str " (in " ++ str n ++ str ")"
    in
    FullHint.print ~vals ~forinfo env sigma h ++ origin

and expand_foreach dbg concl v_val h =
  (* Cartesian product *)
  let cprod v_vals =
    let rec aux v_vals rv =
      match v_vals with
      | _ :: l :: tl ->
        let rv2 = ref [] in
        List.iter (fun i ->
            List.iter (fun r -> rv2 := (i :: r) :: !rv2) rv
        ) l;
        aux (l :: tl) (List.rev !rv2)
      | hd :: _ -> rv
      | [] -> []
    in
    let rev = List.rev v_vals in
    aux rev (List.map (fun i -> [i])(List.hd rev))
  in
  let pr_lofl lofl =
    List.iter (fun l -> Printf.eprintf "[";
      List.iter (fun id -> Printf.eprintf "%s " (Id.to_string id)) l;
      Printf.eprintf "] "
    ) lofl;
    Printf.eprintf "\n%!";
  in
  let _ = pr_lofl in

(*  if CList.test then begin *)
(*  	Printf.eprintf "\n*Enter expand_foreach\n%!"; *)
(*    Printf.eprintf "v_val at entry:\n%!"; *)
(*    pr_lofl [List.map (fun (v,vt) -> v) v_val]; *)
(*  end; *)
  match FullHint.repr h with
  | Extern (p, tacast, bnds, recc, saved) ->
  (*      let _ : int = conclPattern concl p ~recc tacast in    unit Proofview.tactic *)
    begin match bnds, v_val with
    | [], _ -> [conclPattern concl p ~recc tacast]
    | _ :: _, [] -> []
    | _ :: _, _ :: _ ->
      let v_vals = List.map (fun (bv, bt) ->
        List.map (fun (v,vartype) -> v)
          (List.filter (fun (v,vt) -> vt = (Id.to_string bt)) v_val)) bnds in
(*      Printf.eprintf "\nv_vals\n%!"; *)
(*      pr_lofl v_vals; *)
      (* if false then pr_lofl (cprod v_vals); *)
      List.map (fun vals ->
          let t' = conclPattern concl p ~recc
                               (!fwd_intern_foreach saved bnds vals) in
          tclLOG dbg (pr_hint dbg ~vals h) (FullHint.run h (fun _ ->
(*            Printf.eprintf "Tactic foreach item vals = %!"; (pr_lofl [vals]); *)
            t'))
        ) (cprod v_vals)
    end
  | _ -> []

and tac_of_hint dbg db_list local_db concl v_val h =
  let pr_hint ?(vals=[]) h env sigma =
   let (lev,_,_,_) = dbg in
   let forinfo = lev = Info in
   let origin = match FullHint.database h with
    | None -> mt ()
    | Some n -> if forinfo then str "  (* in " ++ str n ++ str " *)"
                  else str " (in " ++ str n ++ str ")"
    in
    FullHint.print ~vals ~forinfo env sigma h ++ origin
  in
  let tactic = function
    | Res_pf h -> unify_resolve_nodelta h
    | ERes_pf _ -> Proofview.Goal.enter (fun gl ->
        let info = Exninfo.reify () in
        Tacticals.tclZEROMSG ~info (str "eres_pf"))
    | Give_exact h  -> exact h
    | Res_pf_THEN_trivial_fail h ->
      Tacticals.tclTHEN
        (unify_resolve_nodelta h)
        (* With "(debug) trivial", we shouldn't end here, and
           with "debug auto" we don't display the details of inner trivial *)
        (trivial_fail_db (no_dbg dbg) db_list local_db)
    | Unfold_nth c ->
      Proofview.Goal.enter begin fun gl ->
       if exists_evaluable_reference (Proofview.Goal.env gl) c then
         Tacticals.tclPROGRESS (reduce (Unfold [AllOccurrences,c]) Locusops.onConcl)
       else
         let info = Exninfo.reify () in
         Tacticals.tclFAIL ~info (str"Unbound reference")
       end
    | Extern (p, tacast, bnds, recc, saved) ->
      let rec make_tclOR tacs =
        (* todo: should I use Proofview.tclOR here to propagage Tac_Timeout? *)
        match tacs with
        | t1 :: [] -> t1
        | t1 :: (t2 :: _ as tl) -> Tacticals.tclOR t1 (make_tclOR tl)
        | [] -> Proofview.tclZERO No_match
      in
      (* returning a list here with a List.concat elsewhere fails strangely :-( *)
      make_tclOR (expand_foreach dbg concl v_val h)
    in
    match FullHint.repr h with
    | Extern (_,_,bnds,_,_) when bnds <> [] -> FullHint.run h tactic
    | _ -> tclLOG dbg (pr_hint h) (FullHint.run h tactic)
(** The use of the "core" database can be de-activated by passing
    "nocore" amongst the databases. *)

let gen_trivial ?(debug=Off) lems dbnames =
  Hints.wrap_hint_warning @@
    Proofview.Goal.enter begin fun gl ->
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    let db_list =
      match dbnames with
      | Some dbnames -> make_db_list dbnames
      | None -> current_pure_db ()
    in
    let d = mk_trivial_dbg debug in
    let local_db = make_local_hint_db env sigma false lems in
    tclTRY_dbg d (trivial_fail_db d db_list local_db)
  end

(**************************************************************************)
(*                       The classical Auto tactic                        *)
(**************************************************************************)

exception SearchBound

let var_values gl =
  let sigma = Proofview.Goal.sigma gl in
  let hyps = Proofview.Goal.hyps gl in

  let open Context in
  let hnames = List.concat (List.map (fun i ->
      match i with
      | Named.Declaration.LocalAssum ({binder_name=id}, typ) ->
        let vartype = if EConstr.isInd sigma typ then "IND" else "HYP" in
        [id, vartype]
      | Named.Declaration.LocalDef ({binder_name=id}, value, typ) -> []) (* Probably a set *)
    hyps)
  in
  hnames

(* n is the max depth of search *)
(* local_db contains the local Hypotheses *)

let search d n db_list lems =
  let make_local_db gl =
    let env = Proofview.Goal.env gl in
    let sigma = Proofview.Goal.sigma gl in
    make_local_hint_db env sigma false lems
  in
  let rec search d n local_db =
    Tacticals.check_timeout ();  (* seems necessary to make timeouts reliable on WSL2/Ubuntu *)
    if Int.equal n 0 then
      let info = Exninfo.reify () in
      Proofview.tclZERO ~info SearchBound
    else
      Tacticals.tclORELSE0 (dbg_assumption d) @@
      Tacticals.tclORELSE0 (intro_register d (search d n) local_db) @@
      Proofview.Goal.enter begin fun gl ->
        let env = Proofview.Goal.env gl in
        let sigma = Proofview.Goal.sigma gl in
        let concl = Proofview.Goal.concl gl in
        let hyps = Proofview.Goal.hyps gl in
        let v_val = var_values gl in
        let d' = incr_dbg d in
        let secvars = compute_secvars gl in
        let hdc = try Some (decompose_app_bound sigma concl) with Bound -> None in
        let hintmap = hintmap_of env sigma secvars hdc concl in
        let hinttac = tac_of_hint d db_list local_db concl v_val in
        (local_db::db_list)
(*      |> is reverse function application (x |> f is the same as f x) *)
        |> List.map_append (fun db -> try hintmap db with Not_found -> [])
        |> List.map begin fun h ->
             Proofview.tclTHEN (hinttac h) @@
               Proofview.Goal.enter begin fun gl ->
                 let hyps' = Proofview.Goal.hyps gl in
                 let local_db' =
                   (* update local_db if local hypotheses have changed *)
                   if hyps' == hyps then local_db else make_local_db gl
                 in
                 search d' (n-1) local_db'
               end
           end
        |> Tacticals.tclFIRST
      end
  in
  Proofview.Goal.enter begin fun gl ->
    search d n (make_local_db gl)
  end

let default_search_depth = 5

let gen_auto ?(debug=Off) n lems dbnames =
  PSHash.reset pshash;
  hintCounts := HintCounts.empty;
  auto_stats := init_stats;
  let start = get_time () in
  Hints.wrap_hint_warning @@
    Proofview.Goal.enter begin fun gl ->
    let n = match n with None -> default_search_depth | Some n -> n in
    let db_list =
      match dbnames with
      | Some dbnames -> make_db_list dbnames
      | None -> current_pure_db ()
    in
    let d = mk_auto_dbg debug in
    let delay f = Proofview.tclUNIT () >>= fun () -> f () in
    let stats = delay (fun () ->
      if debug = Info then begin
        let delta_t = get_time () -. start in
        Feedback.msg_notice (str (Printf.sprintf "tries %d  successes %d  duplicates %d  fails %d  tac time %3.3f  total time %3.3f"
          !auto_stats.tries !auto_stats.successes !auto_stats.dups
          (!auto_stats.tries - !auto_stats.successes - !auto_stats.dups)
          !auto_stats.time delta_t));
        HintCounts.iter (fun tac counts ->
            Feedback.msg_notice (Pp.str (Printf.sprintf "%5d %5d %5d %5d %1.2f %2.3f %s"
              counts.tries counts.successes counts.dups
              (counts.tries - counts.successes - counts.dups)
              (float_of_int counts.goals /. (float_of_int counts.successes))
              counts.time tac))) !hintCounts;
      end;
      Proofview.tclUNIT ()) in
    Proofview.tclTHEN
      (Proofview.tclORELSE
        (tclTRY_dbg d (search d n db_list lems))
        (fun (exn, info) ->
          Tacticals.tclTHEN
            stats
            (Proofview.tclZERO ~info exn)))
      stats
  end

let auto ?(debug=Off) n lems dbnames = gen_auto ~debug (Some n) lems (Some dbnames)

let default_auto = auto default_search_depth [] []
