(************************************************************************)
(*         *      The Rocq Prover / The Rocq Development Team           *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* Interpretation of search commands *)

open CErrors
open Names
open Util
open Pp
open Printer
open Search
open Vernacexpr
open Goptions

let open_or_global_module qid =
  try Nametab.full_name_open_mod qid
  with Not_found ->
    try Nametab.full_name_module qid
    with Not_found ->
      user_err ?loc:qid.CAst.loc
       (str "Module/Section " ++ Ppconstr.pr_qualid qid ++ str " not found.")

(*
let global_module qid =
    user_err ?loc:qid.CAst.loc
     (str "Module/Section " ++ Ppconstr.pr_qualid qid ++ str " not found.")
*)
let interp_search_restriction = function
  | SearchOutside l ->
      SearchOutside (List.map open_or_global_module l)
  | SearchInside l ->
      SearchInside (List.map open_or_global_module l)

let kind_searcher env = Decls.(function
  (* Kinds referring to the keyword introducing the object *)
  | IsAssumption _
  | IsDefinition (Definition | Example | Fixpoint | CoFixpoint | Method | StructureComponent | Let | LetContext)
  | IsProof _
  | IsPrimitive
  | IsSymbol as k -> Inl k
  (* Kinds referring to the status of the object *)
  | IsDefinition (Coercion | SubClass | IdentityCoercion as k') ->
    let coercions = Coercionops.coercions () in
    Inr (fun gr -> List.exists (fun c -> Environ.QGlobRef.equal env c.Coercionops.coe_value gr &&
                                      (k' <> SubClass && k' <> IdentityCoercion || c.Coercionops.coe_is_identity)) coercions)
  | IsDefinition CanonicalStructure ->
    let canonproj = Structures.CSTable.entries () in
    Inr (fun gr -> List.exists (fun c -> Environ.QGlobRef.equal env c.Structures.CSTable.solution gr) canonproj)
  | IsDefinition Scheme ->
    let schemes = DeclareScheme.all_schemes () in
    let schemes = lazy begin
      Indmap.fold (fun _ schemes acc ->
          CString.Map.fold (fun _ c acc -> Cset.add c acc) schemes acc)
        schemes Cset.empty
    end
    in
    Inr (function
        | ConstRef c -> Cset.mem c (Lazy.force schemes)
        | _ -> false)
  | IsDefinition Instance ->
    let instances = Typeclasses.all_instances () in
    Inr (fun gr -> List.exists (fun c -> Environ.QGlobRef.equal env c.Typeclasses.is_impl gr) instances))

let interp_constr_pattern env sigma ?(expected_type=Pretyping.WithoutTypeConstraint) c =
  let c = Constrintern.intern_gen expected_type ~pattern_mode:true env sigma c in
  let flags = { Pretyping.no_classes_no_fail_inference_flags with expand_evars = false } in
  let sigma, c = Pretyping.understand_tcc ~flags env sigma ~expected_type c in
  (* FIXME: it is necessary to be unsafe here because of the way we handle
     evars in the pretyper. Sometimes they get solved eagerly. *)
  Patternops.legacy_bad_pattern_of_constr env sigma c

let interp_search_item env sigma =
  function
  | SearchSubPattern ((where,head),pat) ->
      let expected_type = Pretyping.(if head then IsType else WithoutTypeConstraint) in
      let pat =
        try interp_constr_pattern env sigma ~expected_type pat
        with e when CErrors.noncritical e ->
          (* We cannot ensure (yet?) that a typable pattern will
             actually be typed, consider e.g. (forall A, A -> A /\ A)
             which fails, not seeing that A can be Prop; so we use an
             untyped pattern as a fallback (i.e w/o no insertion of
             coercions, no compilation of pattern-matching) *)
          snd (Constrintern.interp_constr_pattern env sigma ~as_type:head pat) in
      GlobSearchSubPattern (where,head,pat)
  | SearchString ((Anywhere,false),s,None)
      when Id.is_valid_ident_part s && String.equal (String.drop_simple_quotes s) s ->
      GlobSearchString s
  | SearchString ((where,head),s,sc) ->
      let sc = Option.map snd sc in
      let ref =
        Notation.interp_notation_as_global_reference
          ~head:false (fun _ -> true) s sc in
      GlobSearchSubPattern (where,head,Pattern.PRef ref)
  | SearchKind k ->
     match kind_searcher env k with
     | Inl k -> GlobSearchKind k
     | Inr f -> GlobSearchFilter f

let rec interp_search_request env sigma = function
  | b, SearchLiteral i -> b, GlobSearchLiteral (interp_search_item env sigma i)
  | b, SearchDisjConj l -> b, GlobSearchDisjConj (List.map (List.map (interp_search_request env sigma)) l)

(* 05f22a5d6d5b8e3e80f1a37321708ce401834430 introduced the
   `search_output_name_only` option to avoid excessive printing when
   searching.

   The motivation was to make search usable for IDE completion,
   however, it is still too slow due to the non-indexed nature of the
   underlying search mechanism.

   In the future we should deprecate the option and provide a fast,
   indexed name-searching interface.
*)
let search_output_name_only = ref false

let () =
  declare_bool_option
    { optstage = Summary.Stage.Interp;
      optdepr  = None;
      optkey   = ["Search";"Output";"Name";"Only"];
      optread  = (fun () -> !search_output_name_only);
      optwrite = (:=) search_output_name_only }

(* todo: likely some mistakes here *)
let rec get_len c =
  let open Constr in
  match kind c with
  | Rel _ -> 1
  | Var _ -> 1
  | Meta _ -> 1
  | Evar _ -> 1
  | Sort _ -> 1
  | Cast (c,_, t) -> 1 + (get_len c)
  | Prod (na,t,c) -> 1 + (get_len t) + (get_len c)
  | Lambda (na,t,c) -> 1 + (get_len t) + (get_len c)
  | LetIn (na,b,t,c) -> 1 + (get_len b) + (get_len t) + (get_len c)
  | App (c,l) -> (get_len c) + (Array.fold_left (fun acc v -> acc + (get_len v)) 0 l)
  | Const _ -> 1
  | Ind _ -> 1
  | Construct _ -> 1
  | Case (ci,u,params, ((_,pv),_) ,iv,c,brs) -> 1 + (get_len c) +
      (get_len pv) +
      (Array.fold_left (fun acc (_,v) -> acc + (get_len v)) 0 brs)
  | Fix ((t,i),(lna,tl,bl)) -> 1 + (Array.fold_left (fun acc v -> acc + (get_len v)) 0 tl) +
                                   (Array.fold_left (fun acc v -> acc + (get_len v)) 0 bl)
  | CoFix(i,(lna,tl,bl)) -> 1 + (Array.fold_left (fun acc v -> acc + (get_len v)) 0 tl) +
                                (Array.fold_left (fun acc v -> acc + (get_len v)) 0 bl)
  | Proj _ -> 1
  | Int _ -> 1
  | Float _ -> 1
  | String _ -> 1
  | Array (u,t,def,ty) -> 1 + (Array.fold_left (fun acc v -> acc + (get_len v)) 0 t)

let rec get_head c =
  let open Constr in
  match kind c with
  | Prod (na,t,c) ->
    get_head c
  | _ -> c

let rec pr_constr ?(indent=0) c =
  let open Constr in
  let s = match kind c with
  | Rel i -> "Rel " ^ (string_of_int i)
  | Var _ -> "Var"
  | Meta _ -> "Meta"
  | Evar _ -> "Evar"
  | Sort _ -> "Sort"
  | Cast _ -> "Cast"
  | Prod (na,t,c) -> "Prod " ^ (Pp.string_of_ppcmds (Name.print na.binder_name))
  | Lambda _ -> "Lambda"
  | LetIn _ -> "LetIn"
  | App _ -> "App"
  | Const (c,u) -> "Const " ^ (Names.Constant.to_string c)
  | Ind ((i,_),u) -> "Ind " ^ (Names.MutInd.to_string i)
  | Construct (((c,i),_),u) -> Printf.sprintf "Construct %s %d" (Names.MutInd.to_string c) i
  | Case _ -> "Case"
  | Fix _ -> "Fix"
  | CoFix _ -> "CoFix"
  | Proj _ -> "Proj"
  | Int _ -> "Int"
  | Float _ -> "Float"
  | String _ -> "String"
  | Array _ -> "Array"
  in
  Printf.eprintf "%s%s\n%!" (String.make indent ' ') s;
  let indent = indent + 2 in
  match kind c with
  | Prod (na,t,c) ->
    pr_constr ~indent t;
    pr_constr ~indent c
  | App (c,l) ->
    pr_constr ~indent c;
    Array.iter (fun i -> pr_constr ~indent i) l
  | Lambda (na,t,c) ->
    pr_constr ~indent t;
    pr_constr ~indent c;
  | _ -> ()

let add_hint hint ref =
  let open Hints in
  try
    add_hints ~locality:SuperGlobal ["AUTO"] hint
  with | e -> ()
(*  Printf.eprintf "Can't add %s\n%!" (Pp.string_of_ppcmds (Printer.pr_global ref)) *)

let fwd_do_rewrite = ref ((fun x -> failwith "fwd_do_rewrite") :
    Libnames.qualid -> int -> bool -> Hints.hints_entry)

let rec hyps_len c =
  let open Constr in
  let open Vars in
  match Constr.kind c with
  | Prod(_,t,c2) -> if noccurn 1 c2 then (get_len t)+(hyps_len c2) else hyps_len c2
  | _ -> 0

let get_pri diff =
  (if diff < 0 then 100
   else if diff = 0 then 150
   else 200) + diff

(* todo: also add for registered setoid equalities *)
let add_rewrite_hints kn c ref =
  let open Constr in
  match kind (get_head c) with
  | App (c,l) ->
    begin match kind c with
    | Ind ((i,_),u) ->
      begin match Names.MutInd.to_string i with
        | "Coq.Init.Logic.eq" ->
          let qid = Libnames.qualid_of_string (KerName.to_string kn) in
(*          Printf.eprintf "Sizes lhs = %d rhs = %d\n\n%!" (get_len l.(1)) (get_len l.(2)); *)
(*          (try *)
          (* allow for implicit type for eq at l.(0) *)
          (* set priority based on length difference *)
          let diff = (get_len l.(2)) - (get_len l.(1)) in
          let rtol = diff >= 0 in

          let cst = Global.constant_of_delta_kn kn in
          let ref = GlobRef.ConstRef cst in
          add_hint (!fwd_do_rewrite qid (get_pri diff) rtol ) ref;
          (* todo: don't add symmetric rules such as add_comm twice *)
          add_hint (!fwd_do_rewrite qid (get_pri (- diff)) (not rtol)) ref

        | "Coq.Init.Logic.iff" -> () (* todo *)
        | _ -> ()
      end;
    | _ -> ()
    end
  | _ -> ()

let test = (try let _ = Sys.getenv("TEST") in true with _ -> false)

let () = Declare.set_reg_callback (fun (kn:KerName.t) (kind:Decls.logical_kind) (is_new:bool) ->
    let verbose = false in
    if test then try begin
      if verbose then Printf.eprintf "reg_callback %s\n%!" (Names.KerName.to_string kn);
      Printexc.record_backtrace true;
      let cst = Global.constant_of_delta_kn kn in
      let ref = GlobRef.ConstRef cst in
      let (typ, _) = Typeops.type_of_global_in_context (Global.env ()) ref in
      let hlen = hyps_len typ in
      if hlen = 0 then
        add_rewrite_hints kn typ ref;
      let clen = get_len (get_head typ) in
      let pri = get_pri (hlen - clen) in
      if verbose && (pri <> 200) then
        Printf.eprintf "%s  hlen %d  clen %d\n%!" (KerName.to_string kn) hlen clen;

      let open Hints in
      match kind with
      | Decls.IsProof tk ->
(*        Printf.eprintf "%s %s\n%!" (Decls.tk_to_string tk) *)
(*          (Pp.string_of_ppcmds (Printer.pr_global ref)); *)
        (* note you can pass multiple theorems with HintsResolveEntry *)
        if test && is_new then pr_constr typ;
        add_hint ((HintsResolveEntry [{ hint_priority = Some pri; hint_pattern = None }, true, ref])) ref;
      | Decls.IsDefinition df ->
(*        Printf.eprintf "%s %s\n%!" (Decls.df_to_string df) *)
(*          (Pp.string_of_ppcmds (Printer.pr_global ref)); *)
        if Constr.isProd typ then
          add_hint ((HintsResolveEntry [{ hint_priority = Some pri; hint_pattern = None }, true, ref])) ref;
      | _ -> (* Printf.eprintf "logical kind %s\n%!" (Decls.lk_to_string kind); *) ()
    end with | Not_found -> Printf.eprintf "Not_found\n%!"
    | e ->
      Printf.eprintf "Error: %s\n%s\n" (Printexc.to_string e) (Printexc.get_backtrace ()))


let interp_search env sigma s r =
  let r = interp_search_restriction r in
  let get_pattern c = snd (Constrintern.interp_constr_pattern env sigma c) in
  let warnlist = ref [] in
  let pr_search ref kind env sigma c = (* c is a Constr.t *)
    let pr = pr_global ref in (* ref is a GlobRef.t *)
    let pp = if !search_output_name_only
      then pr
      else begin
        let open Impargs in
        let impls = implicits_of_global ref in
        let impargs = select_stronger_impargs impls in
        let impargs = List.map binding_kind_of_status impargs in
        if List.length impls > 1 ||
           List.exists Glob_term.(function Explicit -> false | MaxImplicit | NonMaxImplicit -> true)
             (List.skipn_at_best (Termops.nb_prod_modulo_zeta sigma (EConstr.of_constr c)) impargs)
          then warnlist := pr :: !warnlist;
        let pc = pr_ltype_env env sigma ~impargs c in
        let head = get_head c in
        pr_constr head;
        hov 2 (pr ++ str":" ++ spc () ++ pc)
      end
    in Feedback.msg_notice pp
  in
  (match s with
  | SearchPattern c ->
      (Search.search_pattern env sigma (get_pattern c) r |> Search.prioritize_search) pr_search
  | SearchRewrite c ->
      (Search.search_rewrite env sigma (get_pattern c) r |> Search.prioritize_search) pr_search
  | Search sl ->
      (Search.search env sigma (List.map (interp_search_request env Evd.(from_env env)) sl) r |>
       Search.prioritize_search) pr_search);
  if !warnlist <> [] then
  Feedback.msg_notice (str "(" ++
    hov 0 (strbrk "use \"About\" for full details on the implicit arguments of " ++
           pr_enum (fun x -> x) !warnlist ++ str ")"))
