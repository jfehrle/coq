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

let pr_constr_line ?(indent=0) c =
  if indent >= 0 then begin
    let open Constr in
    let s = match kind c with
    | Rel i -> "Rel " ^ (string_of_int i)
    | Var _ -> "Var"
    | Meta n -> "Meta " ^ (string_of_int n)
    | Evar _ -> "Evar"
    | Sort _ -> "Sort"
    | Cast _ -> "Cast"
    | Prod (na,t,c) -> "Prod " ^ (Pp.string_of_ppcmds (Name.print na.binder_name))
    | Lambda (na, t, c) -> "Lambda " ^ (Pp.string_of_ppcmds (Name.print na.binder_name))
    | LetIn (na,b,t,c) -> "LetIn " ^ (Pp.string_of_ppcmds (Name.print na.binder_name))
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
    Printf.eprintf "%s%s\n%!" (String.make indent ' ') s
  end

(* todo: consolidate with Hints.pr_constr *)
let rec pr_constr ?(indent=0) c =
  let open Constr in
  pr_constr_line ~indent c;
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
  | Case (ci,u,params,p,iv,c,brs) ->
(*
    | Case      of case_info * 'univs * 'constr array * ('types,'r) pcase_return * 'constr pcase_invert * 'constr * ('constr,'r) pcase_branch array
type ('constr,'r) pcase_branch = (Name.t,'r) Context.pbinder_annot array * 'constr
type ('a,'r) pbinder_annot = { binder_name : 'a; binder_relevance : 'r }
*)
    let open Context in
    Array.iter (fun p -> pr_constr ~indent p) params;
    (* todo: iv *)
    let (pannota,_),_ = p in
    Array.iter (fun pcr ->
            Printf.eprintf "%s(pretn nb) %s\n%!" (String.make indent ' ')
              (Pp.string_of_ppcmds (Name.print pcr.binder_name))) pannota;
    Array.iter (fun (pannota,c) ->
        Array.iter (fun nb ->
            Printf.eprintf "%s(brs nb) %s\n%!" (String.make indent ' ')
              (Pp.string_of_ppcmds (Name.print nb.binder_name))) pannota;
        let indent = indent + 2 in
        pr_constr ~indent c
    ) brs;
    pr_constr ~indent c;
  | _ -> ()

(* todo: something more elegant *)
let errors_regexp = Str.regexp (String.concat {|\||} [
  "cannot be used as a hint.";
  "Head pattern or sub-pattern must be a global constant"
  ])


let add_hint hint ref =
  let open Hints in
  try
    add_hints ~locality:SuperGlobal ["AUTO"] hint
  with
    | UserError pp when try let _ = Str.search_forward errors_regexp (Pp.string_of_ppcmds pp) 0 in true with Not_found -> false -> ()
    | e -> Printf.eprintf "add_hint Error: for %s: %s\n"
      (Pp.string_of_ppcmds (Printer.pr_global ref))
      (Printexc.to_string e)

let fwd_do_rewrite = ref ((fun x -> failwith "fwd_do_rewrite") :
    Libnames.qualid -> int -> bool ->
    (Names.Id.Set.t * Pattern.constr_pattern) option -> Hints.hints_entry)

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

let whitelist = ["mult_n_Sm";
                "my_mult_n_Sm";
                "my_add_cancel_r";
                "my_dist_plus_mul";
                "my_mul_comm";
                "my_mult_Sn_m";
                "my_plus_assoc";
                "my_plus_comm";
                "my_plus_n_O";
                "my_plus_n_Sm";
                "plus_O_n";
                "plus_Sn_m";
                "plus_n_O";
                "plus_n_Sm";
                "pred_Sn";
                "rev3";
                "rev4";
                "rev5";
                "rev6"]
let _ = whitelist

let rew_sig = ref false

let replace_Rel c =
(*  pr_constr c; *)
(*  Printf.eprintf "replace_Rel, print while replacing:\n%!"; *)
  let head = get_head c in
  let metaMap = ref Int.Map.empty in
  let rec upd before_head indent vars c =
    let open Constr in
    let before_head' = before_head && (c != head) in
    if !rew_sig then pr_constr_line ~indent c;
    let indent = if indent >= 0 then indent + 2 else indent in
    let recur vars c = Constr.map (upd before_head' indent vars) c in
    match kind c with
    | Prod (na,_,_) when before_head ->
      let vars = (match na.binder_name with
          | Name id ->
            let meta = Evarutil.new_meta () in
            metaMap := Int.Map.add meta id !metaMap;
            Some (mkMeta meta)
          | Anonymous -> None) :: vars
      in
      (match kind (recur vars c) with
      | Prod (_,_,c') -> c'
      | _ -> assert false)
    | Lambda _ | LetIn _ | Fix _ | CoFix _ ->
      recur (None::vars) c
    | Rel n ->
      let len = List.length vars in
      if n > len then Printf.eprintf "Error: nth of %d, length = %d\n%!" n (List.length vars);
      (match List.nth vars (n-1) with
      | Some meta -> meta
      | None -> c)
    | Case (ci,u,pms,p,iv,b,bl) ->
      let vars2 = ref vars in
      let map_under_context f d =
        let (nas, p) = d in
        let rec newvars n vars = if n = 0 then vars else newvars (n-1) (None :: vars) in
        vars2 := newvars (Array.length nas) vars;
        let p' = (recur !vars2) p in
        if p' == p then d else (nas, p')
      in
      let map_return_predicate f (p,r as v) =
        begin
          let (nas, _) = p in
          let open Context in
          if !rew_sig then Printf.eprintf "as <names>: %s\n%!" (String.concat " " (Array.to_list (Array.map (fun i -> Pp.string_of_ppcmds (Name.print i.binder_name)) nas)));
        end;
        let p' = map_under_context f p in
        if p == p' then v else p', r
      in
      let map_branches f bl =
        let bl' = Array.map (map_under_context f) bl in
        if Array.for_all2 (==) bl' bl then bl else bl'
      in
      let f = recur vars in
      let pms' = Array.Smart.map f pms in
      let b' = f b in
      let iv' = map_invert f iv in
      let p' = map_return_predicate f p in
      let bl' = map_branches (recur !vars2) bl in
      if b'==b && iv'==iv && p'==p && bl'==bl && pms'==pms then c
      else mkCase (ci, u, pms', p', iv', b', bl')
    | _ -> recur vars c
  in
  let indent = if !rew_sig then 0 else -1 in
  let c' = upd true indent [] c in
  c', !metaMap

let meta_regexp = Str.regexp {|META\([0-9]*\)|}

let replace_PMeta p map =
  let rec upd p =
    let open Pattern in
    match p with
    | PMeta (Some id) ->
      let s = Id.to_string id in
      if Str.string_match meta_regexp s 0 then
        let n = int_of_string (Str.matched_group 1 s) in
        PMeta (Some (Int.Map.find n map))  (* Not_found? *)
      else p
    | _ -> Patternops.map upd p
  in
  upd p


let get_info (c : EConstr.t) =
  let env = Global.env() in
  let sigma = Evd.from_env env in
  let c, map = replace_Rel (EConstr.to_constr sigma c) in
  if !rew_sig then Printf.eprintf "after replace:\n%!";
  if !rew_sig then pr_constr c;
  let c = EConstr.of_constr c in
  let ids = List.fold_left (fun set (meta,id) -> Id.Set.add id set) Id.Set.empty (Int.Map.bindings map) in
(*  let idlist = List.map (fun id -> Names.Id.to_string id) (Id.Set.elements ids) in *)
(*  if CList.test then *)
(*    Printf.eprintf "get_info ids: %s\n%!" (String.concat " " idlist); *)
  let lhs, rhs = match EConstr.kind sigma c with
  | App (c,l) -> l.(1), l.(2)
  | _ -> assert false
  in
  ids, map, lhs, rhs

let to_pat c ids map =
  let env = Global.env() in
  let sigma = Evd.from_env env in
  let pat = Patternops.pattern_of_constr env sigma c in
  let pat = replace_PMeta pat map in
  Some (ids, pat)

(* todo: also add for registered setoid equalities *)
let add_rewrite_hints kn c0 ref =
  let open Constr in
  match kind (get_head c0) with
  | App (c,l) ->
    begin match kind c with
    | Ind ((i,_),_) ->
      begin match Names.MutInd.to_string i with
        | "Corelib.Init.Logic.eq" ->
          let qid = Libnames.qualid_of_string (KerName.to_string kn) in
(*          Printf.eprintf "Sizes lhs = %d rhs = %d\n\n%!" (get_len l.(1)) (get_len l.(2)); *)
(*          (try *)
          (* allow for implicit type for eq at l.(0) *)
          (* set priority based on length difference *)
          let diff = (get_len l.(2)) - (get_len l.(1)) in
          let rtol = diff >= 0 in

          let cst = Global.constant_of_delta_kn kn in
          let ref_ = GlobRef.ConstRef cst in
          (* todo: for debugging UNBOUND_REL in rew_sig *)
(*          rew_sig := (KerName.to_string kn) = "Corelib.Init.Specif.rew_sig"; *)
          if !rew_sig then Printf.eprintf "kn = %s\n%!" (KerName.to_string kn);
          let ids, map, lhs, rhs = get_info (EConstr.of_constr c0) in
          let lpat = to_pat lhs ids map in
          let rpat = to_pat rhs ids map in
          add_hint (!fwd_do_rewrite qid (get_pri diff) rtol lpat) ref_;
          (* todo: don't add symmetric rules such as add_comm twice *)
          add_hint (!fwd_do_rewrite qid (get_pri (- diff)) (not rtol) rpat) ref
(*          DUPLICATES FOR PERF MEASUREMENT: *)
(*          ; add_hint (!fwd_do_rewrite qid (get_pri diff) rtol ) ref; *)
(*          add_hint (!fwd_do_rewrite qid (get_pri (- diff)) (not rtol)) ref *)

        | "Corelib.Init.Logic.iff" -> () (* todo *)
        | _ -> ()
      end;
    | _ -> ()
    end
  | _ -> ()

let () = Declare.set_reg_callback (fun (kn:KerName.t) (kind:Decls.logical_kind) (is_new:bool) ->
    let verbose = false in
    if CList.test then try begin
(*      if verbose then Printf.eprintf "reg_callback %s\n%!" (Names.KerName.to_string kn); *)
      begin try ignore @@ Hints.searchtable_map "AUTO" with Not_found ->
        Hints.create_hint_db false "AUTO" TransparentState.empty true end;
      let cst = Global.constant_of_delta_kn kn in
      let ref = GlobRef.ConstRef cst in
      let (typ, _) = Typeops.type_of_global_in_context (Global.env ()) ref in
      let hlen = hyps_len typ in
(*      let label = Id.to_string (Label.to_id (KerName.label kn)) in *)
      if hlen = 0 (* && List.mem label whitelist *) then
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
(*        if CList.test && is_new then pr_constr typ; *)
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
