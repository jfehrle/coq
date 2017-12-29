(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2017     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(* todo: why isn't this needed?
type diffs_level = [ `OFF (* don't show diffs at all *)
                   | `ON  (* only show added text *)
                   | `REMOVED (* show both added and removed text *)
                   ]
*)

let diff_option = ref `OFF

(* todo: How to persist the value between sessions?
   Eg if the user wants this as a permanent config setting? *)
let read_diffs_option () = match !diff_option with
| `OFF -> "off"
| `ON -> "on"
| `REMOVED -> "removed"

let write_diffs_option = function
| "yes" | "off" -> diff_option := `OFF
| "no" | "on" -> diff_option := `ON
| "removed" -> diff_option := `REMOVED
| _ -> CErrors.user_err Pp.(str "Diffs option only accepts the following values: off, on, removed.")

let _ =
  Goptions.(declare_string_option {
    optdepr = false;
    optname = "show diffs in proofs" ;
    optkey = ["Diffs"] ;
    optread = read_diffs_option ;
    optwrite = write_diffs_option
  })

let show_diffs () = !diff_option <> `OFF;;
let show_removed () = !diff_option = `REMOVED;;


(* DEBUG/UNIT TEST *)
let cfprintf oc = Printf.(kfprintf (fun oc -> fprintf oc "") oc)
let log_out_ch = ref stdout
let cprintf s = cfprintf !log_out_ch s


(* GENERAL DIFF CODE *)

module StringDiff = Diff2.Make(struct
  type elem = String.t
  type t = elem array
  let get t i = Array.get t i
  let length t = Array.length t
end)

type diff_list = StringDiff.elem Diff2.edit list

(* debug print diff data structure *)
let string_of_diffs diffs =
  let buf = Buffer.create 16 in
  let rec string_of_diffs_r = function
    | `Common (opos, npos, s) :: t ->
      Printf.bprintf buf "Common '%s' opos = %d npos = %d\n" s opos npos;
      string_of_diffs_r t
    | `Removed (pos, s) :: t ->
      Printf.bprintf buf  "Removed '%s' opos = %d\n" s pos;
      string_of_diffs_r t
    | `Added (pos, s) :: t ->
      Printf.bprintf buf  "Added '%s' npos = %d\n" s pos;
      string_of_diffs_r t
    | [] -> ()
  in
  string_of_diffs_r diffs;
  Buffer.contents buf

(* Adjust the diffs returned by the Myers algorithm to reduce the span of the
changes.  This gives more natural-looking diffs.

While the Myers algorithm minimizes the number of changes between two
sequences, it doesn't minimize the span of the changes.  For example,
representing elements in common in lower case and inserted elements in upper
case (but ignoring case in the algorithm), ABabC and abABC both have 3 changes
(A, B and C).  However the span of the first sequence is 5 elements (ABabC)
while the span of the second is 3 elements (ABC).

The algorithm modifies the changes iteratively, for example ABabC -> aBAbC -> abABC

dtype: identifies which of Added OR Removed to use; the other one is ignored.
diff_list: output from the Myers algorithm
*)
(* todo: is there a javadoc equivalent comment format? *)
let shorten_diff_span dtype diff_list =
  let changed = ref false in
  let diffs = Array.of_list diff_list in
  let len = Array.length diffs in
  let vinfo index =
    match diffs.(index) with
    (* todo: surely there is a better way to do this *)
    | `Common (opos, npos, s) -> (`Common, opos, npos, s)
    | `Removed (pos, s) -> (`Removed, pos, 0, s)
    | `Added (pos, s) -> (`Added, 0, pos, s) in
  let get_variant index =
    let (v, _, _, _) = vinfo index in
    v in
  let get_str index =
    let (_, _, _, s) = vinfo index in
    s in

  let iter start len lt incr = begin
    let src = ref start in
    let dst = ref start in
    while (lt !src len) do
      if (get_variant !src) = dtype then begin
        if (lt !dst !src) then
          dst := !src;
        while (lt !dst len) && (get_variant !dst) <> `Common do
          dst := !dst + incr;
        done;
        if (lt !dst len) && (get_str !src) = (get_str !dst) then begin
          (* swap diff *)
          let (_, c_opos, c_npos, str) = vinfo !dst
          and (_, v_opos, v_npos, _) = vinfo !src in
          changed := true;
          if dtype = `Added then begin
            diffs.(!src) <- `Common (c_opos, v_npos, str);
            diffs.(!dst) <- `Added (c_npos, str);
          end else begin
            diffs.(!src) <- `Common (v_opos, c_npos, str);
            diffs.(!dst) <- `Removed (c_opos, str)
          end
        end
      end;
      src := !src + incr
    done
  end in

  iter 0 len (<) 1; (* left to right *)
  iter (len-1) (-1) (>) (-1); (* right to left *)
  if !changed then Array.to_list diffs else diff_list;;

let has_changes diffs =
  let rec has_changes_r diffs added removed =
    match diffs with
    | `Added _ :: t   -> has_changes_r t true removed
    | `Removed _ :: t -> has_changes_r t added true
    | h :: t -> has_changes_r t added removed
    | [] -> (added, removed) in
  has_changes_r diffs false false;;

(* get the Myers diff of 2 lists of strings *)
let diff_strs old_strs new_strs =
  let diffs = List.rev (StringDiff.diff old_strs new_strs) in
  shorten_diff_span `Removed (shorten_diff_span `Added diffs);;


let tokenize_string s =
  (* todo: cLexer changes buff as it proceeds.  Seems like that should be saved, too.
  But I don't understand how it's used--it looks like things get appended to it but
  it never gets cleared. *)
  let rec stream_tok acc str =
    let e = Stream.next str in
    if Tok.(equal e EOI) then
      List.rev acc
    else
      stream_tok ((Tok.extract_string e) :: acc) str
  in
  let st = CLexer.get_lexer_state () in
  try
    let istr = Stream.of_string s in
    let lex = CLexer.lexer.Plexing.tok_func istr in
    let toks = stream_tok [] (fst lex) in
    CLexer.set_lexer_state st;
    toks
  with exn ->
    CLexer.set_lexer_state st;
    raise exn;;

(* get the Myers diff of 2 strings *)
let diff_str old_str new_str =
  let old_toks = Array.of_list (tokenize_string old_str)
  and new_toks = Array.of_list (tokenize_string new_str) in
  diff_strs old_toks new_toks;;

type diff_type =
  [ `Removed
  | `Added
  | `Common
  ]

let get_dinfo = function
  | `Common (_, _, s) -> (`Common, s)
  | `Removed (_, s) -> (`Removed, s)
  | `Added (_, s) -> (`Added, s)

[@@@ocaml.warning "-32"]
let string_of_diff_type = function
  | `Common  -> "Common"
  | `Removed -> "Removed"
  | `Added -> "Added"
[@@@ocaml.warning "+32"]

(* Proof AND Pp-SPECIFIC CODE *)

let wrap_in_bg diff_tag pp =
  let open Pp in
  (tag (Pp.start_pfx ^ diff_tag ^ ".bg") (str "")) ++ pp ++
  (tag (Pp.end_pfx   ^ diff_tag ^ ".bg") (str ""))

exception Trouble

(* Generate a new Pp that adds tags marking diffs to a Pp structure:
which - either `Added or `Removed, indicates which type of diffs to add
pp - the original structure. For `Added, must be the new pp passed to diff_pp
  For `Removed, must be the old pp passed to diff_pp
diffs - the diff list returned by diff_pp

Diffs of single strings in the Pp are tagged with "diff.added" or "diff.removed".
Diffs that span multiple strings in the Pp are tagged with "start.diff.*" or
"end.diff.*", but only on the first and last strings of the span.

Ppcmd_strings will be split into multiple Ppcmd_strings if a diff starts or ends
in the middle of the string.  Whitespace just before or just after a diff will
not part of the highlight.

Prexisting tags in pp may contain only a single Ppcmd_string.  Those tags will be
placed inside the diff tags to ensure proper nesting of tags within spans of
"start.diff.*" ... "end.diff.*".
 *)
let add_diff_tags which pp diffs  =
  let open Pp in
  let diff_tag = if which = `Added then "diff.added" else "diff.removed" in
  let diffs : diff_list ref = ref diffs in
  let in_diff = ref false in (* true = buf chars need a tag *)
  let in_span = ref false in (* true = last pp had a start tag *)
  let trans = ref false in   (* true = this diff starts/ends highlight *)
  let buf = Buffer.create 16 in
  let acc_pp = ref [] in
  let diff_str, diff_ind, diff_len = ref "", ref 0, ref 0 in
  let prev_dtype, dtype, next_dtype = ref `Common, ref `Common, ref `Common in
  let is_white c = List.mem c [' '; '\t'; '\n'; '\r'] in

  let skip () =
    while !diffs <> [] &&
      (let (t, _) = get_dinfo (List.hd !diffs) in
        t <> `Common && t <> which)
    do
      diffs := List.tl !diffs
    done
  in

  let put_tagged case =
    if Buffer.length buf > 0 then begin
      let pp = str (Buffer.contents buf) in
      Buffer.clear buf;
      let tagged = match case with
      | ""      -> pp
      | "tag"   -> tag diff_tag pp
      | "start" -> in_span := true;  tag (start_pfx ^ diff_tag) pp
      | "end"   -> in_span := false; tag (end_pfx ^ diff_tag) pp
      | _ -> raise Trouble in
      acc_pp := tagged :: !acc_pp
    end
  in

  let output_pps () =
    let next_diff_char_hl = if !diff_ind < !diff_len then !dtype = which else !next_dtype = which in
    let tag = if not !in_diff then ""
              else  if !in_span then
                      if next_diff_char_hl then "" else "end"
                    else
                      if next_diff_char_hl then "start" else "tag" in
    put_tagged tag;  (* flush any remainder *)
    let l = !acc_pp in
    acc_pp := [];
    match List.length l with
    | 0 -> str ""
    | 1 -> List.hd l
    | _ -> seq (List.rev l)
  in

  let maybe_next_diff () =
    if !diff_ind = !diff_len && (skip(); !diffs <> []) then begin
      let (t, s) = get_dinfo (List.hd !diffs) in
      diff_str := s; diff_ind := 0; diff_len := String.length !diff_str;
      diffs := List.tl !diffs; skip();
      prev_dtype := !dtype;
      dtype := t;
      next_dtype := (match !diffs with
        | diff2 :: _ -> let (nt, _) = get_dinfo diff2 in nt
        | [] -> `Common);
      trans := !dtype <> !prev_dtype
    end;
  in

  let s_char c =
    maybe_next_diff ();
    (* matching first should handle tokens with spaces, e.g. in comments/strings *)
    if !diff_ind < !diff_len && c = !diff_str.[!diff_ind] then begin
      if !dtype = which && !trans && !diff_ind = 0 then begin
        put_tagged "";
        in_diff := true
      end;
      Buffer.add_char buf c;
      diff_ind := !diff_ind + 1;
      if !dtype = which && !dtype <> !next_dtype && !diff_ind = !diff_len then begin
        put_tagged (if !in_span then "end" else "tag");
        in_diff := false
      end
    end else if is_white c then
      Buffer.add_char buf c
    else begin
      cprintf "mismatch: expected '%c' but got '%c'\n" !diff_str.[!diff_ind] c;
      raise Trouble  (* mismatch, shouldn't happen *)
    end
  in

  (* rearrange so existing tags are inside diff tags, provided that those tags
    only contain Ppcmd_string's.  Other cases (e.g. tag of a box) are not supported. *)
  (* todo: too many repr/unreprs here, is it me or the way it's defined in pp.*? *)
  let reorder_tags child pp_tag pp =
    match repr child with
    | Ppcmd_tag (t1, pp) -> tag t1 (tag pp_tag pp)
    | Ppcmd_glue l ->
      if List.exists (fun x ->
          match repr x with
          | Ppcmd_tag (_, _) -> true
          | _ -> false)  l
      then seq (List.map (fun x ->
          match repr x with
          | Ppcmd_tag (t2, pp2) -> tag t2 (tag pp_tag pp2)
          | pp2 -> tag pp_tag (unrepr pp2))   l)
      else child
    | _ -> tag pp_tag child
  in

  let rec add_tags_r pp =
    let r_pp = repr pp in
    match r_pp with
    | Ppcmd_string s -> String.iter s_char s; output_pps ()
    | Ppcmd_glue l -> seq (List.map add_tags_r l)
    | Ppcmd_box (block_type, pp) -> unrepr (Ppcmd_box (block_type, add_tags_r pp))
    | Ppcmd_tag (pp_tag, pp) -> reorder_tags (add_tags_r pp) pp_tag pp
    | _ -> pp
  in
  let (has_added, has_removed) = has_changes !diffs in
  let rv = add_tags_r pp in
  if !diffs <> [] then
    raise Trouble; (* left-over diff info *)
  if has_added || has_removed then wrap_in_bg diff_tag rv else rv;;

module StringMap = Map.Make(String);;

type hyp_info = {
  idents: string list;
  rhs_pp: Pp.t;
  mutable done_: bool;
}

(* Generate the diffs between the old and new hyps.
   This works by matching lines with the hypothesis name and diffing the right-hand side.
   Lines that have multiple names such as "n, m : nat" are handled specially to account
   for, say, the addition of m to a pre-existing "n : nat".
 *)
let diff_hyps o_line_idents o_map n_line_idents n_map =
  let rv : Pp.t list ref = ref [] in

  let is_done ident map = (StringMap.find ident map).done_ in
  let exists ident map = try StringMap.find ident map; true with Not_found -> false in
  let contains list ident =
    try List.find (fun x  -> x = ident) list; [ident]
    with Not_found -> []
  in

  let output old_ids_uo new_ids =
    (* use the order from the old line in case it's changed in the new *)
    let old_ids = if old_ids_uo = [] then [] else
      let orig = (StringMap.find (List.hd old_ids_uo) o_map).idents in
      List.concat (List.map (contains orig) old_ids_uo) in

    let setup ids map = if ids = [] then ("", Pp.mt ()) else
      let open Pp in
      let rhs_pp = (StringMap.find (List.hd ids) map).rhs_pp in
      let pp_ids = List.map (fun x -> str x) ids in
      let hyp_pp = List.fold_left (fun l1 l2 -> l1 ++ str ", " ++ l2) (List.hd pp_ids) (List.tl pp_ids) ++ rhs_pp in
      (string_of_ppcmds hyp_pp, hyp_pp)
    in

    let (o_line, o_pp) = setup old_ids o_map in
    let (n_line, n_pp) = setup new_ids n_map in

    let hyp_diffs = diff_str o_line n_line in
    let (has_added, has_removed) = has_changes hyp_diffs in
    if show_removed () && has_removed then begin
      let o_entry = StringMap.find (List.hd old_ids) o_map in
      o_entry.done_ <- true;
      rv := (add_diff_tags `Removed o_pp hyp_diffs) :: !rv;
    end;
    if n_line <> "" then begin
      let n_entry = StringMap.find (List.hd new_ids) n_map in
      n_entry.done_ <- true;
      rv := (add_diff_tags `Added n_pp hyp_diffs) :: !rv
    end
  in

  (* process identifier level diff *)
  let process_ident_diff diff =
    let (dtype, ident) = get_dinfo diff in
    match dtype with
    | `Removed ->
      if dtype = `Removed then begin
        let o_idents = (StringMap.find ident o_map).idents in
        (* only show lines that have all idents removed here; other removed idents appear later *)
        if show_removed () &&
            List.for_all (fun x -> not (exists x n_map)) o_idents then
          output (List.rev o_idents) []
      end
    | _ -> begin (* Added or Common case *)
      let n_idents = (StringMap.find ident n_map).idents in

      (* Process a new hyp line, possibly splitting it.  Duplicates some of
         process_ident iteration, but easier to understand this way *)
      let process_line ident2 =
        if not (is_done ident2 n_map) then begin
          let n_ids_list : string list ref = ref [] in
          let o_ids_list : string list ref = ref [] in
          let fst_omap_idents = ref None in
          let add ids id map =
            ids := id :: !ids;
            (StringMap.find id map).done_ <- true in

          (* get identifiers shared by one old and one new line, plus
             other Added in new and other Removed in old *)
          let process_split ident3 =
            if not (is_done ident3 n_map) then begin
              let this_omap_idents = try Some (StringMap.find ident3 o_map).idents
                                    with Not_found -> None in
              if !fst_omap_idents = None then
                fst_omap_idents := this_omap_idents;
              match (!fst_omap_idents, this_omap_idents) with
              | (Some fst, Some this) when fst == this ->  (* yes, == *)
                add n_ids_list ident3 n_map;
                add o_ids_list ident3 o_map;
                (* include all undone Removed idents in old *)
                List.iter (fun x -> if not (is_done x o_map) && not (exists x n_map) then
                                    (add o_ids_list x o_map)) fst
              | (_, None) ->
                add n_ids_list ident3 n_map (* include all undone Added idents in new *)
              | _ -> ()
            end in
          List.iter process_split n_idents;
          output (List.rev !o_ids_list) (List.rev !n_ids_list)
        end in
      List.iter process_line n_idents (* O(n^2), so sue me *)
    end in

  let cvt s = Array.of_list (List.concat s) in
  let ident_diffs = diff_strs (cvt o_line_idents) (cvt n_line_idents) in
  List.iter process_ident_diff ident_diffs;
  List.rev !rv;;


type 'a hyp = (Names.Id.t list * 'a option * 'a)
type 'a reified_goal = { name: string; ty: 'a; hyps: 'a hyp list; env : Environ.env; sigma: Evd.evar_map }

(* XXX: Port to proofview, one day. *)
(* open Proofview *)
module CDC = Context.Compacted.Declaration

let to_tuple : CDC.t -> (Names.Id.t list * 'pc option * 'pc) =
  let open CDC in function
    | LocalAssum(idl, tm)   -> (idl, None, tm)
    | LocalDef(idl,tdef,tm) -> (idl, Some tdef, tm);;

(* XXX: Very unfortunately we cannot use the Proofview interface as
   Proof is still using the "legacy" one. *)
let process_goal sigma g : Constr.t reified_goal =
  let env  = Goal.V82.env   sigma g in
  let hyps = Goal.V82.hyps  sigma g in
  let ty   = Goal.V82.concl sigma g in
  let name = Goal.uid g             in
  (* There is a Constr/Econstr mess here... *)
  let ty   = EConstr.to_constr sigma ty in
  (* compaction is usually desired [eg for better display] *)
  let hyps      = Termops.compact_named_context (Environ.named_context_of_val hyps) in
  let hyps      = List.map to_tuple hyps in
  { name; ty; hyps; env; sigma };;

let pp_of_type env sigma ty =
  Printer.pr_goal_concl_style_env env sigma EConstr.(of_constr ty);;

(* fetch info from a goal, returning (idents, map, concl_pp) where
idents is a list with one entry for each hypothesis, each entry is the list of
idents on the lhs of the hypothesis.  map is a map from ident to hyp_info
reoords.  For example: for the hypotheses:
  b : bool
  n, m : nat

list will be [ ["b"]; ["n"; "m"] ]

map will contain:
  "b" -> { ["b"], Pp.t for ": bool"; false }
  "n" -> { ["n"; "m"], Pp.t for ": nat"; false }
  "m" -> { ["n"; "m"], Pp.t for ": nat"; false }
 where the last two entries share the idents list.

concl_pp is the conclusion as a Pp.t
*)
let goal_info goal sigma =
  let map = ref StringMap.empty in
  let line_idents = ref [] in
  let build_hyp_info env sigma hyp =
    let (names, body, ty) = hyp in
    let open Pp in
    let idents = List.map (fun x -> Names.Id.to_string x) names in

    line_idents := idents :: !line_idents;
    let mid = match body with
    | Some x -> str " := " ++ pp_of_type env sigma ty ++ str " : "
    | None -> str " : " in
    let ts = pp_of_type env sigma ty in
    let rhs_pp = mid ++ ts in

    let make_entry() = { idents; rhs_pp; done_ = false } in
    List.iter (fun ident -> map := (StringMap.add ident (make_entry ()) !map); ()) idents
  in

  try
    let { ty=ty; hyps=hyps; env=env } = process_goal sigma goal in
    List.iter (build_hyp_info env sigma) (List.rev hyps); (* todo: not sure why rev is needed here *)
    let concl_pp = pp_of_type env sigma ty in
    ( List.rev !line_idents, !map, concl_pp )
  with _ -> ([], !map, Pp.mt ());;

let diff_pp o_pp n_pp =
  let open Pp in
  let o_str = string_of_ppcmds o_pp in
  let n_str = string_of_ppcmds n_pp in
  let diffs = diff_str o_str n_str in
  (add_diff_tags `Removed o_pp diffs, add_diff_tags `Added n_pp diffs);;

let diff_pp2 o_pp n_pp =
  let open Pp in
  let o_str = string_of_ppcmds o_pp in
  let n_str = string_of_ppcmds n_pp in
  let diffs = diff_str o_str n_str in
  let (_, has_removed) = has_changes diffs in
  let added = add_diff_tags `Added n_pp diffs in
  if show_removed () && has_removed then
    let removed = add_diff_tags `Removed o_pp diffs in
    (v 0 (removed ++ cut() ++ added))
  else added;;

let diff_goal_info o_info n_info =
  try
    let (o_line_idents, o_hyp_map, o_concl_pp) = o_info in
    let (n_line_idents, n_hyp_map, n_concl_pp) = n_info in
    let concl_pp = diff_pp2 o_concl_pp n_concl_pp in

    let hyp_diffs_list = diff_hyps o_line_idents o_hyp_map n_line_idents n_hyp_map in
    (hyp_diffs_list, concl_pp)
  with _ -> ([], Pp.mt ());;  (* todo: not empty! error message and return default *)

let hyp_list_to_pp hyps =
  let open Pp in
  match hyps with
  | h :: tl -> List.fold_left (fun x y -> x ++ cut () ++ y) h tl
  | [] -> mt ();;

let diff_first_goal o_proof n_proof =
  let first_goal_info proof =
    match proof with
    | None -> ([], StringMap.empty, Pp.mt ())
    | Some proof2 ->
      let (goals,_,_,_,sigma) = Proof.proof proof2 in
      match goals with
      | hd :: tl -> goal_info hd sigma;
      | _ -> ([], StringMap.empty, Pp.mt ())
  in
  diff_goal_info (first_goal_info o_proof) (first_goal_info n_proof);;

exception Diff_Failure of Pp.t (* e.g., input can't be lexed *)

(* do diffs on the first goal returning a Pp.t *)
let diff_pr_open_subgoals ?(quiet=false) o_proof n_proof =
  match n_proof with
  | None -> Pp.mt ()
  | Some proof ->
    try
      let (hyps_pp_list, concl_pp) = diff_first_goal o_proof n_proof in
      let open Pp in
      let diffs = v 0 (
        (hyp_list_to_pp hyps_pp_list) ++ cut () ++
        str "============================" ++ cut () ++
        concl_pp) in
      Printer.pr_open_subgoals2 ~quiet ~proof ~diffs ()
    with _ -> raise (Diff_Failure (Printer.pr_open_subgoals2 ~quiet ~proof ()));;

(* print the proof step, possibly with diffs highlighted, *)
let print_and_diff oldp newp =
  match newp with
  | None -> ()
  | Some proof ->
    let output =
      if show_diffs () then
        try diff_pr_open_subgoals oldp newp
        with Diff_Failure pp -> begin
          (* todo: print the unparsable string (if we know it) *)
          Feedback.msg_warning Pp.(str"Diff failure; showing results without diff highlighting");
          pp
        end
      else
        Printer.pr_open_subgoals2 ~proof () in
    Feedback.msg_notice output;;
