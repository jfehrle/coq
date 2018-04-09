(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(*
Computes the differences between 2 Pp's and adds additional tags to a Pp
to highlight them.  Strings are split into tokens using the Coq lexer,
then the lists of tokens are diffed using the Myers algorithm.  A fixup routine,
shorten_diff_span, shortens the span of the diff result in some cases.

Highlights use 4 tags to specify the color and underline/strikeout.  These are
"diffs.added", "diffs.removed", "diffs.added.bg" and "diffs.removed.bg".  The
first two are for added or removed text; the last two are for unmodified parts
of a modified item.  Diffs that span multiple strings in the Pp are tagged with
"start.diff.*" and "end.diff.*", but only on the first and last strings of the span.

If the inputs are not acceptable to the lexer, break the strings into
lists of tokens and call diff_strs, then add_diff_tags with a Pp.t that matches
the input lists of strings.  Tokens that the lexer doesn't return exactly as they
appeared in the input will raise an exception in add_diff_tags (e.g. comments
and quoted strings).  Fixing that requires tweaking the lexer.

Limitations/Possible enhancements:

- Make diff_pp immune to unlexable strings by adding a flag to the lexer.
*)

(* DEBUG/UNIT TEST *)
let cfprintf oc = Printf.(kfprintf (fun oc -> fprintf oc "") oc)
let log_out_ch = ref stdout
let cprintf s = cfprintf !log_out_ch s


module StringDiff = Diff2.Make(struct
  type elem = String.t
  type t = elem array
  let get t i = Array.get t i
  let length t = Array.length t
end)

type diff_type =
  [ `Removed
  | `Added
  | `Common
  ]

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

(* to be set to Proof_diffs.tokenize_string to allow a forward reference to the lexer *)
let tokenize_string = ref (fun (s : string) -> [s])

(* get the Myers diff of 2 strings *)
let diff_str old_str new_str =
  let old_toks = Array.of_list (!tokenize_string old_str)
  and new_toks = Array.of_list (!tokenize_string new_str) in
  diff_strs old_toks new_toks;;

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

let wrap_in_bg diff_tag pp =
  let open Pp in
  (tag (Pp.start_pfx ^ diff_tag ^ ".bg") (str "")) ++ pp ++
  (tag (Pp.end_pfx   ^ diff_tag ^ ".bg") (str ""))

exception Diff_Failure of string

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
      | _ -> raise (Diff_Failure "invalid tag id in put_tagged, should be impossible") in
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
      raise (Diff_Failure "string mismatch, shouldn't happen")
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
  skip ();
  if !diffs <> [] then
    raise (Diff_Failure "left-over diff info at end of Pp.t, should be impossible");
  if has_added || has_removed then wrap_in_bg diff_tag rv else rv;;

module StringMap = Map.Make(String);;

let diff_pp o_pp n_pp =
  let open Pp in
  let o_str = string_of_ppcmds o_pp in
  let n_str = string_of_ppcmds n_pp in
  let diffs = diff_str o_str n_str in
  (add_diff_tags `Removed o_pp diffs, add_diff_tags `Added n_pp diffs);;

let diff_pp_combined ?(show_removed=false) o_pp n_pp =
  let open Pp in
  let o_str = string_of_ppcmds o_pp in
  let n_str = string_of_ppcmds n_pp in
  let diffs = diff_str o_str n_str in
  let (_, has_removed) = has_changes diffs in
  let added = add_diff_tags `Added n_pp diffs in
  if show_removed && has_removed then
    let removed = add_diff_tags `Removed o_pp diffs in
    (v 0 (removed ++ cut() ++ added))
  else added;;
