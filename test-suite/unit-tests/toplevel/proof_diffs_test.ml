open OUnit
open Utest
open Proof_diffs

let tests = ref []
let add_test name test = tests := (mk_test name (TestCase test)) :: !tests
let cfprintf oc = Printf.(kfprintf (fun oc -> fprintf oc "") oc)
let log_out_ch = ref stdout
let cprintf s = cfprintf !log_out_ch s
let string_of_string s : string = "\"" ^ s ^ "\""

let t () =
  (* set up printing into log file *)
  (log_out_ch :=  match !Utest.log_out_ch with
                 | Some ch -> ch
                 | None -> stdout);
  Proof_diffs.log_out_ch := !log_out_ch
let _ = add_test "setup" t

(* todo: would be nice to get the line number from a stack trace *)
(* todo: why can't the body of "v" be given in the add_test? *)
let t () =
  let expected : diff_list = [] in
  let diffs = diff_str "" "   " in

  assert_equal ~msg:"empty" ~printer:string_of_diffs expected diffs;
  let (has_added, has_removed) = has_changes diffs in
  assert_equal ~msg:"has `Added" ~printer:string_of_bool false has_added;
  assert_equal ~msg:"has `Removed" ~printer:string_of_bool false has_removed
let _ = add_test "diff_str empty" t


let t () =
  let expected : diff_list =
    [ `Common (0, 0, "a"); `Common (1, 1, "b"); `Common (2, 2, "c")] in
  let diffs = diff_str "a b c" " a  b\t  c\n" in

  assert_equal ~msg:"white space" ~printer:string_of_diffs expected diffs;
  let (has_added, has_removed) = has_changes diffs in
  assert_equal ~msg:"no `Added" ~printer:string_of_bool false has_added;
  assert_equal ~msg:"no `Removed" ~printer:string_of_bool false has_removed
let _ = add_test "diff_str white space" t

let t () =
  let expected : diff_list = [ `Removed (0, "a"); `Added (0, "b")] in
  let diffs = diff_str "a" "b" in

  assert_equal ~msg:"add/remove" ~printer:string_of_diffs expected diffs;
  let (has_added, has_removed) = has_changes diffs in
  assert_equal ~msg:"has `Added" ~printer:string_of_bool true has_added;
  assert_equal ~msg:"has `Removed" ~printer:string_of_bool true has_removed
let _ = add_test "diff_str add/remove" t

(* example of a limitation, not really a test *)
let t () =
  try
    let _ = diff_str "a" "&gt;" in
    assert_failure "unlexable string gives an exception"
  with _ -> ()
let _ = add_test "diff_str unlexable" t

(* problematic examples for tokenize_string:
   comments omitted
   quoted string loses quote marks (are escapes supported/handled?)
   char constant split into 2
   *)
let t () =
  List.iter (fun x -> cprintf "'%s' " x) (tokenize_string "(* comment *) \"string\" 'c' xx");
  cprintf "\n"
let _ = add_test "tokenize_string examples" t

open Pp

(* note pp_to_string concatenates adjacent strings, could become one token,
e.g. str " a" ++ str "b " will give a token "ab" *)
(* checks background is present and correct *)
let t () =
  let o_pp = str "a" ++ str "!" ++ str "c" in
  let n_pp = str "a" ++ str "?" ++ str "c" in
  let (o_exp, n_exp) = (wrap_in_bg "diff.removed" (str "a" ++ (tag "diff.removed" (str "!")) ++ str "c"),
                        wrap_in_bg "diff.added" (str "a" ++ (tag "diff.added" (str "?")) ++ str "c")) in
  let (o_diff, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"removed" ~printer:db_string_of_pp o_exp o_diff;
  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp n_diff
let _ = add_test "diff_pp/add_diff_tags add/remove" t

let t () =
  (*Printf.printf "%s\n" (string_of_diffs (diff_str "a d" "a b c d"));*)
  let o_pp = str "a" ++ str " d" in
  let n_pp = str "a" ++ str " b " ++ str " c " ++ str "d" ++ str " e " in
  let n_exp = flatten (wrap_in_bg "diff.added" (seq [
      str "a";
      str " "; (tag "start.diff.added" (str "b "));
      (tag "end.diff.added" (str " c")); str " ";
      (str "d");
      str " "; (tag "diff.added" (str "e")); str " "
      ])) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff);;
let _ = add_test "diff_pp/add_diff_tags a span with spaces" t


let t () =
  let o_pp = str " " in
  let n_pp = tag "sometag" (str "a") in
  let n_exp = flatten (wrap_in_bg "diff.added" (tag "diff.added" (tag "sometag" (str "a")))) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff)
let _ = add_test "diff_pp/add_diff_tags diff tags outside existing tags" t

let t () =
  let o_pp = str " " in
  let n_pp = seq [(tag "sometag" (str " a ")); str "b"] in
  let n_exp = flatten (wrap_in_bg "diff.added"
      (seq [tag "sometag" (str " "); (tag "start.diff.added" (tag "sometag" (str "a ")));
          (tag "end.diff.added" (str "b"))]) ) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff)
let _ = add_test "diff_pp/add_diff_tags existing tagged values with spaces" t

let t () =
  let o_pp = str " " in
  let n_pp = str " a b " in
  let n_exp = flatten (wrap_in_bg "diff.added"
      (seq [str " "; tag "diff.added" (str "a b"); str " "])) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff)
let _ = add_test "diff_pp/add_diff_tags multiple tokens in pp" t

let t () =
  let o_pp = str "a d" in
  let n_pp = seq [str "a b"; str "c d"] in
  let n_exp = flatten (wrap_in_bg "diff.added"
      (seq [str "a "; tag "start.diff.added" (str "b");
            tag "end.diff.added" (str "c"); str " d"])) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff)
let _ = add_test "diff_pp/add_diff_tags token spanning multiple Ppcmd_strs" t

let t () =
  let o_pp = seq [str ""; str "a"] in
  let n_pp = seq [str ""; str "a b"] in
  let n_exp = flatten (wrap_in_bg "diff.added"
      (seq [str ""; str "a "; tag "diff.added" (str "b")])) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff)
let _ = add_test "diff_pp/add_diff_tags empty string preserved" t

(* todo: won't work unless the lexer returns the quotes of the string token *)
(*
let t () =
  let s = "\"a b\"" in
  let o_pp = seq [str s] in
  let n_pp = seq [str "\"a b\" "] in
  cprintf "ppcmds: %s\n" (string_of_ppcmds n_pp);
  let n_exp = flatten (wrap_in_bg "diff.added"
      (seq [str ""; str "a "; tag "diff.added" (str "b")])) in
  let (_, n_diff) = diff_pp o_pp n_pp in

  assert_equal ~msg:"string" ~printer:string_of_string "a b" (List.hd (tokenize_string s));
  assert_equal ~msg:"added"   ~printer:db_string_of_pp n_exp (flatten n_diff)
let _ = add_test "diff_pp/add_diff_tags token containing white space" t
*)

let add_entries map idents rhs_pp =
  let make_entry() = { idents; rhs_pp; done_ = false } in
  List.iter (fun ident -> map := (StringMap.add ident (make_entry ()) !map); ()) idents


(* a : foo
   b : bar car ->
   b : car
   a : foo bar *)
let t () =
  write_diffs_option "removed";   (* turn on "removed" option *)
  let o_line_idents = [ ["a"]; ["b"]] in
  let o_hyp_map = ref StringMap.empty in
  add_entries o_hyp_map ["a"] (str " : foo");
  add_entries o_hyp_map ["b"] (str " : bar car");
  let n_line_idents = [ ["b"]; ["a"]] in
  let n_hyp_map = ref StringMap.empty in
  add_entries n_hyp_map ["b"] (str " : car");
  add_entries n_hyp_map ["a"] (str " : foo bar");
  let expected = [flatten (wrap_in_bg "diff.removed" (seq [str "b"; str " : "; (tag "diff.removed" (str "bar")); str " car" ]));
                  flatten (wrap_in_bg "diff.added" (seq [str "b"; str " : car" ]));
                  flatten (wrap_in_bg "diff.added" (seq [str "a"; str " : foo "; (tag "diff.added" (str "bar")) ]))
  ] in

  let hyps_diff_list = diff_hyps o_line_idents !o_hyp_map n_line_idents !n_hyp_map in

  (*List.iter (fun x -> cprintf "%s\n" (string_of_ppcmds (flatten x))) hyps_diff_list;*)
  (*List.iter (fun x -> cprintf "%s\n" (db_string_of_pp (flatten x))) hyps_diff_list;*)

  List.iter2 (fun exp act ->
      assert_equal ~msg:"added"   ~printer:db_string_of_pp exp (flatten act))
      expected hyps_diff_list
let _ = add_test "diff_hyps simple diffs" t

(* a : nat
  c, d : int ->
  a, b : nat
  d : int *)
  (* todo: getting "d, c : int", not "c, d : int".  Not keeping the ordering for old *)
let t () =
  write_diffs_option "removed";   (* turn on "removed" option *)
  let o_line_idents = [ ["a"]; ["c"; "d"]] in
  let o_hyp_map = ref StringMap.empty in
  add_entries o_hyp_map ["a"] (str " : nat");
  add_entries o_hyp_map ["c"; "d"] (str " : int");
  let n_line_idents = [ ["a"; "b"]; ["d"]] in
  let n_hyp_map = ref StringMap.empty in
  add_entries n_hyp_map ["a"; "b"] (str " : nat");
  add_entries n_hyp_map ["d"] (str " : int");
  let expected = [flatten (wrap_in_bg "diff.added" (seq [str "a"; (tag "start.diff.added" (str ", ")); (tag "end.diff.added" (str "b")); str " : nat" ]));
                  flatten (wrap_in_bg "diff.removed" (seq [str "d"; (tag "start.diff.removed" (str ", "));  (tag "end.diff.removed" (str "c")); str " : int" ]));
                  flatten (wrap_in_bg "diff.added" (seq [str "d"; str " : int" ]))
  ] in

  let hyps_diff_list = diff_hyps o_line_idents !o_hyp_map n_line_idents !n_hyp_map in

  (*List.iter (fun x -> cprintf "%s\n" (string_of_ppcmds (flatten x))) hyps_diff_list;*)
  (*List.iter (fun x -> cprintf "%s\n" (db_string_of_pp (flatten x))) hyps_diff_list;*)

  List.iter2 (fun exp act ->
      assert_equal ~msg:"added"   ~printer:db_string_of_pp exp (flatten act))
      expected hyps_diff_list
let _ = add_test "diff_hyps compacted" t

(* a : foo
   b : bar
   c : nat ->
   b, a, c : nat   and keeps order *)
   (* todo: not sure if I like b : bar -- is this a realistic use case?
                               b : nat
                               a : foo
                               a : nat
                               c : nat *)
(*
let t () =
  write_diffs_option "removed";   (* turn on "removed" option *)
  let o_line_idents = [ ["a"]; ["b"]; ["c"]] in
  let o_hyp_map = ref StringMap.empty in
  add_entries o_hyp_map ["a"] (str " : foo");
  add_entries o_hyp_map ["b"] (str " : bar");
  add_entries o_hyp_map ["c"] (str " : nat");
  let n_line_idents = [ ["b"; "a"; "c"] ] in
  let n_hyp_map = ref StringMap.empty in
  add_entries n_hyp_map ["b"; "a"; "c"] (str " : nat");
  let expected = [flatten (wrap_in_bg "diff.added" (seq [str "a"; (tag "start.diff.added" (str ", ")); (tag "end.diff.added" (str "b")); str " : nat" ]));
                  flatten (wrap_in_bg "diff.removed" (seq [str "d"; (tag "start.diff.removed" (str ", "));  (tag "end.diff.removed" (str "c")); str " : int" ]));
                  flatten (wrap_in_bg "diff.added" (seq [str "d"; str " : int" ]))
  ] in

  let hyps_diff_list = diff_hyps o_line_idents !o_hyp_map n_line_idents !n_hyp_map in

  List.iter (fun x -> cprintf "%s\n" (string_of_ppcmds (flatten x))) hyps_diff_list;
  List.iter (fun x -> cprintf "%s\n" (db_string_of_pp (flatten x))) hyps_diff_list;

  List.iter2 (fun exp act ->
      assert_equal ~msg:"added"   ~printer:db_string_of_pp exp (flatten act))
      expected hyps_diff_list
let _ = add_test "diff_hyps compacted with join" t
*)

(* b, a, c : nat ->
   a : foo
   b : bar
   c : nat and keeps order *)
   (* todo: not sure if I like a : nat -- is this a realistic use case?
                               a : foo
                               b : nat
                               b : bar
                               c : nat *)
(*
let t () =
  write_diffs_option "removed";   (* turn on "removed" option *)
  let o_line_idents = [ ["b"; "a"; "c"] ] in
  let o_hyp_map = ref StringMap.empty in
  add_entries o_hyp_map ["b"; "a"; "c"] (str " : nat");
  let n_line_idents = [ ["a"]; ["b"]; ["c"]] in
  let n_hyp_map = ref StringMap.empty in
  add_entries n_hyp_map ["a"] (str " : foo");
  add_entries n_hyp_map ["b"] (str " : bar");
  add_entries n_hyp_map ["c"] (str " : nat");
  let expected = [flatten (wrap_in_bg "diff.added" (seq [str "a"; (tag "start.diff.added" (str ", ")); (tag "end.diff.added" (str "b")); str " : nat" ]));
                  flatten (wrap_in_bg "diff.removed" (seq [str "d"; (tag "start.diff.removed" (str ", "));  (tag "end.diff.removed" (str "c")); str " : int" ]));
                  flatten (wrap_in_bg "diff.added" (seq [str "d"; str " : int" ]))
  ] in

  let hyps_diff_list = diff_hyps o_line_idents !o_hyp_map n_line_idents !n_hyp_map in

  List.iter (fun x -> cprintf "%s\n" (string_of_ppcmds (flatten x))) hyps_diff_list;
  List.iter (fun x -> cprintf "%s\n" (db_string_of_pp (flatten x))) hyps_diff_list;

  List.iter2 (fun exp act ->
      assert_equal ~msg:"added"   ~printer:db_string_of_pp exp (flatten act))
      expected hyps_diff_list
let _ = add_test "diff_hyps compacted with split" t
*)


(* todo: other tests to write
coqtop/terminal formatting BLOCKED: CAN'T GET TAGS IN FORMATTER
  white space at end of line
  spanning diffs
shorten_diff_span

MAYVE NOT WORTH IT
diff_pp/add_diff_tags
  add/remove - show it preserves, recurs and processes:
    nested in boxes
    breaks, etc. preserved
diff_pp2 with/without removed
*)


let _ = run_tests __FILE__ (List.rev !tests)
