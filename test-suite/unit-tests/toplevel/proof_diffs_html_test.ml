open OUnit
open Utest
open Proof_diffs_html

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


(* todo: other tests to write
write_html_diffs BLOCKED: CAN'T GET TAGS IN BUFFER FORMATTER
  has right tags
  splits at end of line
*)



let t () =
  let orig = "V&W<>X\"Y\'Z" in
  let escaped = maybe_escape orig true in
  let expected = "V&amp;W&lt;&gt;X&quot;Y&apos;Z" in

  assert_equal ~msg:"escapes added" ~printer:string_of_string expected escaped;
  let not_escaped = maybe_escape orig false in
  assert_equal ~msg:"escapes not added" ~printer:string_of_string orig not_escaped
let _ = add_test "maybe_escape" t


(* todo: not getting any tags from buffer formatter!!! *)
(*
let t () =
  let o_pp = seq [] in
  let n_pp = v 4 (seq [ str "a "; brk (0, 0); str "b"]) in
  let expected = seq [] in
  let (_, n_diff) = diff_pp o_pp n_pp in

  let buf = Buffer.create 16 in
  let fmt = configure_html_formatter (Format.formatter_of_buffer buf) in
  cprintf "\n\nBEFORE\n";
  Format.fprintf fmt "@[%a@]" (Pp.pp_with2 true) n_pp;
  Format.pp_print_flush fmt ();
  let actual = Buffer.contents buf in

  cprintf "'%s'\n" actual;

  assert_equal ~msg:"line break" ~printer:db_string_of_pp expected n_diff;;
let _ = add_test "html formatter line break" t
*)


let _ = run_tests __FILE__ (List.rev !tests)
