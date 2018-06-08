(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* DIFFS HTML OUTPUT*)

let html_diffs = ref false;;  (* controls whether to write diffs in html to file XXX.v.html *)
let enable_html () = html_diffs := true;;

(* Conditionally substitutes HTML escape sequences for HTML-reserved characters *)
(* todo: duplicated in Pp.ml, can I pass it in? *)
let maybe_escape str html =
  if html then
    let buf = Buffer.create 16 in
    let lim = String.length str - 1 in
    for i = 0 to lim do
      let c = str.[i] in
      Buffer.add_string buf (match c with
        | '&' -> "&amp;"
        | '<' -> "&lt;"
        | '>' -> "&gt;"
        | '\"' -> "&quot;"
        | '\'' -> "&apos;"
        | ch -> String.make 1 ch)
    done;
    Buffer.contents buf
  else
    str;;

let html_header= "<head> <style>\n\
                   diff_added { background-color: rgb(182,241,192); text-decoration: underline;}\n\
                   diff_removed { background-color: rgb(246,185,193); text-decoration: line-through;}\n\
                   diff_added_bg { background-color: rgb(233,254,238);}\n\
                   diff_removed_bg { background-color: rgb(252,233,235);}\n\
                   tac_s { color: blue; }\n\
                   db_s { color: green; }\n\
                   body { background-color: rgb(255,248,220); }\n\
                   </style> </head>\n"

(* note: similar formatter setup in topfmt.ml *)
let map_tag s = String.map (fun x -> if x = '.' then '_' else x) s

let diff_tag_stack = ref []  (* global, just like std_ft *)

let init_output_fns html_fmt =
  let reopen_highlight = ref "" in
  let open Format in
  let fns = Format.pp_get_formatter_out_functions html_fmt () in
  let newline () =
    if !diff_tag_stack <> [] then begin
      let close = String.concat "" (List.map (fun t -> "</" ^ t ^ ">") !diff_tag_stack) in
      fns.out_string close 0 (String.length close);
      reopen_highlight := String.concat "" (List.map (fun t -> "<" ^ t ^ ">") (List.rev !diff_tag_stack))
    end;
    fns.out_string "\n" 0 1 in
  let string s off n =
    if !reopen_highlight <> ""  && String.trim (String.sub s off n) <> "" then begin
      fns.out_string !reopen_highlight 0 (String.length !reopen_highlight);
      reopen_highlight := ""
    end;
    fns.out_string s off n in
  let new_fns = { fns with out_string = string; out_newline = newline; } in
  Format.pp_set_formatter_out_functions html_fmt new_fns;;

let configure_html_formatter html_fmt =
  let open Format in
  let open Pp in
  let old_out_fns = pp_get_formatter_tag_functions html_fmt () in

  Format.pp_set_mark_tags html_fmt true;
  Format.pp_set_tags html_fmt true;
  let open_tag tag =
    let (tpfx, ttag) = split_tag tag in
    if tpfx = start_pfx then diff_tag_stack := (map_tag ttag) :: !diff_tag_stack;
    if tpfx = end_pfx then "" else "<" ^ (map_tag ttag) ^ ">";
  in
  let close_tag tag =
    let (tpfx, ttag) = split_tag tag in
    if tpfx = end_pfx then diff_tag_stack := (try List.tl !diff_tag_stack with tl -> []);
    if tpfx = start_pfx then "" else "</" ^ (map_tag ttag) ^ ">"
  in
  let fns = { old_out_fns with mark_open_tag = open_tag; mark_close_tag = close_tag } in
  pp_set_formatter_tag_functions html_fmt fns;
  init_output_fns html_fmt;
  html_fmt;;

(* todo: put in a record? *)
let diff_chan = ref None
let diff_file_name = ref ""
let in_file_name = ref ""
let in_file_fd = ref None
let html_formatter = ref None

exception Trouble (* e.g. unable to write to the HTML file *)

let get_diff_chan loc =
  (diff_file_name :=
    let open Loc in
      (match loc with
      | Some loc2 ->
        (match loc2.fname with
        | InFile f -> in_file_name := f; f ^ ".html"
        | ToplevelInput -> raise Trouble)
      | None -> raise Trouble));

  match !diff_chan with
  | None -> let chan = open_out !diff_file_name in
    output_string chan html_header;
    output_string chan "<pre>";
    diff_chan := Some chan;
    html_formatter := Some (configure_html_formatter (Format.formatter_of_out_channel chan));
    chan
  | Some chan -> chan;;

let get_in_file_fd () =
  match !in_file_fd with
  | None -> let chan = Unix.openfile !in_file_name [Unix.O_RDONLY] 0 in
    in_file_fd := Some chan; chan
  | Some chan -> chan;;

(* close the .v input file and the HTML output file *)
let close_diff_files () =
  (match !in_file_fd with
  | Some chan -> Unix.close chan
  | None -> ());
  match !diff_chan with
  | Some chan -> output_string chan "</pre>"; close_out chan
  | None -> ();;

(* print tactic and diffs to file in HTML format *)
let write_html diffs loc =
  let diff_chan = get_diff_chan loc in
    (match loc with
    | Some loc2 ->
      (* print the tactic HTML-escaped *)
      let open Loc in
      let len = (loc2.ep - loc2.bp) in
      let in_bytes = Bytes.create len in
      let fd = get_in_file_fd () in
      let _ = Unix.lseek fd loc2.bp Unix.SEEK_SET in
      let _ = Unix.read fd in_bytes 0 len in
      Printf.fprintf diff_chan "%d:  <tac_s>%s</tac_s>\n\n"
        loc2.line_nb (maybe_escape (Bytes.to_string in_bytes) true);
    | None -> raise Trouble);
  (match !html_formatter with
    | Some fmt -> Format.fprintf fmt "@[%a@]" (Pp.pp_with2 true) diffs;
      Format.pp_print_flush fmt ()
    | None -> raise Trouble);

  output_string diff_chan "\n\n";
  flush diff_chan;;  (* flush less often *)

[@@@ocaml.warning "-32"]
let write_html_debug str loc =
  let diff_chan = get_diff_chan loc in
  output_string diff_chan ("\n\n**\n" ^ str ^ "\n\n");
  flush diff_chan;;  (* flush less often *)

(* write a proof step with diffs in html to the html file *)
let write_html_diffs oldp newp loc =
  let diffs =
    try
      Printer.diff_pr_open_subgoals ~quiet:true oldp newp
      (* todo: print the unparsable string (if we know it) *)
    with Pp_diff.Diff_Failure msg ->
      begin
        Feedback.msg_warning (Pp.str ("Diff failure:" ^ msg ^ "; showing results without diff highlighting" ));
        match newp with
        | Some proof -> Printer.pr_open_subgoals ~proof
        | None -> Pp.mt ()
      end
  in
  try
    write_html diffs loc
  with Trouble -> CErrors.user_err (Pp.str "Error writing HTML diffs file");;
[@@@ocaml.warning "+32"]
