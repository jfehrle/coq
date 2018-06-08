(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** true if HTML diffs are enabled (-Xhtmldifs on the command line) *)
val html_diffs : bool ref
(** Turns on generation of HTML diffs *)
val enable_html : unit -> unit

(** Conditionally substitutes HTML escape sequences for HTML-reserved characters *)
val maybe_escape : string -> bool -> string

(** write a proof step with diffs in html to the html file *)
val write_html_diffs : Proof.t option -> Proof.t option -> Loc.t option -> unit
(** close the .v input file and the HTML output file *)
val close_diff_files : unit -> unit

(* exposed for testing *)
val configure_html_formatter : Format.formatter -> Format.formatter
