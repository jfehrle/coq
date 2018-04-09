(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(** Computes the diff between two Pp.t structures and return
versions of each with diffs highlighted as (old, new) *)
val diff_pp : Pp.t -> Pp.t -> Pp.t * Pp.t

(** Computes the diff between two Pp.t structures and return
a highlighted Pp.t.  If [show_removed] is true, show separate lines for
removals and additions, otherwise only show additions *)
val diff_pp_combined : ?show_removed:bool -> Pp.t -> Pp.t -> Pp.t

(** Raised if the diff fails, e.g. the input can't be lexed *)
exception Diff_Failure of string

(* for dependency injection to allow calling the lexer *)
val tokenize_string : (string -> string list) ref

module StringDiff :
sig
  type elem = String.t
  type t = elem array
end

type diff_type =
  [ `Removed
  | `Added
  | `Common
  ]

type diff_list = StringDiff.elem Diff2.edit list

val diff_str : string -> string -> StringDiff.elem Diff2.edit list
val diff_strs : StringDiff.t -> StringDiff.t -> StringDiff.elem Diff2.edit list
val add_diff_tags : diff_type -> Pp.t -> StringDiff.elem Diff2.edit list -> Pp.t
val has_changes : diff_list -> bool * bool
val get_dinfo : StringDiff.elem Diff2.edit -> diff_type * string
val wrap_in_bg : string -> Pp.t -> Pp.t
val string_of_diffs : diff_list -> string
