(************************************************************************)
(*  v      *   The Coq Proof Assistant  /  The Coq Development Team     *)
(* <O___,, *   INRIA - CNRS - LIX - LRI - PPS - Copyright 1999-2018     *)
(*   \VV/  **************************************************************)
(*    //   *      This file is distributed under the terms of the       *)
(*         *       GNU Lesser General Public License Version 2.1        *)
(************************************************************************)

(** diff options **)
val write_diffs_option : string -> unit
val show_diffs : unit -> bool

exception Diff_Failure of Pp.t (* e.g., input can't be lexed *)

(** Compute the diff between two Pp.t structures and return
versions of each with diffs highlighted as (old, new) *)
val diff_pp : Pp.t -> Pp.t -> Pp.t * Pp.t

(** Compute the diff between the first goal of two Proofs and return
the highlighted hypotheses and conclusion *)
val diff_first_goal : Proof.t option -> Proof.t option -> Pp.t list * Pp.t

open Evd
open Proof_type

val diff_goals : ?prev_gs:(goal sigma) -> goal sigma option -> Pp.t

val print_and_diff : Proof.t option -> Proof.t option -> unit
val diff_pr_open_subgoals : ?quiet:bool -> Proof.t option -> Proof.t option -> Pp.t

(* Exposed for unit test, don't use these otherwise *)
(* output channel for the test log file *)
val log_out_ch : out_channel ref

module StringDiff :
sig
  type elem = String.t
end

type diff_list = StringDiff.elem Diff2.edit list

type hyp_info = {
  idents: string list;
  rhs_pp: Pp.t;
  mutable done_: bool;
}

type diff_type =
  [ `Removed
  | `Added
  | `Common
  ]

module StringMap :
sig
  type +'a t
  val empty: hyp_info t
  val add : string -> hyp_info -> hyp_info t -> hyp_info t
end

val diff_str : string -> string -> StringDiff.elem Diff2.edit list
val string_of_diffs : diff_list -> string
val has_changes : diff_list -> bool * bool
val wrap_in_bg : string -> Pp.t -> Pp.t
val tokenize_string : string -> string list
val diff_hyps : string list list -> hyp_info StringMap.t -> string list list -> hyp_info StringMap.t -> Pp.t list
