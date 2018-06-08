(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *   INRIA, CNRS and contributors - Copyright 1999-2018       *)
(* <O___,, *       (see CREDITS file for the list of authors)           *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

(* diff options *)

(** Controls whether to show diffs.  Takes values "on", "off", "removed" *)
val write_diffs_option : string -> unit
(** Returns true if the diffs option is "on" or "removed" *)
val show_diffs : unit -> bool

(** Computes the diff between the first goal of two Proofs and return
the highlighted hypotheses and conclusion *)
val diff_first_goal : Proof.t option -> Proof.t option -> Pp.t list * Pp.t

open Evd
open Proof_type

(** Computes the diff between two goals *)
val diff_goals : ?prev_gs:(goal sigma) -> goal sigma option -> Pp.t

(* Exposed for unit test, don't use these otherwise *)
(* output channel for the test log file *)
val log_out_ch : out_channel ref


type hyp_info = {
  idents: string list;
  rhs_pp: Pp.t;
  mutable done_: bool;
}

module StringMap :
sig
  type +'a t
  val empty: hyp_info t
  val add : string -> hyp_info -> hyp_info t -> hyp_info t
end

val diff_hyps : string list list -> hyp_info StringMap.t -> string list list -> hyp_info StringMap.t -> Pp.t list
