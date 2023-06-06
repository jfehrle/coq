val set_debug : bool -> unit

val get_debug : unit -> bool

val upd_bpts : ((string * int) * bool) list -> unit

val breakpoint_stop : Loc.t option -> bool

val stepping_stop : ('a list option -> 'a list option -> 'b list * 'b list * int * int) ->
                  'a list option -> 'a list option -> DebugHook.Action.t -> bool
(*                  Loc.t option list -> Loc.t option list -> DebugHook.Action.t -> bool *)

val set_break : bool -> unit

val action : DebugHook.Action.t ref

(* Comm module stuff *)

val init : unit -> unit

val isTerminal : unit -> bool

module Val : Dyn.S

module FmtVars : sig
  type 'a tag = 'a Val.tag
  type 'a fmt = 'a Names.Id.Map.t -> (string * Pp.t) list

  (** [add tag fmt] registers a function to format the variables for a debugger
      type (e.g. Ltac1/Ltac2) [tag]. If there already was such a printer, it is replaced. *)
  val add : 'a tag -> 'a fmt -> unit

  (** [find tag] gives the currently registered format the variables for a debugger
      type (e.g. Ltac1/Ltac2) [tag] if there is one, and raises [Not_found] otherwise. *)
  val find : 'a tag -> 'a fmt

end

val read : (unit -> (string option * Loc.t option) list) ->
            (int -> (string * Pp.t) list) ->
            DebugHook.Action.t

val shift_stack : (string * Loc.t option) list -> Loc.t option ->
                 (string option * Loc.t option) list

val format_stack : (string option * Loc.t option) list ->
                  (string * (string * int list) option) list

val db_pr_goals : unit -> unit Proofview.tactic

val ssigma : Evd.evar_map option ref  (* todo: remove? *)

val senv : Environ.env option ref

(** Push a loc chunk (multiple frames) onto the loc chunk stack *)
val push_loc_chunk : Loc.t option list -> unit

(** Pop a loc chunk (multiple frames) from the loc chunk stack *)
val pop_loc_chunk : unit -> unit
