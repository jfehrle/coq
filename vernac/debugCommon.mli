val set_debug : bool -> unit

val get_debug : unit -> bool

val upd_bpts : ((string * int) * bool) list -> unit

val breakpoint_stop : Loc.t option -> bool

val stepping_stop : Loc.t option list -> Loc.t option list -> bool

val set_break : bool -> unit

val action : DebugHook.Action.t ref

(* Comm module stuff *)

val init : unit -> unit

val isTerminal : unit -> bool

type formatted_stack = DebugHook.Answer.stack
type formatted_vars = DebugHook.Answer.vars

module Val : Dyn.S

module Stack : sig
  type 'a tag = 'a Val.tag
  type 'a stack_frame = 'a
  type 'a stack_chunk = 'a tag * 'a list
  type 'a fmt_stack = 'a list -> formatted_stack
  type 'a fmt_vars = 'a -> formatted_vars

  (** [add tag fmt] registers interface functions for a debugger
      type (e.g. Ltac1/Ltac2) [tag]. If there already was such a printer, it is replaced. *)
  val add : 'a tag -> 'a fmt_stack -> 'a fmt_vars -> unit

  (** [find tag] gives the currently registered interface functions for a debugger
      type (e.g. Ltac1/Ltac2) [tag] if there is one, and raises [Not_found] otherwise. *)
  val find : 'a tag -> 'a fmt_stack * 'a fmt_vars

end


val read : 'a Stack.tag -> DebugHook.Action.t

val shift_stack : (string * Loc.t option) list -> Loc.t option ->
                 (string option * Loc.t option) list

val format_stack : (string option * Loc.t option) list -> formatted_stack

val db_pr_goals : unit -> unit Proofview.tactic

val ssigma : Evd.evar_map option ref  (* todo: remove? *)

val senv : Environ.env option ref

type from = Ltac1 | Ltac2

(** Push a loc chunk (multiple frames) onto the loc chunk stack *)
val push_loc_chunk : Loc.t option list -> from -> unit

(** Pop a loc chunk (multiple frames) from the loc chunk stack *)
val pop_loc_chunk : unit -> unit
