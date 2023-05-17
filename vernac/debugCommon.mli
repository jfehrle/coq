val set_debug : bool -> unit

val get_debug : unit -> bool

val upd_bpts : ((string * int) * bool) list -> unit

val breakpoint_stop : Loc.t option -> bool

val stepping_stop : ('a list option -> 'a list option -> 'b list * 'b list * int * int) ->
                  'a list option -> 'a list option -> DebugHook.Action.t -> bool

val set_break : bool -> unit

val action : DebugHook.Action.t ref

(* Comm module stuff *)

val init : unit -> unit

val isTerminal : unit -> bool

val read : (unit -> (string option * Loc.t option) list) ->
            (int -> (string * Pp.t) list) ->
            DebugHook.Action.t

val shift_stack : (string * Loc.t option) list -> Loc.t option ->
                 (string option * Loc.t option) list

val format_stack : (string option * Loc.t option) list ->
                  (string * (string * int list) option) list

val db_pr_goals : unit -> unit Proofview.tactic

val ssigma : Evd.evar_map option ref

val senv : Environ.env option ref
