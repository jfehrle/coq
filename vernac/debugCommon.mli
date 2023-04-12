val upd_bpts : ((string * int) * bool) list -> unit

val check_bpt : string -> int -> bool

val get_break : unit -> bool

val set_break : bool -> unit

val action : DebugHook.Action.t ref

(* Comm module stuff *)


val init : unit -> unit

val isTerminal : unit -> bool

val read : (unit -> (string option * Loc.t option) list) ->
            (int -> (string * Pp.t) list) ->
            DebugHook.Action.t

val get_stack2 : (string * Loc.t option) list -> Loc.t option ->
                 (string option * Loc.t option) list

val format_stack: (string option * Loc.t option) list ->
                  (string * (string * int list) option) list
