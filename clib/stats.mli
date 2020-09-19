val init : unit -> unit
val set_infiles : string list -> unit
val get_stats_enabled : unit -> bool

val print_stats : unit -> unit

(* callbacks for generating statistics *)
val parser_action : string -> int -> int -> unit
val parser_ext : string -> string -> string -> int -> unit

(*
type list_type =
  [ SList1
  | SList1Sep
  | SList0
  | Slist0Sep
  | Opt
  ]
*)

val got_list :  string -> int -> unit

val got_token : string -> unit

val got_loc : Loc.t -> string -> unit

val check_stack : unit -> unit

type pid = {
  file : string;
  char : int;
}

val pid_compare : pid -> pid -> int
