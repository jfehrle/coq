val html_diffs : bool ref
val enable_html : unit -> unit

val maybe_escape : string -> bool -> string
val write_html_diffs : Proof.t option -> Proof.t option -> Loc.t option -> unit
val close_diff_files : unit -> unit
