(* tells whether Ltac Debug is set *)
let debug = ref false

let set_debug b = debug := b

let get_debug () = !debug

type breakpoint = {
  dirpath : string;  (* module dirpath *)
  offset : int;
}

module BPSet = CSet.Make(struct
  type t = breakpoint
  let compare b1 b2 =
    let c1 = Int.compare b1.offset b2.offset in
    if c1 <> 0 then c1 else String.compare b1.dirpath b2.dirpath
  end)

let breakpoints = ref BPSet.empty



(** add or remove a single breakpoint.  Maps the breakpoint from
  IDE format (absolute path name, offset) to (module dirpath, offset)
  opt - true to add, false to remove
  ide_bpt - the breakpoint (absolute path name, offset)
  *)
let update_bpt fname offset opt =
  let open Names in
  let dp =
    if fname = "ToplevelInput" then
      DirPath.make [Id.of_string "Top"]
    else begin (* find the DirPath matching the absolute pathname of the file *)
      (* ? check for .v extension? *)
      let dirname = Filename.dirname fname in
      let basename = Filename.basename fname in
      let base_id = Id.of_string (Filename.remove_extension basename) in
      DirPath.make (base_id ::
          (try
            let p = Loadpath.find_load_path (CUnix.physical_path_of_string dirname) in
            DirPath.repr (Loadpath.logical p)
          with _ -> []))
    end
  in
  let dirpath = DirPath.to_string dp in
  let bp = { dirpath; offset } in
(*  Printf.printf "update_bpt: %s -> %s  %d\n%!" fname dirpath ide_bpt.offset;*)
  match opt with
  | true  -> breakpoints := BPSet.add bp !breakpoints
  | false -> breakpoints := BPSet.remove bp !breakpoints

let upd_bpts updates =
  List.iter (fun op ->
    let ((file, offset), opt) = op in
(*    Printf.printf "Coq upd_bpts %s %d %b\n%!" file offset opt;*)
    update_bpt file offset opt;
  ) updates

let check_bpt dirpath offset =
(*    Printf.printf "In tactic_debug, dirpath = %s offset = %d\n%!" dirpath offset;*)
    BPSet.mem { dirpath; offset } !breakpoints

let break = ref false
(* causes the debugger to stop at the next step *)

let set_break b = break := b

let breakpoint_stop loc =
  if !break then begin
    break := false;
    true
  end else
    let open Loc in
    match loc with
    | Some {fname=InFile {dirpath=Some dirpath}; bp} -> check_bpt dirpath bp
    | Some {fname=ToplevelInput;                 bp} -> check_bpt "Top"   bp
    | _ -> false

let stepping_stop stacks_info stack p_stack action =
  let open DebugHook.Action in
  match action with
  | Continue -> false
  | StepIn   -> true
  | StepOver -> let st, st_prev, l_cur, l_prev = stacks_info stack p_stack in
                if l_cur = 0 || l_cur < l_prev then true (* stepped out *)
                else if l_prev = 0 (*&& l_cur > 0*) then false
                else
                  let peq = List.nth st (l_cur - l_prev) == (List.hd st_prev) in
                  (l_cur > l_prev && (not peq)) ||  (* stepped out *)
                  (l_cur = l_prev && peq)  (* stepped over *)
  | StepOut  -> let st, st_prev, l_cur, l_prev = stacks_info stack p_stack in
                if l_cur < l_prev then true
                else if l_prev = 0 then false
                else
                  List.nth st (l_cur - l_prev) != (List.hd st_prev)
  | _ -> failwith "action op"


let action = ref DebugHook.Action.StepOver

open Pp (* for str *)

module CSet = CSet.Make (Names.DirPath)
let bad_dirpaths = ref CSet.empty

let cvt_loc loc =
  let open Loc in
  match loc with
  | Some {fname=ToplevelInput; bp; ep} ->
    Some ("ToplevelInput", [bp; ep])
  | Some {fname=InFile {dirpath=None; file}; bp; ep} ->
    Some (file, [bp; ep])  (* for Load command *)
  | Some {fname=InFile {dirpath=(Some dirpath)}; bp; ep} ->
    let open Names in
    let dirpath = DirPath.make (List.rev_map (fun i -> Id.of_string i)
      (String.split_on_char '.' dirpath)) in
    let pfx = DirPath.make (List.tl (DirPath.repr dirpath)) in
    let paths = Loadpath.find_with_logical_path pfx in
    let basename = match DirPath.repr dirpath with
    | hd :: tl -> (Id.to_string hd) ^ ".v"
    | [] -> Printf.eprintf "empty name\n%!"; ""
    in
    let vs_files = List.map (fun p -> (Filename.concat (Loadpath.physical p) basename)) paths in
    let filtered = List.filter (fun p -> Sys.file_exists p) vs_files in
    begin match filtered with
    | [] -> (* todo: maybe tweak this later to allow showing a popup dialog in the GUI *)
      if not (CSet.mem dirpath !bad_dirpaths) then begin
        bad_dirpaths := CSet.add dirpath !bad_dirpaths;
        let msg = Pp.(fnl () ++ str "Unable to locate source code for module " ++
                        str (Names.DirPath.to_string dirpath)) in
        let msg = if vs_files = [] then msg else
          (List.fold_left (fun msg f -> msg ++ fnl() ++ str f) (msg ++ str " in:") vs_files) in
        Feedback.msg_warning msg
      end;
      None
    | [f] -> Some (f, [bp; ep])
    | f :: tl ->
      if not (CSet.mem dirpath !bad_dirpaths) then begin
        bad_dirpaths := CSet.add dirpath !bad_dirpaths;
        let msg = Pp.(fnl () ++ str "Multiple files found matching module " ++
            str (Names.DirPath.to_string dirpath) ++ str ":") in
        let msg = List.fold_left (fun msg f -> msg ++ fnl() ++ str f) msg vs_files in
        Feedback.msg_warning msg
      end;
      Some (f, [bp; ep]) (* be arbitrary unless we can tell which file was loaded *)
    end
  | None -> None (* nothing to highlight, e.g. not in a .v file *)

 let format_frame text loc =
   Printf.eprintf "stack frame: %s\n%!" text;
   try
     let open Loc in
       match loc with
       | Some { fname=InFile {dirpath=(Some dp)}; line_nb } ->
         let dplen = String.length dp in
         let lastdot = String.rindex dp '.' in
         let file = String.sub dp (lastdot+1) (dplen - (lastdot + 1)) in
         let module_name = String.sub dp 0 lastdot in
         let routine =
           try
             (* try text as a kername *)
             assert (CString.is_prefix dp text);
             let knlen = String.length text in
             let lastdot = String.rindex text '.' in
             String.sub text (lastdot+1) (knlen - (lastdot + 1))
           with _ -> text
         in
         Printf.sprintf "%s:%d, %s  (%s)" routine line_nb file module_name;
       | Some { fname=ToplevelInput; line_nb } ->
         let items = String.split_on_char '.' text in
         Printf.sprintf "%s:%d, %s" (List.nth items 1) line_nb (List.hd items);
       | _ -> Printf.eprintf "_ case\n%!"; text
   with _ -> text


let get_stack2 raw_stack cur_loc =
(*  Printf.printf "server: db_stack call\n%!";*)
  let rec shift s prev_loc res =
    match s with
    | (tacn, loc) :: tl ->
      shift tl loc (((Some tacn), prev_loc) :: res)
    | [] -> (None, prev_loc) :: res
  in
  List.rev (shift raw_stack cur_loc [])

let format_stack s =
  List.map (fun (tac, loc) ->
      let floc = cvt_loc loc in
      match tac with
      | Some tacn ->
        let tacn = if loc = None then
          tacn ^ " (no location)"
        else
          format_frame tacn loc in
        (tacn, floc)
      | None ->
        match loc with
        | Some { Loc.line_nb } ->
          (":" ^ (string_of_int line_nb), floc)
        | None -> (": (no location)", floc)
    ) s

(* end dependencies *)


(* Comm module *)
[@@@ocaml.warning "-32"]
let hook () = Option.get (DebugHook.Intf.get ())
let wrap = Proofview.NonLogical.make

(* TODO: ideally we would check that the debugger hooks are
   correctly set, however we don't do this yet as debugger
   initialization is unconditionally done for example in coqc.
   Improving this would require some tweaks in tacinterp which
   are out of scope for the current refactoring. *)
let init () =
  let open DebugHook in
  match Intf.get () with
  | Some intf ->
    if Intf.(intf.isTerminal) then
      action := Action.StepIn
    else begin
      set_break false;
      breakpoints := BPSet.empty;
      (hook ()).Intf.submit_answer (Answer.Init);
      while
(*        Printf.eprintf "read_cmd\n";*)
        let cmd = (hook ()).Intf.read_cmd () in
        let open DebugHook.Action in
        match cmd with
        | UpdBpts updates -> upd_bpts updates; true
        | Configd -> action := Action.Continue; false
        | _ -> failwith "Action type not allowed"
      do () done
    end
  | None -> ()
    (* CErrors.user_err
     *   (Pp.str "Your user interface does not support the Ltac debugger.") *)

open DebugHook.Intf
open DebugHook.Answer

let prompt g = wrap (fun () -> (hook ()).submit_answer (Prompt g))
let goal g = wrap (fun () -> (hook ()).submit_answer (Goal g))
let output g = wrap (fun () -> (hook ()).submit_answer (Output g))

(* routines for deferring output; output is sent only if
   the debugger stops at the next step *)
let out_queue = Queue.create ()
let defer_output f = wrap (fun () -> Queue.add f out_queue)
let print_deferred () = wrap (fun () ->
  while not (Queue.is_empty out_queue)
  do
    (hook ()).submit_answer (Output ((Queue.pop out_queue) ()))
  done)
let clear_queue () = wrap (fun () -> Queue.clear out_queue)

let print g = (hook ()).submit_answer (Output (str g))

let isTerminal () = (hook ()).isTerminal
let read get_stack get_vars =
  let rec l () =
(*    Printf.eprintf "before read\n%!";*)
    let cmd = (hook ()).read_cmd () in
(*    Printf.eprintf "after read\n%!";*)
    let open DebugHook.Action in
    match cmd with
    | Ignore -> l ()
    | UpdBpts updates -> upd_bpts updates; l ()
    | GetStack ->
      ((hook)()).submit_answer (Stack (format_stack (get_stack ())));
      l ()
    | GetVars framenum ->
      ((hook)()).submit_answer (Vars (get_vars framenum));
      l ()
    | _ -> action := cmd; cmd
  in
  Printf.eprintf "read sets action to %s\n%!" (DebugHook.Action.to_string !action);
  l ()
