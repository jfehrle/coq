open Tacexpr

(*
let (* rec *) dump_glob_constr ?(indent = 0) CAst.{v=m} =
  let print ?(indent=indent) s = Printf.eprintf "%s%s\n%!" (String.make indent ' ') s in
  let open Glob_term in
  let open DAst in
  match m with
  | Value _ -> print "Value"
  | Thunk _ -> print "Thunk"
*)
(*
  match v with
  | GRef _ -> print "GRef"
  | GVar _ -> print "GVar"
  | _ -> print "???"
*)


let (* rec *) dump_may_eval ?(indent=0) m =
  let print ?(indent=indent) s = Printf.eprintf "%s%s\n%!" (String.make indent ' ') s in
(*  let indent = indent + 2 in *)
(*  let recur = dump_gen_tactic_arg ~indent in *)
  match m with
  | ConstrTerm (glob_constr, constr_expr_opt) ->
    print "ConstrTerm";
(*    dump_glob_constr ~indent glob_constr; *)
  | ConstrEval (a,b) -> print "ConstrEval"
  | ConstrContext (a,b) -> print "ConstrContext"
  | ConstrTypeOf a -> print "ConstrTypeOf"

let (* rec *) dump_gen_tactic_arg ?(indent=0) a =
  let print ?(indent=indent) s = Printf.eprintf "%s%s\n%!" (String.make indent ' ') s in
  let indent = indent + 2 in
(*  let recur = dump_gen_tactic_arg ~indent in *)
  match a with
  | TacGeneric (isquot,arg) -> print "TacGeneric"
  | ConstrMayEval c -> print "ConstrMayEval";
    dump_may_eval ~indent c
  | Reference r -> print "Reference"
  | TacCall c -> print "TacCall"
  | TacFreshId x -> print "TacFreshId"
  | Tacexp t -> print "Tacexp"
  | TacPretype c -> print "TacPretype"
  | TacNumgoals -> print "TacNumgoals"

let dump_message_token ?(indent=0) t =
  let print ?(indent=indent) s = Printf.eprintf "%s%s\n%!" (String.make indent ' ') s in
  let pr = print ~indent in
  match t with
  | MsgString s -> pr (Printf.sprintf "MsgString \"%s\" " s);
  | MsgInt i -> pr (Printf.sprintf "MsgInt %d " i);
  | MsgIdent id -> pr (Printf.sprintf "MsgIdent %s " (Pp.string_of_ppcmds (Pputils.pr_lident id)))

let rec dump_gen_tactic_expr  ?(indent=0) CAst.{v=e} =
  let print ?(indent=indent) s = Printf.eprintf "%s%s\n%!" (String.make indent ' ') s in
  let pr = print ~indent in
  let indent = indent + 2 in
  let recur = dump_gen_tactic_expr ~indent in
  match e with
  | TacAtom t -> pr "TacAtom"
  | TacFun tacfun ->pr "TacFun"
  | TacLetIn (r,l,u) ->
    let open Pp in
    let names = List.fold_left (fun acc CAst.({v=name}, _) -> acc ++ (Names.Name.print name) ++ str " ") (mt ()) l in
    pr (Printf.sprintf "TacLetIn %s" (string_of_ppcmds names));
    List.iter (fun (_, arg) -> dump_gen_tactic_arg ~indent arg) l;
    recur u
  | TacMatchGoal (lz,lr,lmr) -> pr "TacMatchGoal"
  | TacMatch (lz,c,lmr) -> pr "TacMatch"
  | TacId mtl -> pr "TacId";
    List.iter (fun i -> dump_message_token ~indent i) mtl;
    Printf.eprintf "\n"
  | TacFail _ -> pr "TacFail"
  | TacProgress tac -> pr "TacProgress"
  | TacAbstract (tac,s) -> pr "TacAbstract"
  | TacThen (t1,t2) -> pr "TacThen"
  | TacDispatch tl -> pr "TacDispatch"
  | TacExtendTac (tf,t,tl) -> pr "TacExtendTac"
  | TacThens (t,tl) -> pr "TacThens"
  | TacThens3parts (t1,tf,t2,tl) -> pr "TacThens3parts"
  | TacDo (n,tac) -> pr "TacDo"
  | TacTimeout (n,tac) -> pr "TacTimeout"
  | TacTime (s,tac) -> pr "TacTime"
  | TacTry tac -> pr "TacTry"
  | TacRepeat tac -> pr "TacRepeat"
  | TacOr (tac1,tac2) -> pr "TacOr"
  | TacOnce tac -> pr "TacOnce"
  | TacExactlyOnce tac -> pr "TacExactlyOnce"
  | TacIfThenCatch (tac,tact,tace) -> pr "TacIfThenCatch"
  | TacOrelse (tac1,tac2) -> pr "TacOrelse"
  | TacFirst l -> pr "TacFirst"
  | TacSolve l -> pr "TacSolve"
  | TacArg a -> pr "TacArg";
    dump_gen_tactic_arg ~indent a
  | TacSelect (s, tac) -> pr "TacSelect"
  | TacAlias (s,l) -> pr "TacAlias"
  | TacML (opn,l) -> pr "TacML"
