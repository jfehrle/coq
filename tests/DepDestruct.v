From MetaCoq
Require Export MetaCoq DepDestruct.

Goal forall n, 0 <= n.
MProof.
  intros n.
  new_destruct n.
Abort.

Section Bugs.

(* It is not allowing indices to be definitions, it seems *)
Goal forall n, n = S n -> False.
MProof.
  intros n H.
  Fail new_destruct H. (* fine, all indices need to be var *)
  pose (j := S n).
  assert (eq : j = S n) |1> reflexivity.
  move_back H (rewrite <- eq).
  intro H. (* now H has only indices *)
  Fail new_destruct H. (* cannot abstract non variable S n *)
Abort.

(** BUG: It fails with one constructor types, but not with two *)
Inductive one_constr : Prop :=
| the_one_constr : one_constr
.

Goal one_constr -> True.
MProof.
intros t.
new_destruct t.
Abort.

Inductive two_constrs : Prop :=
| first_constr : two_constrs
| second_constr : two_constrs
.

Goal two_constrs -> True.
MProof.
intros t.
new_destruct t.
- trivial.
- trivial.
Qed.
Unset Unicoq Debug.

End Bugs.



Section ExampleReflect.

  Inductive reflect (P :Prop) : bool -> Type :=
  | RTrue : P -> reflect P true
  | RFalse : ~P -> reflect P false.

Goal forall P b, reflect P b -> P <-> b = true.
MProof.
  intros P b r.
  new_destruct r.
  - intro xP &> split &> [reflexivity; intros &> assumption].
  - intro nxP &> split &> [intros &> contradiction; intros &> discriminate].
Qed.

  Example reflect_reflect P : ITele (SType) := iTele (fun b=>@iBase SType (reflect P b)).

  Example reflect_RTrue P : CTele (reflect_reflect P) :=
    (cProd (fun p=>@cBase SType _ (aTele _ (aBase)) (RTrue P p))).

  Example reflect_RFalse P : CTele (reflect_reflect P) :=
    (cProd (fun p=>@cBase SType _ (aTele _ (aBase)) (RFalse P p))).

  Example reflect_args P b : ATele (reflect_reflect P) :=
    aTele b aBase.

  Example bla P : RTele _ (reflect_reflect P) :=
    Eval simpl in rTele (fun b=>rBase (rsort:=SProp) (fun _=>P <-> b = true)).
  Example bla_branch P := Eval simpl in get_type_of_branch (bla P) (reflect_RTrue P).


  Example bla_RTele P b (r : reflect P b) :=
    Eval compute in eval (abstract_goal (rsort := SProp) (reflect_args P b) ((P <-> b = true)) r).

  Example bla_goals P b r : list dyn :=
    Eval compute in
      map (fun cs => Dyn (get_type_of_branch (rsort := SProp) (bla_RTele P b r) cs))
          (reflect_RTrue P :: reflect_RFalse P :: nil).

  Example reflectP_it : ITele _ :=
    iTele (fun P => iTele (fun b => iBase (sort := SType) (reflect P b))).
  Program Example reflectP_RTrue : CTele reflectP_it :=
    cProd (fun P => cProd (fun p => (cBase (aTele _ (aTele _ aBase)) (@RTrue P p)))).
  Program Example reflectP_RFalse : CTele reflectP_it :=
    cProd (fun P => cProd (fun np => (cBase (aTele _ (aTele _ aBase)) (@RFalse P np)))).
  Example reflectP_args P b : ATele reflectP_it :=
    aTele P (aTele b (aBase)).

  Example reflect_app P b := Eval compute in ITele_App (reflect_args P b).

  Example blaP_RTele P b r :=
    Eval compute in eval (abstract_goal (rsort := SProp) (reflectP_args P b) ((P <-> b = true)) r).

  Example blaP_goals P b r : list dyn :=
    Eval compute in
      map (fun cs => Dyn (get_type_of_branch (blaP_RTele P b r) cs))
          (reflectP_RFalse :: reflectP_RTrue :: nil).

  Goal True.
    MProof.
    (fun g =>
       r <- destcase (match 3 with 0 => true | S _ => false end);
       print_term r;;
                  cpose r (fun r=>idtac) g) : tactic.
    (fun g=>
       let c := reduce RedHNF r in
       case <- makecase c;
       cpose case (fun y=>idtac) g) : tactic.
  Abort.

  Goal forall P b, reflect P b -> P <-> b = true.
  Proof.
    intros P b r.
    pose (rG := eval (abstract_goal (rsort := SType) (reflect_args P b) (P <-> b = true) r)).
    simpl in rG.
    assert (T : get_type_of_branch rG (reflect_RTrue P)).
    { now firstorder. }
    assert (F : get_type_of_branch rG (reflect_RFalse P)).
    { compute. firstorder. now discriminate. }
    pose (mc :=
            makecase {|
                case_val := r;
                case_return := Dyn (RTele_Fun rG);
                case_branches := (Dyn T) :: (Dyn F) :: nil
              |}).
    compute in mc.
    pose (c := eval mc).
    unfold eval in c.
    exact (elem c).
  Qed.

Notation "'mpose' ( x := t )" := ((fun g=>r <- t; cpose r (fun x=>idtac) g) : tactic)
  (at level 40, x at next level).

Fixpoint unfold_funs {A} (t: A) (n: nat) {struct n} : M A :=
  match n with
  | 0 => ret t
  | S n' =>
    mmatch A as A' return M A' with
    | [? B (fty : B -> Type)] forall x, fty x => [H]
      let t' := reduce RedSimpl match H in _ = P return P with eq_refl => t end in (* we need to reduce this *)
      name <- fresh_name "A";
      nu name None (fun x=>
        r <- unfold_funs (t' x) n';
      abs x r)
    | [? A'] A' => [H]
      match H in _ = P return M P with eq_refl => ret t end
    end
  end.


Import TacticOverload.

(* MetaCoq version *)
Goal forall P b, reflect P b -> P <-> b = true.
MProof.
  intros P b r.
  mpose (rG := abstract_goal (rsort := SType) (reflect_args P b) (P <-> b = true) r).
  simpl.
  assert (T : get_type_of_branch rG (reflect_RTrue P)).
  { simpl. cintros x {- Tactics.split&> [cintros xP {- reflexivity -}; cintros notP {- assumption -}] -}. (* it doesn't work if intros is put outside *) }
  assert (F : get_type_of_branch rG (reflect_RFalse P)).
  { simpl. intros. Tactics.split. intros. select (~ _) (fun a=>select P (fun x=>exact (match a x with end))). intros&> discriminate. }
  mpose (return_type := unfold_funs (RTele_Fun rG) 5).
  pose (mc :=
          makecase {|
              case_val := r;
              case_return := Dyn (return_type);
              case_branches := (Dyn T) :: (Dyn F) :: nil
            |}).
  let mc := reduce RedNF mc in r <- mc; pose (c := r).
  clear mc.
  unfold_in (@get_type_of_branch) T. simpl_in T.
  unfold_in (@get_type_of_branch) F. simpl_in F.
  clear return_type.
  (* TODO: figure out why `unfold` above doesn't work anymore. *)
  (* clear rG. *)
  match c with
  | Dyn c => exact c
  end.
Abort.


End ExampleReflect.


Module VectorExample.
Require Import Vector.
Goal forall n (v : t nat n), n = length (to_list v).
Proof.
  pose (it := iTele (fun n => @iBase (SType) (t nat n))).
  pose (vnil := ((@cBase SType _ (aTele 0 aBase) (nil nat))) : CTele it).
  pose (vcons := (cProd (fun a => cProd (fun n => cProd (fun (v : t nat n) => (@cBase SType _ (aTele (S n) aBase) (cons _ a _ v)))))) : CTele it).
  fix f 2.
  intros n v.
  pose (a := (aTele n (aBase)) : ATele it).
  pose (rt := eval (abstract_goal (rsort := SProp) a (n = length (to_list v)) v)).
  simpl in vcons, rt.
  assert (N : get_type_of_branch rt vnil).
  { now auto. }
  assert (C : get_type_of_branch rt vcons).
  { intros x k v'. hnf. simpl. f_equal. exact (f _ _). }
  pose (mc :=
          makecase {|
              case_val := v;
              case_return := Dyn (RTele_Fun rt);
              case_branches := Dyn N :: Dyn C :: List.nil
            |}
       ).
  simpl RTele_Fun in mc.
  (* pose (ma := (match v as v' in t _ k return k = length (to_list v') with *)
  (*              | nil _ => N *)
  (*              | cons _ a k v => C a k v *)
  (*              end)). *)
  (* pose (c' := eval (destcase ma)). *)
  (* unfold eval in c'. *)
  pose (c := eval mc).
  unfold eval in c.
  exact (elem c).
Qed.
End VectorExample.


Example get_reflect_ITele := Eval compute in ltac:(mrun (get_ITele (reflect True))).
Example reflect_nindx := Eval compute in let (n, _) := get_reflect_ITele in n.
Example reflect_sort := Eval compute in let (sort, _) := snd get_reflect_ITele in sort.
Example reflect_itele : ITele reflect_sort :=
  Eval compute in
  match snd get_reflect_ITele as pair return let (sort, _) := pair in ITele sort with
  | existT _ s it => it
  end.
