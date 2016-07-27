From MetaCoq
Require Export MetaCoq MCListUtils MCTactics ImportedTactics.
Import MetaCoqNotations.
Import MCTacticsNotations.

Require Import Strings.String.

Require Import Lists.List.
Import ListNotations.

Set Printing Universes.
Section Sorts.
  Inductive Sort : Type := SProp | SType.
  Polymorphic Definition type_of@{type_of1} {A : Type@{type_of1}} (x : A) : Type@{type_of1} := A.
  Polymorphic Definition stype_of@{stype_of1 stype_of2} (s : Sort) : Type@{stype_of2}
    := match s with SType => Type@{stype_of1} | SProp => Prop end.
  Polymorphic Definition selem_of@{selem_of1 selem_of2} {s : Sort} (x : stype_of@{selem_of1 selem_of2} s) : Type@{selem_of2} :=
    match s return stype_of@{selem_of1 selem_of2} s -> Type@{selem_of2} with
    | SType => fun x => x
    | SProp => fun x => x
    end x.

  Polymorphic Definition ForAll@{ForAll_A ForAll_st1 ForAll_st2 ForAll_max1 ForAll_max2}
              {sort : Sort} {A : Type@{ForAll_A}} :
    (A -> stype_of@{ForAll_st1 ForAll_st2} sort) -> stype_of@{ForAll_max1 ForAll_max2} sort :=
    match
      sort as sort'
      return ((A -> stype_of@{ForAll_st1 ForAll_st2} sort') -> stype_of@{ForAll_max1 ForAll_max2} sort')
    with
    | SProp => fun F => forall a : A, F a
    | SType => fun F => forall a : A, F a
    end.

  Polymorphic Definition Fun@{Fun_A Fun_st1 Fun_st2 Fun_max1 Fun_max2} {sort} {A : Type@{Fun_A}} :
    forall {F : A -> stype_of sort}, (forall a, selem_of (F a)) -> selem_of (ForAll@{Fun_A Fun_st1 Fun_st2 Fun_max1 Fun_max2} F) :=
    match sort as sort' return
          forall {F : A -> stype_of sort'}, (forall a, selem_of (F a)) -> selem_of (ForAll F)
    with
    | SProp => fun _ f => f
    | SType => fun _ f => f
    end.

  Polymorphic Definition App@{App_A App_st1 App_st2 App_max1 App_max2} {sort} {A : Type@{App_A}} : forall {F : A -> _},  selem_of (ForAll@{App_A App_st1 App_st2 App_max1 App_max2} (sort := sort) F) -> forall a, selem_of (F a) :=
    match sort as sort' return forall F, selem_of (ForAll (sort := sort') F) -> forall a, selem_of (F a) with
    | SProp => fun F f a => f a
    | SType => fun F f a => f a
    end.
End Sorts.

Polymorphic Inductive ITele@{it_base1 it_base2 it_tele it_max} (sort : Sort) : Type@{it_max} :=
| iBase : stype_of@{it_base1 it_base2} sort -> ITele sort
| iTele : forall {T : Type@{it_tele}}, (T -> ITele sort) -> ITele sort.

Delimit Scope ITele_scope with IT.
Bind Scope ITele_scope with ITele.
Arguments iBase {_} _.
Arguments iTele {_ _%type} _.

Polymorphic Inductive ATele@{at_base1 at_base2 at_tele at_max} {sort} : ITele@{at_base1 at_base2 at_tele at_max} sort -> Type :=
| aBase : forall {T: stype_of sort}, ATele (iBase T)
| aTele : forall {T : Type@{at_tele}} {f : T -> ITele sort} (a:T), ATele (f a) -> ATele (iTele f).
Delimit Scope ATele_scope with AT.
Bind Scope ATele_scope with ATele.
Arguments ATele {_} _%IT.
Arguments aBase {_ _}.
Arguments aTele {_ _%type _} _%AT _.

(* it_FT_res1 and it_FT_res2 will be equal to
    it_FT_base1 and it_FT_base2.
    However, Coq does not realize that there is no need to have them.
    Accordingly, we put them in for now.
  *)
Polymorphic Definition ITele_Fun_Type@{it_FT_base1 it_FT_base2 it_FT_tele it_FT_max it_FT_res1 it_FT_res2} {isort} : ITele@{it_FT_base1 it_FT_base2 it_FT_tele it_FT_max} isort -> Type@{it_FT_res2} :=
  fix rec it :=
    match it with
    | iBase T => stype_of@{it_FT_res1 it_FT_res2} isort
    | iTele f => forall t, rec (f t)
    end.

Polymorphic Definition ITele_Fun_App@{it_FA_base1 it_FA_base2 it_FA_tele it_FA_max} {isort} : forall {it : ITele@{it_FA_base1 it_FA_base2 it_FA_tele it_FA_max} isort}, ITele_Fun_Type it :=
  fix rec it :=
    match it as it' return ITele_Fun_Type it' with
    | iBase T => T
    | iTele f => fun t => rec (f t)
    end.

Polymorphic Fixpoint ITele_App@{it_A_base1 it_A_base2 it_A_tele it_A_max} {isort} {it : ITele isort} (args : ATele@{it_A_base1 it_A_base2 it_A_tele it_A_max} it) : stype_of isort :=
  match args with
  | @aBase _ T => T
  | @aTele _ _ f v args =>
    ITele_App args
  end.

Polymorphic Inductive CTele@{ct_base1 ct_base2 ct_tele ct_max ct_prod ct_max_all} {sort} (it : ITele@{ct_base1 ct_base2 ct_tele ct_max} sort) : Type@{ct_max_all} :=
| cBase : forall {a : ATele it} (c : selem_of (ITele_App a)), CTele it
| cProd : forall {T : Type@{ct_prod}}, (T -> CTele it) -> CTele it.
Delimit Scope CTele_scope with CT.
Bind Scope CTele_scope with CTele.
Arguments CTele {_} _%IT.
Arguments cBase {_ _%AT} _ _.
Arguments cProd {_ _%type _} _.

Polymorphic Inductive RTele@{rt_base1 rt_base2 rt_tele rt_max rt_type1 rt_type2 rt_max_all} {isort} rsort : ITele@{rt_base1 rt_base2 rt_tele rt_max} isort -> Type@{rt_max_all} :=
| rBase : forall {T : stype_of isort}, (selem_of T -> stype_of@{rt_type1 rt_type2} rsort) -> RTele rsort (iBase T)
| rTele : forall {T:Type@{rt_tele}} {f}, (forall (t : T), RTele rsort (f t)) -> RTele rsort (iTele f).
Delimit Scope RTele_scope with RT.
Bind Scope RTele_scope with RTele.
Arguments RTele {_} _ _%IT.
Arguments rBase {_ _ _} _.
Arguments rTele {_ _ _%type _} _.

Polymorphic Fixpoint RTele_App@{rt_A_base1 rt_A_base2 rt_A_tele rt_A_max rt_A_type1 rt_A_type2 rt_A_max_all} {isort rsort} {it : ITele isort} (rt : RTele@{rt_A_base1 rt_A_base2 rt_A_tele rt_A_max rt_A_type1 rt_A_type2 rt_A_max_all} rsort it) : forall (a : ATele it), selem_of (ITele_App a) -> stype_of rsort :=
  match rt in RTele _ it'
  with
  | @rBase _ _ T t =>
    fun (a : ATele (iBase T)) =>
      match a as a' in ATele it' return
            match it' with
            | iBase T' => (selem_of T' -> stype_of rsort) -> selem_of (ITele_App a') -> stype_of rsort
            | iTele f => True
            end
      with
      | aBase => fun f => f
      | aTele _ _ => I
      end t
  | rTele r =>
    let rec t := RTele_App (r t) in
    fun (a : ATele (iTele _)) =>
      match a as a' in ATele it' return
            match it' with
            | iBase _ => True
            | @iTele _ T' f => (forall (t:T') (a:ATele (f t)), selem_of (ITele_App a) -> _) -> selem_of (ITele_App a') -> stype_of rsort
            end
      with
      | aBase => I
      | aTele v a => fun rec => rec v a
      end rec
  end.

(* rt_T_weird1 and rt_T_weird2 will be equal to
    rt_T_type1 and rt_T_type2.
    Again, Coq does not realize that. So we leave them in for now.
  *)
Polymorphic Fixpoint RTele_Type@{rt_T_base1 rt_T_base2 rt_T_tele rt_T_max rt_T_type1 rt_T_type2 rt_T_max_all rt_T_weird1 rt_T_weird2 rt_T_max_weird} {isort} {it : ITele isort} {rsort} (rt : RTele@{rt_T_base1 rt_T_base2 rt_T_tele rt_T_max rt_T_type1 rt_T_type2 rt_T_max_all} rsort it) : Type@{rt_T_max_weird} :=
match rt with
| @rBase _ _ s r =>
  (forall (t : selem_of s), stype_of@{rt_T_weird1 rt_T_weird2} rsort)
| rTele rt => forall t, RTele_Type (rt t)
end.

(* No idea why we still need rt_F_max_weird. *)
Polymorphic Fixpoint RTele_Fun@{rt_F_base1 rt_F_base2 rt_F_tele rt_F_max rt_F_type1 rt_F_type2 rt_F_max_all rt_F_max_weird} {isort} {it : ITele isort} {rsort} (rt : RTele rsort it) : RTele_Type@{rt_F_base1 rt_F_base2 rt_F_tele rt_F_max rt_F_type1 rt_F_type2 rt_F_max_all rt_F_type1 rt_F_type2 rt_F_max_weird} rt :=
  match rt with
  | rBase r => r
  | rTele rt => fun t => (RTele_Fun (rt t))
  end.

Section ExampleReflect.

  Inductive reflect (P :Prop) : bool -> Type :=
  | RTrue : P -> reflect P true
  | RFalse : ~P -> reflect P false.

  Example reflect_reflect P : ITele (SType) := iTele (fun b=>@iBase SType (reflect P b)).

  Example reflect_RTrue P : CTele (reflect_reflect P) :=
    (cProd (fun p=>@cBase SType _ (aTele _ (aBase)) (RTrue P p))).

  Example reflect_RFalse P : CTele (reflect_reflect P) :=
    (cProd (fun p=>@cBase SType _ (aTele _ (aBase)) (RFalse P p))).

  Example reflect_args P b : ATele (reflect_reflect P) :=
    aTele b aBase.

End ExampleReflect.


Example reflect_app P b := Eval compute in ITele_App (reflect_args P b).

(* We need to handle Prop (maybe) *)
Polymorphic Fixpoint abstract_goal@{ag_base1 ag_base2 ag_tele ag_max ag_g1 ag_g2 ag_max_all} {isort} {rsort} {it : ITele isort} (args : ATele@{ag_base1 ag_base2 ag_tele ag_max} it) (G : stype_of@{ag_g1 ag_g2} rsort) :
  selem_of (ITele_App args) -> M (RTele@{ag_base1 ag_base2 ag_tele ag_max ag_g1 ag_g2 ag_max} rsort it) :=
  match args with
  | @aBase _ T => fun t =>
    b <- is_var t;
    if b then
      r <- abs t G;
      ret (rBase r)
    else
      failwith "Argument t should be a variable"
  | @aTele _ _ f v args => fun t=>
      r <- abstract_goal args G t;
      b <- is_var v;
      if b then
        r <- abs (P:=fun v'=>RTele rsort (f v')) v r;
        ret (rTele r)
      else
        failwith "All indices need to be variables"
  end.

Polymorphic Fixpoint get_type_of_branch@{gtob_base1 gtob_base2 gtob_tele gtob_max gtob_prod gtob_res1 gtob_res2 gtob_max_all} {isort} {rsort} {it : ITele isort} (rt : RTele@{gtob_base1 gtob_base2 gtob_tele gtob_max gtob_res1 gtob_res2 gtob_max_all} rsort it) (ct : CTele@{gtob_base1 gtob_base2 gtob_tele gtob_max gtob_prod gtob_max_all} it) : stype_of@{gtob_res1 gtob_res2} rsort :=
  match ct with
  | cBase a t => RTele_App rt a t
  | cProd f => ForAll@{gtob_res1 gtob_res1 gtob_res2 gtob_res1 gtob_res2} (fun t => get_type_of_branch rt (f t))
  end.


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
  case <- makecase r;
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
              case_type := RTele_App rG (reflect_args P b) r;
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
      let t' := match H in _ = P return P with eq_refl => t end in (* we need to reduce this *)
      nu x,
        r <- unfold_funs (t' x) n';
      abs x r
    | [? A'] A' => [H]
      match H in _ = P return M P with eq_refl => ret t end
    end
  end.

(* MetaCoq version *)
Goal forall P b, reflect P b -> P <-> b = true.
MProof.
  intros P b r.
  mpose (rG := abstract_goal (rsort := SType) (reflect_args P b) (P <-> b = true) r).
  tsimpl.
  assert (T : get_type_of_branch rG (reflect_RTrue P)).
  - cintros x {- MCTactics.split;; [cintros P {- reflexivity -}; cintros notP {- assumption -}] -}. (* it doesn't work if intros is put outside *)
  assert (F : get_type_of_branch rG (reflect_RFalse P)).
  - tsimpl. intros. MCTactics.split. intros. exact (match a x with end). intros;; discriminate.
  mpose (typ0 := unfold_funs (RTele_Fun rG) 0).
  mpose (typ1 := unfold_funs (RTele_Fun rG) 1).
  mpose (typ2 := unfold_funs (RTele_Fun rG) 10).
  pose (mc :=
          makecase {|
              case_val := r;
              case_type := RTele_App rG (reflect_args P b) r;
              case_return := Dyn (typ2);
              case_branches := (Dyn T) :: (Dyn F) :: nil
            |}).
  mpose (c := mc).
  exact (elem c).
Qed.

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
              case_type := RTele_App rt a v;
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

Polymorphic Definition get_ITele : forall {T : Type} (ind : T), MetaCoq (nat * {s : Sort & ITele s}) :=
mfix2 f (T : _) (ind : _) : M (nat * {s : Sort & ITele s})%type :=
  print_term ind;;
  mmatch T with
  | [? (A : Type) (F : A -> Type)] forall a, F a =>
    [H]
        let indFun := match H in eq _ P return P with eq_refl => ind end
                     in nu a : A,
                               r <- f (F a) (indFun a);
                     let (n, sit) := r in
                     let (sort, it) := sit : {s : Sort & ITele s} in
                     f <- abs a it;
                       ret (S n, existT _ sort (iTele f))
  | Prop =>
   [H]
      let indProp := match H in eq _ P return P with eq_refl => ind end
                    in ret (0, existT _ SProp (iBase (sort := SProp) indProp))
  | Type =>
    [H]
       let indType := match H in eq _ P return P with eq_refl => ind end
                      in ret (0, existT _ (SType) (iBase (sort := SType) indType))
  | Set =>
    [H]
       let indType := match H in eq _ P return P with eq_refl => ind end
                      in ret (0, existT _ (SType) (iBase (sort := SType) indType))
                    | _ => failwith "Impossible ITele"
           end
                      .

Example get_reflect_ITele := Eval compute in ltac:(mrun (get_ITele (reflect True))).
Example reflect_nindx := Eval compute in let (n, _) := get_reflect_ITele in n.
Example reflect_sort := Eval compute in let (sort, _) := snd get_reflect_ITele in sort.
Example reflect_itele : ITele reflect_sort :=
  Eval compute in
  match snd get_reflect_ITele as pair return let (sort, _) := pair in ITele sort with
  | existT _ s it => it
  end.

Definition args_of : forall A, A -> M (list dyn) :=
  mfix2 rec (A : Type) (a : A) : M _ :=
    mmatch a with
    | [? T (t : T) f] f t => r <- rec _ f; ret (r ++ [Dyn t])
    | _ => ret nil
    end.

(* Get exactly `max` many arguments *)
Definition NotEnoughArguments : Exception. exact exception. Qed.
Fixpoint args_of_max (max : nat) : forall {A}, A -> M (list dyn) :=
    match max with
    | 0 => fun _ _ => ret nil
    | S max => fun A a =>
      mmatch a with
      | [? T (t : T) f] f t => r <- args_of_max max f; ret (r ++ [Dyn t])
      | _ => raise NotEnoughArguments
      end
  end.

Example args_of_RTrue P := Eval compute in ltac:(mrun (args_of _ (reflect P true))).
Example args_of_max_RTrue P := Eval compute in ltac:(mrun (args_of_max 2 (reflect P true))).

Polymorphic Fixpoint get_ATele {isort} : forall (it : ITele isort), list dyn -> M (ATele it) :=
  fix rec it al :=
    match it as it', al return M (ATele it') with
    | iBase T, nil => ret (@aBase _ T)
    | iTele f, t_dyn :: al =>
      t <- coerce (elem t_dyn);
        r <- rec (f t) al;
        ret (aTele t r)
    | _, _ => raise NoPatternMatches
    end.

Example ATele_of_RTrue := Eval compute in ltac:(mrun (get_ATele (reflect_itele) (tail (args_of_RTrue True)))).

Polymorphic Definition get_CTele_raw : forall {isort} (it : ITele isort) (nindx : nat) {A : stype_of isort}, selem_of A -> M (CTele it) :=
  fun isort it nindx =>
    mfix2 rec (A : stype_of isort) (a : selem_of A) : M (CTele it) :=
    print "get_CTele_raw: A";;
    print_term A;;
               B <- evar Type;
      F <- evar (B -> stype_of isort);
      oH <- munify A (ForAll F) UniNormal;
      match oH with
      | Some H =>
        print "Prod case";;
        let f := reduce RedWhd (match H in _ = P return selem_of P with eq_refl => a end) in
                nu b : B,
                       r <- rec (F b) (App f b);
                   f' <- abs b r;
                   print "After Abs";;
                   ret (cProd f')
      | None =>
        m1 <- munify B (stype_of isort) UniNormal;
          match m1 with
          | None => print_term B;; failwith "Should never happen"
          | Some H => let idB := reduce RedWhd (match H in _ = T' return B -> T' with
                                 | eq_refl => fun (x : _) => x
                                 end) in
                              munify F idB UniNormal;; ret tt
                          end;;
                               print_term B;; print_term F;;
        print "NoFun case";;
              let A_red := reduce RedWhd  A in
                         args <- args_of_max nindx A_red;
                           atele <- get_ATele it args;
                           a' <- @coerce _ (selem_of (ITele_App (isort := isort) atele)) a ;
                             ret (cBase atele a')
end.

Polymorphic Definition get_CTele :=
  fun {isort} =>
    match isort as sort return forall {it : ITele sort} nindx {A : stype_of sort}, selem_of A -> M (CTele it) with
    | SProp => get_CTele_raw (isort := SProp)
    | SType => get_CTele_raw (isort := SType)
    end.

Example get_RTrue_CTele := Eval compute in ltac:(mrun (get_CTele reflect_itele 1 _ (RTrue True))).
Example get_RFalse_CTele := Eval compute in ltac:(mrun (get_CTele reflect_itele 1 _ (RFalse True))).

(* Record sdyn sort : Type := *)
(*   { sdyn_type : stype_of sort; sdyn_elem : selem_of sdyn_type }. *)
(* Arguments sdyn_type [_] _. *)
(* Arguments sdyn_elem [_] _. *)


Polymorphic Definition sort_dyn@{i i1} (isort : Sort) (A : Type@{i}) (a : A) : M (sigT (@selem_of@{i i1} isort)) :=
    P <- @coerce (Type@{i}) (stype_of@{i i1} isort) A;
    p <- @coerce _ (selem_of@{i i1} P) a;
    ret ((existT _ _ p)).

Polymorphic Definition sort_goal {T : Type} (A : T) : M (sigT stype_of) :=
  mmatch T with
| Prop => [H] let A_Prop := match H in _ = R return R with eq_refl => A end in
                      ret (existT _ SProp A_Prop)
| Type => [H] let A_Type := match H in _ = R return R with eq_refl => A end in
                      ret (existT _ SType A_Type)
end.

Polymorphic Definition map' := map.
Arguments map' [_ _] _ _.

Definition dyn_of_stype {sort} : stype_of sort -> dyn :=
  match sort with
  | SProp => fun s => Dyn (selem_of s)
  | SType => fun s => Dyn (selem_of s)
  end.

(* Unset Strict Universe Declaration. *)
(* Polymorphic Definition uni_coerce : forall (A : Type@{i}) (B : Type@{j}), A -> M B := *)
(* fun A B (x : A) => *)
(* oH <- munify A B UniNormal; *)
(* match oH with *)
(* | Some H => retS coerce_rect B H x *)
(* | None => raise CantCoerce *)
(* end. *)


Set Printing Universes.

Polymorphic Definition get_ind {A : Type} (n : A) :
  M (nat * sigT (fun s => (ITele s)) * list dyn) :=
  r <- constrs A;
    print_term r;;
               let (indP, constrs) := r in
               sortit <- get_ITele (elem indP) : M (nat * sigT ITele);
                 print_term sortit;;
                            let nindx : nat := fst sortit in
                            let (isort, it) := snd sortit in
                            ret (nindx, existT _ _ it, constrs)
.

        (* Compute ind type ATele *)

Polymorphic Definition get_ind_atele {isort} (it : ITele isort) (nindx : nat) (A : Type) :=
    indlist <- args_of_max nindx A : M (list dyn);
      atele <- get_ATele it indlist : M (ATele it);
      ret atele.

Definition makecase_wrapper {i r} {it : ITele i} (a : ATele it) (rt : RTele r it) (branches : list goal) v  : M (selem_of (RTele_App rt a v)) :=
  branches <- mmap goal_to_dyn branches;
  mc <- makecase
     {|
       case_val := v;
       case_type := selem_of (RTele_App rt a v);
       case_return := Dyn (RTele_Fun rt);
       case_branches := branches
     |};
    wt <- (coerce (elem mc));
    ret wt
.

Polymorphic Definition new_destruct@{nd_A} {A : Type@{nd_A}} (n : A) : tactic :=
  fun g=>
    ind <- get_ind n;
      let (nsortit, constrs) := ind in
      let (nindx, sortit) := nsortit in
      let (isort, it) := sortit in
      atele <- get_ind_atele it nindx A;
                 (* Compute CTeles *)
        cts <- mmap (fun c_dyn : dyn =>
                       c <- sort_dyn isort (type c_dyn) (elem c_dyn);
                         let (ty, el) := c in
                         get_CTele it nindx ty el
                    ) constrs;
                     (* Compute return type RTele *)
        gt <- goal_type g;
        rsG <- sort_goal gt;
        let (rsort, sG) := rsG in
        print_term (isort, rsort);;
        n' <- coerce n;
          rt <- abstract_goal atele sG n';
          let sg := reduce RedSimpl (map (
                        fun ct =>
                           (selem_of (get_type_of_branch rt ct))
                                  ) cts) in
          goals <- mmap (fun ty=>r <- evar ty; ret (TheGoal r)) sg;
          branches <- mmap goal_to_dyn goals;
          let tsg := reduce RedWhd (type_of sg) in
          print_term tsg;;
          print_term sg;;
          let rrf := reduce RedSimpl (RTele_Fun rt) in
          let rrt := reduce RedSimpl (RTele_Type rt) in
          print_term rrt;;
          print_term rrf;;
          print "after coerce";;
            caseterm <- makecase {|
                       case_val := n';
                       case_type := selem_of (RTele_App rt atele n');
                       case_return := Dyn rrf;
                       case_branches := branches
                     |};
          ret goals
          (* let h'' := map Dyn sg in *)
          (* ret (map dyn_to_goal h'') *)
.

(* Definition test (P : Prop) (b : bool) := *)
(*   c <- coerce (reflect P b); *)
(*     let x := iff c c in *)
(*     ret tt. *)
(* Example blafu := ltac:(mrun (test True true)). *)

Example fubar (T : Type) (A : T) := mmatch T with | Prop => ret true | _ => ret false end.
Example fubar2 (T : Type) (A : T) := mtry r <- @coerce _ Prop A; ret true with _ => ret false end.
Definition fubarProp := Eval compute in ltac:(mrun (fubar Prop (True <-> True))).
Definition fubarType := Eval compute in ltac:(mrun (fubar Type (True <-> True))).
Definition fubar2Prop := Eval compute in ltac:(mrun (fubar2 Prop (True <-> True))).
Definition fubar2Type := Eval compute in ltac:(mrun (fubar2 Type (True <-> True))).
Compute (fubarProp, fubarType, fubar2Prop, fubar2Type).

Example test P b (r : reflect P b) : P -> if b then nat else True -> nat.
MProof.
  new_destruct r.
  ltac:(
    mrun ((fun g =>
             t <- new_destruct r g;
               print "debuggery";;
               print_term t;;
                          ret [g]) : tactic)
).


  b <- is_var n;
  ctx <- if b then hyps_except n else hypotheses;
  P <- Cevar (A->Type) ctx;
  let Pn := P n in
  gT <- goal_type g;
  unify_or_fail Pn gT;;
  l <- get_inductive A;
  l <- MCListUtils.mmap (fun d : dyn =>
    (* a constructor c has type (forall x, ... y, A) and we return
       (forall x, ... y, P (c x .. y)) *)
    t' <- copy_ctx P d;
    e <- evar t';
    ret {| elem := e |}) l;
  let c := {| case_ind := A;
              case_val := n;
              case_type := Pn;
              case_return := {| elem := P |};
              case_branches := l
           |} in
  d <- makecase c;
  d <- coerce (elem d);
  let d := hnf d in
  unify_or_fail (@TheGoal Pn d) g;;
  let l := hnf (List.map dyn_to_goal l) in
  ret l.
