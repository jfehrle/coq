(* todo: use Ltac2, pass in a parameter *)
Ltac myauto :=
  tryif
  timeout 20
     progress info_auto 5 with nocore AUTO
  then idtac "success: "
  else idtac "failure".
(* Print HintDb AUTO. *)

Hint Extern 250 => foreach IND: induction IND : AUTO.
Hint Extern 1 => progress simpl : AUTO.
Hint Extern 0 => reflexivity : AUTO.
Hint Extern 1 => foreach HYP: rewrite <- HYP : AUTO.

(* Print HintDb AUTO. *)

(* plus_n_0, plus_n_Sm are in core *)
Theorem my_plus_n_O : forall n : nat, n = n + 0.
(* intro.
induction n.  (*external*)  (* in plus *)
- reflexivity.  (*external*)  (* in plus *)
- progress simpl.  (*external*)  (* in plus *)
  rewrite <- IHn.  (*external*)  (* in plus *)
  reflexivity.  (*external*)  (* in plus *) *)
Time myauto.
Qed.
(* Hint Resolve my_plus_n_O : plus. *)
(* Hint Resolve <- plus_n_O : plus. *)
(* Hint Extern 2 => rewrite <- my_plus_n_O : plus. *)

Theorem my_plus_n_Sm: forall m n: nat, S (n + m) = n + S m.
(* intro.
intro.
induction n.  (*external*)  (* in plus *)
- reflexivity.  (*external*)  (* in plus *)
- progress simpl.  (*external*)  (* in plus *)
  rewrite <- IHn.  (*external*)  (* in plus *)
 *)(*   reflexivity.  (*external*)  (* in plus *) *)

Time myauto.
Qed.
(* Hint Resolve <- plus_n_m : plus. *)
(* Hint Extern 3 => rewrite <- my_plus_n_Sm : plus. *)

Theorem my_plus_comm : forall m n: nat,
  n + m = m + n.
(* intro.
intro.
simple apply eq_add_S.  (* in AUTO *)
simple apply eq_add_S.  (* in AUTO *)
simple apply eq_add_S.  (* in AUTO *)
simple apply eq_add_S.  (* in AUTO *)
induction n.  (*external*)  (* in AUTO *)
- simple apply my_plus_n_O.  (* in AUTO *)
- progress simpl.  (*external*)  (* in AUTO *)
  simple apply f_equal_nat.  (* in AUTO *)
  rewrite <- my_plus_n_Sm.  (*external*)  (* in AUTO *)
  assumption.
 *)Time myauto.
Qed.
(* Hint Extern 3 => rewrite <- my_plus_comm : plus. *)

Theorem my_plus_assoc: forall q r p: nat, p + (q + r) = p + q + r.
(* intro.
intro.
intro.
rewrite <- my_plus_comm.  (*external*)  (* in plus *)
induction p.  (*external*)  (* in plus *)
- rewrite <- my_plus_n_O.  (*external*)  (* in plus *)
  reflexivity.  (*external*)  (* in plus *)
- idtac "X".
  rewrite <- my_plus_comm.  (*external*)  (* in plus *)
  idtac "x".
  rewrite <- my_plus_comm.  (*external*)  (* in plus *)
  idtac "x".
  progress simpl.  (*external*)  (* in plus *)
  rewrite <- IHp.  (*external*)  (* in plus *)
  induction p.  (*external*)  (* in plus *)
  + rewrite <- my_plus_comm.  (*external*)  (* in plus *)
    simple apply my_plus_n_O.  (* in plus *)
  + rewrite <- my_plus_n_Sm.  (*external*)  (* in plus *)
    reflexivity.  (*external*)  (* in plus *) *)

(* intros.
induction p.  (*external*)  (* in plus *)
- reflexivity.  (*external*)  (* in plus *)
- progress simpl.  (*external*)  (* in plus *)
  rewrite <- IHp.  (*external*)  (* in plus *)
  reflexivity. *)  (*external*)  (* in plus *)
Time myauto.
Qed.
(* Hint Extern 3 => rewrite <- my_plus_assoc : plus. *)

(* better name: my_mult_n_O *)
Theorem my_add_cancel_r: forall m : nat, m * 0 = 0.
Time myauto.
(* intros.
induction m.
reflexivity.
apply IHm.
 *)
Qed.
(* Hint Resolve my_add_cancel_r : plus. *)
(* Hint Extern 2 => rewrite <- my_add_cancel_r : plus. *)

Hint Extern 1 => foreach HYP: rewrite -> HYP : AUTO.

Hint Extern 0 => reflexivity : AUTO.
Theorem my_mult_n_Sm: forall m n: nat, ((n * m) + n) = (n * (S m)).
(* intros.
induction n.
- reflexivity.
- simpl.
  rewrite <- my_plus_n_Sm.
  rewrite <- my_plus_assoc.
  rewrite <- IHn.
  reflexivity.
*)
(* intros.
induction n.
-
 *)
 Time myauto.
(* Print HintDb AUTO. *)

Qed.
(* Hint Extern 3 => rewrite <- my_mult_n_Sm : plus. *)

(*
(* original in Peano.v too complex *)
Lemma mult_n_Sm : forall n m:nat, n * m + n = n * S m.
Proof.
  intros n m; induction n as [| p H]; simpl; auto.
  destruct H; rewrite <- plus_n_Sm; apply eq_S.
  pattern m at 1 3; elim m; simpl; auto.
Qed.
 *)

(* Require Import ZArith. *)
(* Search "_ * _". *)



Theorem my_mul_comm: forall m n: nat, n * m = m * n.
(* intros.
induction n.  (*external*)  (* in plus *)
- rewrite my_add_cancel_r.
  reflexivity.
- rewrite <- my_mult_n_Sm.  (*external*)  (* in plus *)
  rewrite <- IHn.  (*external*)  (* in plus *)
  simpl.
  rewrite <- my_plus_comm.  (*external*)  (* in plus *)
  reflexivity.  (*external*)  (* in plus *)
 *)
Time myauto.
(* intros.
induction n.
- rewrite my_add_cancel_r.  (* simple apply my_add_cancel_r doesn't work *)
  reflexivity.
- simpl.
  rewrite IHn.
  rewrite <- my_mult_n_Sm.
  rewrite <- my_plus_comm.
  reflexivity.
  *)
Qed.

Theorem my_mult_Sn_m: forall m n : nat, ((n * m) + m) = ((S n) * m).
Time myauto.
(* intros.
rewrite <- my_plus_comm.
reflexivity. *)
Qed.
(* Hint Extern 3 => rewrite <- my_mult_n_Sm : plus. *)

(* Print HintDb AUTO. *)

(* key point: list induction variable last in forall *)
Theorem my_dist_plus_mul: forall m p n, (n + m) * p = n * p + m * p.
Time myauto.
(* intros.
induction n.
- reflexivity.
- progress simpl.
  rewrite <- my_plus_assoc.
  rewrite <- IHn.
  reflexivity.
  *)
Qed.
(* Hint Extern 4 => rewrite -> my_dist_plus_mul : plus. *)

Theorem my_mul_assoc: forall m p n : nat, n * (m * p) = n * m * p.
Time myauto.
(* intros.
induction n.
- reflexivity.
- progress simpl.
  rewrite my_dist_plus_mul.
  rewrite <- IHn.
  reflexivity. *)
Qed.

Theorem rev3: forall a b c : nat, a+b+c = c+b+a.
Time myauto.
(* intros.
rewrite <- my_plus_comm.
rewrite <- my_plus_comm with (m:=a).
rewrite <- my_plus_assoc.
reflexivity.
 *)
Qed.
(*
rewrite <- my_plus_assoc.  (*external*)  (* in plus *)
rewrite <- my_plus_assoc.  (*external*)  (* in plus *)
rewrite <- my_plus_comm.  (*external*)  (* in plus *)
rewrite <- my_plus_assoc.  (*external*)  (* in plus *)
rewrite <- my_plus_comm.  (*external*)  (* in plus *)
induction c.  (*external*)  (* in plus *)
- rewrite <- my_plus_comm.  (*external*)  (* in plus *)
  reflexivity.  (*external*)  (* in plus *)
- progress simpl.  (*external*)  (* in plus *)
  rewrite <- IHc.  (*external*)  (* in plus *)
  reflexivity.  (*external*)  (* in plus *)
*)

(* my_plus_assoc: forall q r p: nat, p + (q + r) = p + q + r.
my_plus_comm : forall m n: nat,
 *)

Theorem rev4: forall a b c d : nat, a+b+c+d = d+c+b+a.
Time myauto.
(*
intros.
rewrite <- my_plus_comm.
rewrite rev3.
rewrite <- my_plus_assoc with (r:=a).
rewrite -> my_plus_assoc.
rewrite -> my_plus_assoc with (q:=b).
reflexivity. *)
Qed.



Theorem rev5: forall a b c d e : nat, a+b+c+d+e = e+d+c+b+a.
Time myauto.
Qed.

Theorem rev6: forall a b c d e f: nat, a+b+c+d+e+f = f+e+d+c+b+a.
Time myauto.
Qed.

Theorem rev7: forall a b c d e f g: nat, a+b+c+d+e+f+g = g+f+e+d+c+b+a.
Time myauto.
Qed.

(* mult_n_O: forall n : nat, 0 = n * 0 *)

(* Require Import ZArith. *)
(* Search "_ * _". *)
