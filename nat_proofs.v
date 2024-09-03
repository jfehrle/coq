(* Ltac myauto id :=
  tryif
  timeout 100
    progress auto 9 with nocore plus
  then idtac "success: " id
  else idtac "failure".
Goal True.
idtac "my_theorem_name"; myauto 4.
idtac "my_theorem_name"; myauto 3.
 *)
Create HintDb plus discriminated.
Hint Extern 6 => foreach IND: induction IND : plus.
Hint Extern 1 => progress simpl : plus.
Hint Extern 0 => reflexivity : plus.
Hint Extern 1 => foreach HYP: rewrite <- HYP : plus.

(* plus_n_0, plus_n_Sm are in core *)
Theorem my_plus_n_O : forall n : nat, n = n + 0.
(* intro.
induction n.  (*external*)  (* in plus *)
- reflexivity.  (*external*)  (* in plus *)
- progress simpl.  (*external*)  (* in plus *)
  rewrite <- IHn.  (*external*)  (* in plus *)
  reflexivity.  (*external*)  (* in plus *) *)
Time info_auto 9 with nocore plus.
Qed.
Hint Resolve my_plus_n_O : plus.
(* Hint Resolve <- plus_n_O : plus. *)
Hint Extern 2 => rewrite <- my_plus_n_O : plus.

Theorem my_plus_n_Sm: forall m n: nat, S (n + m) = n + S m.
Time info_auto 9 with nocore plus.
Qed.
(* Hint Resolve <- plus_n_m : plus. *)
Hint Extern 3 => rewrite <- my_plus_n_Sm : plus.

Theorem my_plus_comm : forall m n: nat,
  n + m = m + n.
Time info_auto 9 with nocore plus.
Qed.
Hint Extern 3 => rewrite <- my_plus_comm : plus.

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
Time info_auto 9 with nocore plus.
Qed.
Hint Extern 3 => rewrite <- my_plus_assoc : plus.

(* better name: my_mult_n_O *)
Theorem my_add_cancel_r: forall m : nat, m * 0 = 0.
Time info_auto 9 with nocore plus.
(* intros.
induction m.
reflexivity.
apply IHm.
 *)
Qed.
Hint Resolve my_add_cancel_r : plus.
Hint Extern 2 => rewrite <- my_add_cancel_r : plus.
(* Hint Extern 1 => foreach HYP: rewrite -> HYP : plus. *)

Hint Extern 0 => reflexivity : plus.
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
 Time info_auto 9 with nocore plus.  (* 2.7 sec *)
Print HintDb plus.

Qed.
Hint Extern 3 => rewrite <- my_mult_n_Sm : plus.

(*
(* original in Peano.v too complex *)
Lemma mult_n_Sm : forall n m:nat, n * m + n = n * S m.
Proof.
  intros n m; induction n as [| p H]; simpl; auto.
  destruct H; rewrite <- plus_n_Sm; apply eq_S.
  pattern m at 1 3; elim m; simpl; auto.
Qed.
 *)

Require Import ZArith.
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
Time info_auto 9 with nocore plus.
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
Time info_auto 9 with nocore plus.
(* intros.
rewrite <- my_plus_comm.
reflexivity. *)
Qed.
Hint Extern 3 => rewrite <- my_mult_n_Sm : plus.

Print HintDb plus.

(* key point: list induction variable last in forall *)
Theorem my_dist_plus_mul: forall m p n, (n + m) * p = n * p + m * p.
Time info_auto 9 with nocore plus.
(* intros.
induction n.
- reflexivity.
- progress simpl.
  rewrite <- my_plus_assoc.
  rewrite <- IHn.
  reflexivity.
  *)
Qed.
Hint Extern 4 => rewrite -> my_dist_plus_mul : plus.

Theorem my_mul_assoc: forall m p n : nat, n * (m * p) = n * m * p.
Time info_auto 9 with nocore plus.
(* intros.
induction n.
- reflexivity.
- progress simpl.
  rewrite my_dist_plus_mul.
  rewrite <- IHn.
  reflexivity. *)
Qed.

Theorem rev3: forall a b c : nat, a+b+c = c+b+a.
Time info_auto 9 with nocore plus.
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

Hint Extern 3 => rewrite rev3 : plus.
Hint Extern 3 => rewrite <- rev3 : plus.
Hint Extern 4 => rewrite <- plus_O_n : plus.
Hint Resolve rev3 : plus.

Theorem rev4: forall a b c d : nat, a+b+c+d = d+c+b+a.
Time info_auto 9 with nocore plus.
(*
intros.
rewrite <- my_plus_comm.
rewrite rev3.
rewrite <- my_plus_assoc with (r:=a).
rewrite -> my_plus_assoc.
rewrite -> my_plus_assoc with (q:=b).
reflexivity. *)
Qed.

Hint Extern 3 => rewrite rev4 : plus.
Hint Extern 3 => rewrite rev3 : plus.



Theorem rev5: forall a b c d e : nat, a+b+c+d+e = e+d+c+b+a.
(* intros.
rewrite <- my_plus_comm.
rewrite rev4.
rewrite <- my_plus_assoc with (r:=a).
rewrite -> my_plus_assoc.
rewrite -> my_plus_assoc with (q:=b).
rewrite -> my_plus_assoc.
reflexivity.
 *)
Time info_auto 9 with nocore plus.
Qed.

(* mult_n_O: forall n : nat, 0 = n * 0 *)

Require Import ZArith.
(* Search "_ * _". *)
