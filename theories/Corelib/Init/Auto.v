Require Import Logic Ltac.

Create HintDb AUTO discriminated.
Hint  Constants	Opaque : AUTO.
Hint  Projections	Opaque : AUTO.
Hint  Variables	Opaque : AUTO.


Hint Extern 0 (?a = ?b) => reflexivity : AUTO.
Hint Extern 1 => progress simpl : AUTO.  (* todo: cost is too low *)
Hint Extern 100 => progress red : AUTO.

Hint Extern 1 => foreach HYP: rewrite <- HYP : AUTO.
Hint Extern 1 => foreach HYP: rewrite -> HYP : AUTO.
Hint Extern 150 => foreach IND: induction IND : AUTO.
Hint Extern 150 => foreach IND: destruct IND : AUTO.
Hint Extern 150 => foreach HYP: destruct HYP : AUTO.
Hint Extern 150 => constructor : AUTO.  (* more-efficient alternative? *)
Hint Extern 150 (?a = ?b) => discriminate : AUTO.
Hint Extern 150 (?a <> ?b) => discriminate : AUTO.
