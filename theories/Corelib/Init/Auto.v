Require Import Logic Ltac.

Create HintDb AUTO discriminated.
Hint  Constants	Opaque : AUTO.
Hint  Projections	Opaque : AUTO.
Hint  Variables	Opaque : AUTO.


Hint Extern 0 (?a = ?b) => reflexivity : AUTO.
Hint Extern 1 => progress simpl : AUTO.

Hint Extern 250 => foreach IND: induction IND : AUTO.
Hint Extern 1 => foreach HYP: rewrite <- HYP : AUTO.
Hint Extern 1 => foreach HYP: rewrite -> HYP : AUTO.
Hint Extern 150 (?a = ?b) => discriminate : AUTO.
Hint Extern 150 (?a <> ?b) => discriminate : AUTO.
