.. _record-types:

Record types
----------------

The :cmd:`Record` command defines types similar to :gdef:`records`
in programming languages. Those types describe tuples whose
components, called :gdef:`fields <field>`, can be accessed with
:gdef:`projections <projection>`. Records can also be used to describe
mathematical structures, such as groups or rings, hence the
synonym :cmd:`Structure`.

.. _record_grammar:

.. cmd:: {| Record | Structure } @record_definition
   :name: Record; Structure

   .. insertprodn record_definition field_spec

   .. prodn::
      record_definition ::= {? > } @ident_decl {* @binder } {? : @sort } {? := {? @ident } %{ {*; @record_field } {? ; } %} {? as @ident } }
      record_field ::= {* #[ {*, @attribute } ] } @name {? @field_spec } {? %| @natural }
      field_spec ::= {* @binder } @of_type
      | {* @binder } := @term
      | {* @binder } @of_type := @term

   Defines a non-recursive record type, creating projections for each field
   that has a name other than `_`. The field body and type can depend on previous
   fields, so the order of fields in the definition may matter.

   Use the :cmd:`Inductive` and :cmd:`CoInductive` commands to define recursive
   (inductive or coinductive) records.  These commands also permit defining
   mutually recusrive records provided that all of
   the types in the block are records.  These commands automatically generate
   induction schemes.  Enable the :flag:`Nonrecursive Elimination Schemes` flag
   to enable automatic generation of elimination schemes for :cmd:`Record`.
   See :ref:`proofschemes-induction-principles`.

   :n:`{? > }`
     If specified, the constructor is declared as
     a coercion from the class of the last field type to the record name
     (this may fail if the uniform inheritance condition is not
     satisfied). See :ref:`coercions`.

   :n:`@ident_decl`
     The :n:`@ident` within is the record name.

   :n:`{* @binder }`
     :n:`@binder`\s may be used to declare the *parameters* of the record.

   :n:`: @sort`
     The sort the record belongs to.  The default is :n:`Type`.

   :n:`:= {? @ident }`
     :n:`@ident` is the name of the record constructor.  If omitted,
     the name defaults to :n:`Build_@ident` where :n:`@ident` is the record name.

   :n:`as {? @ident}`
     Specifies the name used to refer to the argument corresponding to the
     record in the type of projections.

     .. todo PR: clarify above description, maybe give an example

   In :n:`@record_field`:

     :n:`@attribute`, if specified, can only be :n:`canonical`.

     :n:`@name` is the field name.  Since field names define projections, you can't
     reuse the same field name in two different records in the same scope.  This
     :ref:`example <reuse_field_name>` shows how to reuse the same field
     name in multiple records.

     :n:`@field_spec` can be omitted only when the type of the field can be inferred
     from other fields. For example: the type of :n:`n` can be inferred from
     :n:`npos` in :n:`Record positive := { n; npos : 0 < n }`.

     :n:`| @natural`
       Specifies the priority of the field.  It is only allowed in :cmd:`Class` commands.

   In :n:`@field_spec`:

     - :n:`{+ @binder } : @of_type` is equivalent to
       :n:`: forall {+ @binder } , @of_type`

     - :n:`{+ @binder } := @term` is equivalent to
       :n:`:= fun {* @binder } => @term`

     - :n:`{+ @binder } @of_type := @term` is equivalent to
       :n:`: forall {* @binder } , @type := fun {* @binder } => @term`

     :n:`:= @term`, if present, gives the value of the field, which may depend
     on the fields that appear before it.  Since their values are already defined,
     such fields cannot be specified when constructing a record.

   The :cmd:`Record` command supports the :attr:`universes(polymorphic)`,
   :attr:`universes(template)`, :attr:`universes(cumulative)`,
   :attr:`private(matching)` and :attr:`projections(primitive)` attributes.

   .. example:: Defining a record

      The set of rational numbers may be defined as:

      .. coqtop:: reset all

         Record Rat : Set := mkRat
          { negative : bool
          ; top : nat
          ; bottom : nat
          ; Rat_bottom_nonzero : 0 <> bottom
          ; Rat_irreducible :
              forall x y z:nat, (x * y) = top /\ (x * z) = bottom -> x = 1
          }.

      The :n:`Rat_*` fields depend on :n:`top` and :n:`bottom`.
      :n:`Rat_bottom_nonzero` is a proof that :n:`bottom` (the denominator)
      is not zero.  :n:`Rat_irreducible` is a proof that the fraction is in
      lowest terms.

.. _reuse_field_name:

   .. example:: Reusing a field name in multiple records

      .. coqtop:: in

         Module A. Record R := { f : nat }. End A.
         Module B. Record S := { f : nat }. End B.

      .. coqtop:: all

         Check {| A.f := 0 |}.
         Check {| B.f := 0 |}.

   .. todo below: huh?  Hugo sez "the model to think about primitive projections
      is not fully stabilized"

   .. note:: Records exist in two flavors. In the first,
      a record :n:`@ident` with parameters :n:`{* @binder }`,
      constructor :n:`@ident__0`, and fields :n:`{* @name @field_spec }`
      is represented as a variant type with a single
      constructor: :n:`Variant @ident {* @binder } : @sort := @ident__0
      {* ( @name @field_spec ) }` and projections are defined by case analysis.
      In the second implementation, records have
      primitive projections: see :ref:`primitive_projections`.

Constructing records
~~~~~~~~~~~~~~~~~~~~

   .. insertprodn term_record field_val

   .. prodn::
      term_record ::= %{%| {*; @field_val } {? ; } %|%}
      field_val ::= @qualid {* @binder } := @term

   Instances of record types can be constructed using either *record form*
   (:n:`@term_record`, shown here) or *application form* (see :n:`@term_application`)
   using the constructor.  The associated record definition is selected using the
   provided field names or constructor name, both of which are global.

   In the record form, the fields can be given in any order.  Fields that can be
   inferred by unification or by using obligations (see :ref:`programs`) may be omitted.

   In application form, all fields of the record must be passed, in order,
   as arguments to the constructor.

   .. example:: Constructing 1/2 as a record

      Constructing the rational :math:`1/2` using either the record or application syntax:

      .. coqtop:: in

         Theorem one_two_irred : forall x y z:nat, x * y = 1 /\ x * z = 2 -> x = 1.
         Admitted.

         (* Record form: top and bottom can be inferred from other fields *)
         Definition half :=
           {| negative := false;
              Rat_bottom_nonzero := O_S 1;
              Rat_irreducible := one_two_irred |}.

         (* Application form: use the constructor and provide values for all the fields
            in order.  "mkRat" is defined by the Record command *)
         Definition half' := mkRat true 1 2 (O_S 1) one_two_irred.

Accessing fields (projections)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   .. insertprodn term_projection term_projection

   .. prodn::
      term_projection ::= @term0 .( @qualid {? @univ_annot } {* @arg } )
      | @term0 .( @ @qualid {? @univ_annot } {* @term1 } )

   The value of a field can be accessed using *projection form* (:n:`@term_projection`,
   shown here) or with *application form* (see :n:`@term_application`) using the
   projection function associated with the field.  Don't forget the parentheses for the
   projection form.
   Glossing over some syntactic details, the two forms are:

   - :n:`@qualid__record.( {? @ } @qualid__field {* @arg })` (projection) and

   - :n:`{? @ } @qualid__field {* @arg } @qualid__record` (application)

   where the :n:`@arg`\s are the parameters of the inductive type.  If :n:`@` is
   specified, all implicit arguments must be provided.

   Since the projected object is part of the notation, it is always
   considered an explicit argument of :token:`qualid`, even if it is
   formally declared as implicit (see :ref:`ImplicitArguments`).

   .. exn:: @ident cannot be defined because it is informative and @ident is not

      For example, :n:`Record foo:Prop := { x:Type }` generates the message
      "x cannot be defined ... and foo is not".  Proofs (objects of sort :n:`Prop`)
      are supposed to be non-distinguishable.  If you have two inhabitants of
      :n:`Type`, such as :n:`%{%| x := nat %|%}` and :n:`%{%| x := bool %|%}`, they are
      distinguishable (i.e. informative) and are therefore prohibited.

   .. example:: Accessing record fields

      .. coqtop:: all

         (* projection form *)
         Eval compute in half.(top).

      .. coqtop:: in

         Goal True.

      .. coqtop:: all

         let x := eval compute in half.(top) in idtac x.

         (* application form *)
         Eval compute in top half.
         Eval compute in bottom half.
         Eval compute in Rat_bottom_nonzero half.

   .. example:: Matching on records

      .. coqtop:: all

         Eval compute in (
           match half with
           | {| negative := false; top := n |} => n
           | _ => 0
           end).

   .. example:: Accessing anonymous record fields with match

      .. coqtop:: in

         Record T := const { _ : nat }.
         Definition gett x := match x with const n => n end.
         Definition inst := const 3.

      .. coqtop:: all

         Eval compute in gett inst.


The following settings let you control the display format for record types:

.. flag:: Printing Records

   When this :term:`flag` is on (this is the default),
   use the record syntax (shown above) as the default display format.

You can override the display format for specified record types by adding entries to these tables:

.. table:: Printing Record @qualid

   This :term:`table` specifies a set of qualids which are displayed as records.  Use the
   :cmd:`Add` and :cmd:`Remove` commands to update the set of qualids.

.. table:: Printing Constructor @qualid

   This :term:`table` specifies a set of qualids which are displayed as constructors.  Use the
   :cmd:`Add` and :cmd:`Remove` commands to update the set of qualids.

.. flag:: Printing Projections

   Activates the projection form (dot notation) for printing projections (off by default).

   .. example::

      .. coqtop:: all

         Check top half.  (* off: application form *)
         Set Printing Projections.
         Check top half.  (* on:  projection form *)

.. note:: Records exist in two flavors. In the first
   implementation, a record :n:`@ident` with parameters :n:`{* @binder }`,
   constructor :n:`@ident__0`, and fields :n:`{* @name @field_spec }`
   is represented as a variant type with a single
   constructor: :n:`Variant @ident {* @binder } : @sort := @ident__0
   {* ( @name @field_spec ) }` and projections are defined by case analysis.
   In the second implementation, records have
   primitive projections: see :ref:`primitive_projections`.

.. warn:: @ident cannot be defined.

  It can happen that the definition of a projection is impossible.
  This message is followed by an explanation of this impossibility.
  There may be three reasons:

  #. The name :token:`ident` already exists in the global environment (see :cmd:`Axiom`).
  #. The :term:`body` of :token:`ident` uses an incorrect elimination for
     :token:`ident` (see :cmd:`Fixpoint` and :ref:`Destructors`).
  #. The type of the projections :token:`ident` depends on previous
     projections which themselves could not be defined.

.. exn:: Records declared with the keyword Record or Structure cannot be recursive.

   The record name :token:`ident` appears in the type of its fields, but uses
   the :cmd:`Record` command. Use  the :cmd:`Inductive` or
   :cmd:`CoInductive` command instead.

.. exn:: Cannot handle mutually (co)inductive records.

   Records cannot be defined as part of mutually inductive (or
   coinductive) definitions, whether with records only or mixed with
   standard definitions.

During the definition of the one-constructor inductive definition, all
the errors of inductive definitions, as described in Section
:ref:`gallina-inductive-definitions`, may also occur.

.. seealso:: Coercions and records in section :ref:`coercions-classes-as-records`.

.. _primitive_projections:

Primitive Projections
~~~~~~~~~~~~~~~~~~~~~

When the :flag:`Primitive Projections` flag is on or the
:attr:`projections(primitive)` attribute is supplied for a :n:`Record` definition, its
:g:`match` construct is disabled. To eliminate the record type, one must
use its defined primitive projections.

For compatibility, the parameters still appear when printing terms
even though they are absent in the actual AST manipulated by the kernel. This
can be changed by unsetting the :flag:`Printing Primitive Projection Parameters`
flag.

There are currently two ways to introduce primitive records types:

#. Through the :cmd:`Record` command, in which case the type has to be
   non-recursive. The defined type enjoys eta-conversion definitionally,
   that is the generalized form of surjective pairing for records:
   `r` ``= Build_``\ `R` ``(``\ `r`\ ``.(``\ |p_1|\ ``) …`` `r`\ ``.(``\ |p_n|\ ``))``.
   Eta-conversion allows to define dependent elimination for these types as well.
#. Through the :cmd:`Inductive` and :cmd:`CoInductive` commands, when
   the :term:`body` of the definition is a record declaration of the form
   ``Build_``\ `R` ``{`` |p_1| ``:`` |t_1|\ ``; … ;`` |p_n| ``:`` |t_n| ``}``.
   In this case the types can be recursive and eta-conversion is disallowed.
   Dependent elimination is not available for such types;
   you must use non-dependent case analysis for these.

For both cases the :flag:`Primitive Projections` :term:`flag` must be set or
the :attr:`projections(primitive)` :term:`attribute`  must be supplied.

.. flag:: Primitive Projections

   This :term:`flag` turns on the use of primitive projections when defining
   subsequent records (even through the :cmd:`Inductive` and :cmd:`CoInductive`
   commands). Primitive projections extend the Calculus of Inductive
   Constructions with a new binary term constructor `r.(p)` representing a
   primitive projection `p` applied to a record object `r` (i.e., primitive
   projections are always applied). Even if the record type has parameters,
   these do not appear in the internal representation of applications of the
   projection, considerably reducing the sizes of terms when manipulating
   parameterized records and type checking time. On the user level, primitive
   projections can be used as a replacement for the usual defined ones, although
   there are a few notable differences.

.. attr:: projections(primitive{? = {| yes | no } })
   :name: projections(primitive)

   This :term:`boolean attribute` can be used to override the value of the
   :flag:`Primitive Projections` :term:`flag` for the record type being
   defined.

.. flag:: Printing Primitive Projection Parameters

   This compatibility :term:`flag` reconstructs internally omitted parameters at
   printing time (even though they are absent in the actual AST manipulated
   by the kernel).

Reduction
+++++++++

The basic reduction rule of a primitive projection is
|p_i| ``(Build_``\ `R` |t_1| … |t_n|\ ``)`` :math:`{\rightarrow_{\iota}}` |t_i|.
However, to take the δ flag into account, projections can be in two states:
folded or unfolded. An unfolded primitive projection application obeys the rule
above, while the folded version delta-reduces to the unfolded version. This
allows to precisely mimic the usual unfolding rules of :term:`constants <constant>`.
Projections obey the usual ``simpl`` flags of the :cmd:`Arguments`
command in particular.
There is currently no way to input unfolded primitive projections at the
user-level, and there is no way to display unfolded projections differently
from folded ones.


Compatibility Projections and :g:`match`
++++++++++++++++++++++++++++++++++++++++

To ease compatibility with ordinary record types, each primitive projection is
also defined as an ordinary :term:`constant` taking parameters and an object of
the record type as arguments, and whose :term:`body` is an application of the
unfolded primitive projection of the same name. These constants are used when
elaborating partial applications of the projection. One can distinguish them
from applications of the primitive projection if the :flag:`Printing Primitive
Projection Parameters` flag is off: For a primitive projection application,
parameters are printed as underscores while for the compatibility projections
they are printed as usual.

Additionally, user-written :g:`match` constructs on primitive records are
desugared into substitution of the projections, they cannot be printed back as
:g:`match` constructs.
