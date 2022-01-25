.. _record-types:

Record types
----------------

The :cmd:`Record` command defines types similar to :gdef:`records`
in programming languages. Those types describes tuples whose
components, called :gdef:`fields <field>`, can be accessed with
:gdef:`projections <projection>`. Records can also be used to describe
mathematical structures, such as groups or rings, hence the
synonym :cmd:`Structure`.

.. _record_grammar:

.. cmd:: {| Record | Structure } @record_definition
   :name: Record; Structure

   .. insertprodn record_definition field_type

   .. prodn::
      record_definition ::= {? > } @ident_decl {* @binder } {? : @sort } {? := {? @ident } %{ {*; @record_field } {? ; } %} {? as @ident } }
      record_field ::= {* #[ {*, @attribute } ] } @name {? @field_type } {? %| @natural }
      field_type ::= {* @binder } @of_type
      | {* @binder } @of_type := @term
      | {* @binder } := @term

   Defines a record type and creates projections for each field that has a name other
   than `_`. The field body and type can depend on previous
   fields, so the order of fields in the definition may matter.

   :n:`{? > }`
     If specified, the constructor is declared as
     a coercion from the class of the last field type to the record name
     (this may fail if the uniform inheritance condition is not
     satisfied). See :ref:`coercions`.

   :n:`@ident_decl`
     The name of the record.

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

     :n:`@name` is the field name.  Since field names define projections, you can't
     reuse the same field name in two different records in the same scope.

     :n:`field_type` can be omitted only when the type of the field can be inferred
     from other fields. For example: the type of :n:`n` can be inferred from
     :n:`npos` in :n:`Record positive := { n; npos : 0 < n }`.

     :n:`| @natural`
       Specifies the priority of the field.  It is only allowed in :cmd:`Class` commands.

   In :n:`@field_type`:

     :n:`{+ @binder } : @of_type` is equivalent to :n:`forall {+ @binder } , @of_type`
     and similarly for the other forms of :n:`@field_type`.

     .. todo PR: is the above correct for "binder := term"?

     :n:`:= @term`, if present, gives the value of the field, which may depend
     on the fields that appear before it.  Such fields are not included in the
     record constructor.

     .. todo PR: what attributes are allowed on fields?  "The command :cmd:`Record` supports"
        below applies to the command as a whole?

   The command :cmd:`Record` supports the :attr:`universes(polymorphic)`,
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

   .. todo PR: below: "not supposed to" or "must not"?

   .. note:: Records defined with the :cmd:`Record` command are not supposed to be
      recursive. To define recursive records, use the
      :cmd:`Inductive` and :cmd:`CoInductive` commands, resulting in an inductive or coinductive record.
      Definition of mutually inductive or coinductive records are also allowed, as long
      as all of the types in the block are records.

   .. note:: Induction schemes are automatically generated for inductive records.
      Automatic generation of elimination schemes for non-recursive records
      defined with the :cmd:`Record` command can be activated with the
      :flag:`Nonrecursive Elimination Schemes` flag (see :ref:`proofschemes-induction-principles`).

   .. todo PR: below: huh?

   .. note:: Records exist in two flavors. In the first,
      a record :n:`@ident` with parameters :n:`{* @binder }`,
      constructor :n:`@ident__0`, and fields :n:`{* @name @field_type }`
      is represented as a variant type with a single
      constructor: :n:`Variant @ident {* @binder } : @sort := @ident__0
      {* ( @name @field_type ) }` and projections are defined by case analysis.
      In the second implementation, records have
      primitive projections: see :ref:`primitive_projections`.

Constructing records
~~~~~~~~~~~~~~~~~~~~

   .. insertprodn term_record field_def

   .. prodn::
      term_record ::= %{%| {*; @field_def } {? ; } %|%}
      field_def ::= @qualid {* @binder } := @term

   Instances of record types can be constructed using either *record syntax*
   (:n:`@term_record`, shown here) or with *applicative syntax* (see :n:`@term_application`)
   using the constructor.

   .. todo PR: what does it mean if you provide a qualid in field_def that's not just an ident?
      Is that just to pick out a field defined in another section?

   In the record form, the fields can be given in any order.  Fields that can be
   inferred by unification or by using obligations (see :ref:`programs`) may be omitted.

   .. todo PR: what is the difference between "mkRat ..." and "@mkRat ..."?

   In applicative form, all fields of the record must be passed, in order,
   as arguments to the constructor.

   .. example:: Constructing 1/2 as a record

      Constructing the rational :math:`1/2` using either the record or applicative syntax:

      .. coqtop:: in

         Theorem one_two_irred : forall x y z:nat, x * y = 1 /\ x * z = 2 -> x = 1.
         Admitted.

         (* Record form: top and bottom can be inferred from other fields *)
         Definition half' :=
           {| negative := false;
              Rat_bottom_nonzero := O_S 1;
              Rat_irreducible := one_two_irred |}.

         (* Applicative form: use the constructor and provide values for all the fields
            in order *)
         Definition half := mkRat true 1 2 (O_S 1) one_two_irred.

Accessing fields (projections)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   .. insertprodn term_projection term_projection

   .. prodn::
      term_projection ::= @term0 .( @qualid {? @univ_annot } {* @arg } )
      | @term0 .( @ @qualid {? @univ_annot } {* @term1 } )

   The value of a field can be accessed using *projection syntax* (:n:`@term_projection`,
   shown here) or with *applicative syntax* (see :n:`@term_application`) using the
   projection function associated with the field.
   Glossing over some syntactic details, the two forms are:

   - :n:`@qualid__record.(@qualid__field {* @arg })` (projective) and

   - :n:`@qualid__field {* @arg } @qualid__record` (applicative)

   where the :n:`@arg`\s are the parameters of the inductive type.

   Since the projected object is part of the notation, it is always
   considered an explicit argument of :token:`qualid`, even if it is
   formally declared as implicit (see :ref:`ImplicitArguments`).

   .. todo PR
      "Record foo2:Prop := { x:Type }." gives the output "foo2 is defined.
      x cannot be defined because it is informative and foo2 is not."
      What does this mean?

   .. example::

      .. todo PR: maybe make this an example of matching?

      Let us define a function by pattern matching over a record:

      .. coqtop:: all

         Eval compute in (
           match half with
           | {| negative := false; top := n |} => n
           | _ => 0
           end).

   .. example:: Accessing record fields

      Let us project fields of a record, using either the applicative or projection syntax:

      .. coqtop:: all

         (* projection form *)
         Eval compute in half.(top).

         (* applicative form *)
         Eval compute in top half.
         Eval compute in bottom half.
         Eval compute in Rat_bottom_nonzero half.

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

   This :term:`flag` activates the dot notation for printing (off by default).

   .. example::

      .. coqtop:: all

         Set Printing Projections.
         Check top half.

.. note:: Records exist in two flavors. In the first
   implementation, a record :n:`@ident` with parameters :n:`{* @binder }`,
   constructor :n:`@ident__0`, and fields :n:`{* @name @field_type }`
   is represented as a variant type with a single
   constructor: :n:`Variant @ident {* @binder } : @sort := @ident__0
   {* ( @name @field_type ) }` and projections are defined by case analysis.
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

.. seealso:: Coercions and records in section :ref:`coercions-classes-as-records` of the chapter devoted to coercions.

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
