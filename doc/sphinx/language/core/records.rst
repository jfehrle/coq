.. _record-types:

Record types
----------------

The :cmd:`Record` command defines types similar to :gdef:`records`
in programming languages. Those types describes tuples whose
components, called :gdef:`fields <field>`, can be accessed by
:gdef:`projection`. Such types can also be used to describe
mathematical structures, such as groups or rings, hence the
synonym :cmd:`Structure`.

.. _record_grammar:

.. cmd:: {| Record | Structure } @record_definition
   :name: Record; Structure

   .. insertprodn record_definition field_def

   .. prodn::
      record_definition ::= {? > } @ident_decl {* @binder } {? : @sort } {? := {? @ident } %{ {*; @record_field } {? ; } %} {? as @ident } }
      record_field ::= {* #[ {*, @attribute } ] } @name {? @field_body } {? %| @natural } {? @decl_notations }
      field_body ::= {* @binder } @of_type
      | {* @binder } @of_type := @term
      | {* @binder } := @term

   Defines the record named
   :n:`@ident_decl`. For each field, :n:`@name` is the name of the
   field. Fields can be :gdef:`abstract <abstract field>` as in
   :n:`@name {* @binder } @of_type` or :gdef:`manifest <manifest
   field>`, that is explicitly specified, as in :n:`@name {* @binder } @of_type := @term`.
   Fields have a type given by :n:`@of_type` (when absent, as in :n:`@name {* @binder } := @term`,
   the type is inferred automatically).  In manifest
   fields, :n:`@term` is called the *body* of the field. The body
   and type of a field can depend on previous fields, so the order
   of fields in the definition may matter.

   To a record is associated a constructor. The name of this
   constructor is provided by the occurrence of :n:`@ident` after
   :n:`:=`. When omitted, the constructor name defaults to
   :n:`Build_@ident` where :n:`@ident` is the record name.

   A record type belongs to a sort which is given by :token:`sort`. If
   omitted, the default sort is Type.

   A sequence of :n:`@binder` may be applied to the record as a whole to declare
   the *parameters* of the record. They may also be applied to individual fields.
   For example, :n:`{+ @binder } : @of_type` is
   equivalent to :n:`forall {+ @binder } , @of_type` and
   similarly for the other forms of :n:`@field_body`.

   When possible, the :cmd:`Record` command associates to each
   field of the record a projection for accessing the instance
   of the field in an object of type :token:`ident`.  These
   projections are given the names of the corresponding fields. If a
   field is named `_` then no projection is built for it.

   If given, the :n:`as @ident` part in the syntax of records
   specifies the name to use for referring to the argument corresponding to the
   record in the type of projections.

   If :n:`{? > }` is provided, the constructor is automatically declared as
   a coercion from the class of the last field type to the record name
   (this may fail if the uniform inheritance condition is not
   satisfied). See :ref:`coercions`.

   Notations can be attached to fields using the :n:`@decl_notations` annotation.

   The command :cmd:`Record` supports the :attr:`universes(polymorphic)`,
   :attr:`universes(template)`, :attr:`universes(cumulative)`,
   :attr:`private(matching)` and :attr:`projections(primitive)` attributes.

Constructing records
~~~~~~~~~~~~~~~~~~~~~~

   .. prodn::
      term_record ::= %{%| {*; @field_def } {? ; } %|%}
      field_def ::= @qualid {* @binder } := @term

   Objects in record types can be written using either an *applicative
   syntax* of the form :n:`@term_application` or a *record syntax* of
   the form :n:`@term_record`.

   In the applicative form, it takes the
   form :n:`@qualid {? @univ_annot } {+ @arg }` or
   :n:`@@qualid {?  @univ_annot } {+ @term1 }` where :n:`@qualid`
   refers to the constructor. That is, the constructor is seen as a
   function and the instances for the abstract fields are given in
   order as arguments of the constructor.

   In the record form, the
   instance of an abstract field is given as body of the
   corresponding field name and the instances can be given in any
   order. If a field is omitted, its instance is implicitly considered
   to be the :n:`_` placeholder. It can then be filled automatically
   by unification or by using obligations (see :ref:`programs`).

.. FIXME: move this to the main grammar in the spec chapter

Record Projections
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   .. insertprodn term_projection term_projection

   .. prodn::
      term_projection ::= @term0 .( @qualid {? @univ_annot } {* @arg } )
      | @term0 .( @ @qualid {? @univ_annot } {* @term1 } )

   The projection from an object can be written using either an
   *applicative syntax* of the form :n:`@term_application` (more
   precisely :n:`@qualid {?  @univ_annot } {+ @arg }` or
   :n:`@@qualid {?  @univ_annot } {+ @term1 }`) or a *projection
   syntax* of the form :n:`@term_projection`. In both cases,
   :n:`@qualid` is the name of the projection.

   The syntax :n:`@term0.(@qualid)` is equivalent to :n:`@qualid @term0`,
   the syntax :n:`@term0.(@qualid {+ @arg })` to :n:`@qualid {+ @arg } @term0`
   and the syntax :n:`@term0.(@@qualid {+ @term0 })`
   to :n:`@@qualid {+ @term0 } @term0`.  In each case,
   :token:`term0` is the projected object and the other arguments are
   the parameters of the inductive type.

   Since the projected object is part of the notation, it is always
   considered an explicit argument of :token:`qualid`, even if it is
   formally declared as implicit (see :ref:`ImplicitArguments`),

   .. todo
      "Record foo2:Prop := { a }." gives the error "Cannot infer this placeholder of type "Type",
      while "Record foo2:Prop := { a:Type }." gives the output "foo2 is defined.
      a cannot be defined because it is informative and foo2 is not."
      Your thoughts?

.. example::

   The set of rational numbers may be defined as:

   .. coqtop:: reset all

      Record Rat : Set := mkRat
       { sign : bool (* false if negative *)
       ; top : nat
       ; bottom : nat
       ; Rat_bottom_cond : 0 <> bottom
       ; Rat_irred_cond :
           forall x y z:nat, (x * y) = top /\ (x * z) = bottom -> x = 1
       }.

   Note here that the fields ``Rat_bottom_cond`` depends on the field ``bottom``
   and ``Rat_irred_cond`` depends on both ``top`` and ``bottom``.

.. example::

   Let us define the rational :math:`1/2` using either the applicative or record syntax:

    .. coqtop:: in

       Theorem one_two_irred : forall x y z:nat, x * y = 1 /\ x * z = 2 -> x = 1.
       Admitted.

       (* Applicative form: values for fields should be given in order *)
       Definition half := mkRat true 1 2 (O_S 1) one_two_irred.

       (* Record form: omitting inferrable fields top and bottom *)
       Definition half' :=
         {| sign := true;
            Rat_bottom_cond := O_S 1;
            Rat_irred_cond := one_two_irred |}.

.. example::

   Let us define a function by pattern matching over a record:

   .. coqtop:: all

      Eval compute in (
        match half with
        | {| sign := true; top := n |} => n
        | _ => 0
        end).

.. example::

   Let us project fields of a record, using either the applicative or projection syntax:

   .. coqtop:: all

      Eval compute in top half.
      Eval compute in bottom half.
      Eval compute in Rat_bottom_cond half.
      Eval compute in half.(top).

The following settings let you control the display format for types:

.. flag:: Printing Records

   When this :term:`flag` is on (this is the default),
   use the record syntax (shown above) as the default display format.

You can override the display format for specified types by adding entries to these tables:

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

.. note:: Records defined with the :cmd:`Record` command are not supposed to be
   recursive. To define recursive records, one can use the
   :cmd:`Inductive` and :cmd:`CoInductive` commands, resulting in an inductive or coinductive record.
   Definition of mutually inductive or coinductive records are also allowed, as long
   as all of the types in the block are records.

.. note:: Induction schemes are automatically generated for inductive records.
   Automatic generation of elimination schemes for non-recursive records
   defined with the :cmd:`Record` command can be activated with the
   :flag:`Nonrecursive Elimination Schemes` flag (see :ref:`proofschemes-induction-principles`).

.. note:: Records exist in two flavors. In the first
   implementation, a record :n:`@ident` with parameters :n:`{* @binder }`,
   constructor :n:`@ident__0`, and fields :n:`{* @name @field_body }`
   is represented as a variant type with a single
   constructor: :n:`Variant @ident {* @binder } : @sort := @ident__0
   {* ( @name @field_body ) }` and projections are defined by case analysis.
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
