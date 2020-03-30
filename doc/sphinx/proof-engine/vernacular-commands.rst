.. _vernacularcommands:

Vernacular commands
=============================

.. _displaying:

Displaying
----------


.. _Print:

.. cmd:: Print {? Term } @smart_qualid {? @univ_name_list }

   .. insertprodn univ_name_list univ_name_list

   .. prodn::
      univ_name_list ::= @%{ {* @name } %}

   This command displays information about the object identified by :n:`@smart_qualid`.
   The presence of :n:`Term` doesn't affect the behavior.

   When :n:`@univ_name_list` clause is provided, it locally renames the
   polymorphic universes of :n:`@smart_qualid`.
   The name `_` means the usual name is printed.

   .. exn:: @qualid not a defined object.
      :undocumented:

   .. exn:: Universe instance should have length @num.
      :undocumented:

   .. exn:: This object does not support universe names.
      :undocumented:


.. cmd:: About @smart_qualid {? @univ_name_list }

   This displays various information about the object
   denoted by :n:`@smart_qualid`: its kind (module, constant, assumption, inductive,
   constructor, abbreviation, …), long name, type, implicit arguments and
   argument scopes. It does not print the body of definitions or proofs.


.. cmd:: Print All

   This command displays information about the current state of the
   environment, including sections and modules.

.. cmd:: Inspect @num

   This command displays the :n:`@num` last objects of the
   current environment, including sections and modules.

.. cmd:: Print Section @qualid

   Displays the objects defined since the beginning of the section named :n:`@qualid`.

   .. todo: "A.B" is permitted but unnecessary for modules/sections.
      should the command just take an @ident?


.. _flags-options-tables:

Flags, Options and Tables
-----------------------------

Coq has many settings to control its behavior.  Setting types include flags, options
and tables:

* A :production:`flag` has a boolean value, such as :flag:`Asymmetric Patterns`.
* An :production:`option` generally has a numeric or string value, such as :opt:`Firstorder Depth`.
* A :production:`table` contains a set of strings or qualids.
* In addition, some commands provide settings, such as :cmd:`Extraction Language`.

.. FIXME Convert "Extraction Language" to an option.

Flags, options and tables are identified by a series of identifiers, each with an initial
capital letter.

.. cmd:: Set @setting_name
   :name: Set flag

   .. insertprodn setting_name setting_name

   .. prodn::
      setting_name ::= {+ @ident }

   Sets the flag :n:`@setting_name` to on.

   This command supports the :attr:`local`, :attr:`global` and :attr:`export` attributes.
   They are described :ref:`here <set_unset_scope_qualifiers>`.

   .. todo "Set Hyps Limit." gives "Bad type of value for this option: expected int, got bool."
      I can create an issue--when do you think someone might get to this?  The issue backlog
      is another topic for discussion/action.

   .. exn:: There is no option @setting_name.

      This message also appears for unknown flags.

.. cmd:: Set @setting_name {| @int | @string }
   :name: Set option

   Sets the option :n:`@setting_name` to the specified value, which
   must be of the appropriate type for the option.

   This command supports the :attr:`local`, :attr:`global` and :attr:`export` attributes.
   They are described :ref:`here <set_unset_scope_qualifiers>`.

.. cmd:: Unset @setting_name
   :name: Unset

   If :n:`@setting_name` is a flag, it is set to off.  If :n:`@setting_name` is an option, it is
   set to its default value.

   This command supports the :attr:`local`, :attr:`global` and :attr:`export` attributes.
   They are described :ref:`here <set_unset_scope_qualifiers>`.

   .. todo doesn't work for "Diffs"

.. cmd:: Test @setting_name
   :name: Test flag/option

   Prints the current value of the flag or option :n:`@setting_name`.

.. cmd:: Print Options

   Prints the current value of all flags and options, and the names of all tables.


.. cmd:: Add @setting_name {+ {| @qualid | @string } }
   :name: Add table

   Adds the specified values to the table :n:`@setting_name`.

.. cmd:: Remove @setting_name {+ {| @qualid | @string } }
   :name: Remove table

   Removes the specified value from the table :n:`@setting_name`.

.. cmd:: Test @setting_name {? for {+ {| @qualid | @string } } }
   :name: Test table

   If a value is given, reports whether the table :n:`@setting_name` contains the specified value.
   Otherwise, this is equivalent to :cmd:`Print Table`.

.. cmd:: Print Table @setting_name

   Prints the values in the table :n:`@setting_name`.

.. cmd:: Print Tables

   A synonym for :cmd:`Print Options`.

.. _set_unset_scope_qualifiers:

Locality attributes supported by :n:`Set` and :cmd:`Unset`
````````````````````````````````````````````````````````````

The :n:`Set` and :cmd:`Unset` commands support the :attr:`local`,
:attr:`global` and :attr:`export` locality attributes:

* no attribute: the original setting is *not* restored at the end of
  the current module or section.
* :attr:`local` (an alternative syntax is to use the ``Local``
  prefix): the setting is applied within the current module or
  section.  The original value of the setting is restored at the end
  of the current module or section.
* :attr:`export` (an alternative syntax is to use the ``Export``
  prefix): similar to :attr:`local`, the original value of the setting
  is restored at the end of the current module or section.  In
  addition, if the value is set in a module, then :cmd:`Import`\-ing
  the module sets the option or flag.
* :attr:`global` (an alternative syntax is to use the ``Global``
  prefix): the original setting is *not* restored at the end of the
  current module or section.  In addition, if the value is set in a
  file, then :cmd:`Require`\-ing the file sets the option.

Newly opened modules and sections inherit the current settings.

.. note::

   The use of the :attr:`global` attribute with the :n:`Set` and
   :cmd:`Unset` commands is discouraged.  If your goal is to define
   project-wide settings, you should rather use the command-line
   arguments ``-set`` and ``-unset`` for setting flags and options
   (cf. :ref:`command-line-options`).

.. _requests-to-the-environment:

Requests to the environment
-------------------------------

.. cmd:: Check @term

   This command displays the type of :n:`@term`. When called in proof mode, the
   term is checked in the local context of the current subgoal.

   Prefix the command with :n:`@selector:` to specify which subgoal to check
   (see Section :ref:`invocation-of-tactics`).

.. cmd:: Eval @red_expr in @term

   This command performs the specified reduction on :n:`@term`, and displays
   the resulting term with its type. The term to be reduced may depend on
   hypothesis introduced in the first subgoal (if a proof is in
   progress).

   .. seealso:: Section :ref:`performingcomputations`.


.. cmd:: Compute @term

   This command performs a call-by-value evaluation of term by using the
   bytecode-based virtual machine. It is a shortcut for ``Eval vm_compute in``
   :n:`@term`.

   .. seealso:: Section :ref:`performingcomputations`.


.. cmd:: Print Assumptions @smart_qualid

   Displays all the assumptions (axioms, parameters and
   variables) a theorem or definition depends on.

   .. exn:: Closed under the global context

      The theorem or definition doesn't depend on any assumptions.

.. cmd:: Print Opaque Dependencies @smart_qualid

   Displays the assumptions and opaque constants that :n:`@smart_qualid` depends on.

.. cmd:: Print Transparent Dependencies @smart_qualid

   Displays the assumptions and  transparent constants that :n:`@smart_qualid` depends on.

.. cmd:: Print All Dependencies @smart_qualid

   Displays all the assumptions and constants :n:`@smart_qualid` relies on.


.. cmd:: Search {+ @search_item } {? {| inside | outside } {+ @qualid } }

   .. insertprodn search_item search_item

   .. prodn::
      search_item ::= {? - } @string {? % @ident }
      | {? - } @one_term

   This command displays the name and type of objects (hypothesis of
   the current goal, theorems, axioms, etc) in the current context.
   This command is useful to remind the user of the names of library lemmas.

   .. todo above: "current context" meaning on the current goal or the first goal?
      below: adapted the old wording, doesn't seem consistent
      also will "invocation-of-tactics" cover the command case, or will there be a new section?

   * :n:`@string  {? % @ident }` - If :n:`@string` is a valid identifier,
     search for objects whose name contains :n:`@string`. If :n:`@string` is a
     notation's string for some reference :n:`@qualid` (referred to by its
     main symbol as in `"+"` or by its notation’s string as in `"_ + _"` or
     `"_ 'U' _"`, see Section :ref:`notations`) the command works like :cmd:`Search` :n:`@qualid`.

     .. todo: don't understand the 2nd sentence; notation's string -> notation string?
        what if it's not a valid identifier?  error or does something different?
        this is a substring match, e.g. "abc" matches "bc", right?

     .. todo would be nice if :n: had syntax for embedding :cmd: et al

     Specifying :n:`@ident` limits this to interpretation in the scope bound to
     the delimiting key :token:`ident` (see Section :ref:`LocalInterpretationRulesForNotations`).

     .. todo: does delimiting key mean anything special?

   * :n:`@one_term` - Search for objects containing a subterm matching the pattern
     :token:`one_term` in which holes of the pattern are indicated by `_` or,
     when a non linear pattern is specified, :n:`?@ident`.

      .. todo: "all statements or types of definition" is equivalent to "objects"?
         Should non linear pattern be defined or reworded?  I had never run across it.
         Not so keen on the term "holes", maybe reword to remove

   * :n:`{? - }` - excludes objects that contain the :n:`@string` or :n:`@one_term`
   * :n:`inside {+ @qualid }` - limit the search to the specified modules
   * :n:`outside {+ @qualid }` - exclude the specified modules from the search

   Prefix the command with :n:`@selector:` to specify the goal on which to search hypothesis.
   By default the first goal is searched. (see Section :ref:`invocation-of-tactics`).

   .. exn:: The reference @qualid was not found in the current environment.

      There is no constant in the environment named qualid.

   .. cmdv:: Search {+ {? -}@term_pattern_string}

      where
      :n:`@term_pattern_string` is a term_pattern, a string, or a string followed
      by a scope delimiting key `%key`.  This generalization of ``Search`` searches
      for all objects whose statement or type contains a subterm matching
      :n:`@term_pattern` (or :n:`@qualid` if :n:`@string` is the notation for a reference
      qualid) and whose name contains all string of the request that
      correspond to valid identifiers.

      .. todo Isn't this the same as for @string?  But the wording was different than
         the other one with @term_pattern_string

      .. example:: :cmd:`Search` examples

         .. coqtop:: in

            Require Import ZArith.

         .. coqtop:: all

            Search Z.mul Z.add "distr".

            Search "+"%Z "*"%Z "distr" -positive -Prop.

            Search (?x * _ + ?x * _)%Z outside OmegaLemmas.


.. cmd:: SearchHead @one_term {? {| inside | outside } {+ @qualid } }

   This command displays the name and type of all hypothesis of the
   current goal (if any) and theorems of the current context whose
   statement’s conclusion has the form `(term t1 .. tn)`. This command is
   useful to remind the user of the name of library lemmas.

   Prefix the command with :n:`@selector:` to specify the goal on which to search hypothesis.
   By default the first goal is searched. (see Section :ref:`invocation-of-tactics`).

   .. example::

      .. coqtop:: reset all

         SearchHead le.

         SearchHead (@eq bool).

   .. cmdv:: SearchHead @term inside {+ @qualid }

      This restricts the search to constructions defined in the modules named
      by the given :n:`qualid` sequence.

   .. cmdv:: SearchHead @term outside {+ @qualid }

      This restricts the search to constructions not defined in the modules
      named by the given :n:`qualid` sequence.

   .. exn:: Module/section @qualid not found.

      No module :n:`@qualid` has been required (see Section :ref:`compiled-files`).


.. cmd:: SearchPattern @one_term {? {| inside | outside } {+ @qualid } }

   This command displays the name and type of all hypothesis of the
   current goal (if any) and theorems of the current context whose
   statement’s conclusion or last hypothesis and conclusion matches the
   expressionterm where holes in the latter are denoted by `_`.
   It is a variant of :n:`Search @term_pattern` that does not look for subterms
   but searches for statements whose conclusion has exactly the expected
   form, or whose statement finishes by the given series of
   hypothesis/conclusion.

   Prefix the command with :n:`@selector:` to specify the goal on which to search hypothesis.
   By default the first goal is searched. (see Section :ref:`invocation-of-tactics`).

   .. example::

      .. coqtop:: in

         Require Import Arith.

      .. coqtop:: all

         SearchPattern (_ + _ = _ + _).

         SearchPattern (nat -> bool).

         SearchPattern (forall l : list _, _ l l).

   Patterns need not be linear: you can express that the same expression
   must occur in two places by using pattern variables `?ident`.


   .. example::

      .. coqtop:: all

         SearchPattern (?X1 + _ = _ + ?X1).

   .. cmdv:: SearchPattern @term inside {+ @qualid }

      This restricts the search to constructions defined in the modules
      named by the given :n:`qualid` sequence.

   .. cmdv:: SearchPattern @term outside {+ @qualid }

      This restricts the search to constructions not defined in the modules
      named by the given :n:`qualid` sequence.

.. cmd:: SearchRewrite @one_term {? {| inside | outside } {+ @qualid } }

   This command displays the name and type of all hypothesis of the
   current goal (if any) and theorems of the current context whose
   statement’s conclusion is an equality of which one side matches the
   expression term. Holes in term are denoted by “_”.

   Prefix the command with :n:`@selector:` to specify the goal on which to search hypothesis.
   By default the first goal is searched. (see Section :ref:`invocation-of-tactics`).

   .. example::

      .. coqtop:: in

         Require Import Arith.

      .. coqtop:: all

         SearchRewrite (_ + _ + _).

   .. cmdv:: SearchRewrite @term inside {+ @qualid }

      This restricts the search to constructions defined in the modules
      named by the given :n:`qualid` sequence.

   .. cmdv:: SearchRewrite @term outside {+ @qualid }

      This restricts the search to constructions not defined in the modules
      named by the given :n:`qualid` sequence.

.. note::

   .. table:: Search Blacklist @string
      :name: Search Blacklist

      Specifies a set of strings used to exclude lemmas from the results of :cmd:`Search`,
      :cmd:`SearchHead`, :cmd:`SearchPattern` and :cmd:`SearchRewrite` queries.  A lemma whose
      fully-qualified name contains any of the strings will be excluded from the
      search results.  The default blacklisted substrings are ``_subterm``, ``_subproof`` and
      ``Private_``.

      Use the :cmd:`Add table` and :cmd:`Remove table` commands to update the set of
      blacklisted strings.

.. cmd:: Locate @locatable

   .. insertprodn locatable locatable

   .. prodn::
      locatable ::= @smart_qualid
      | Term @smart_qualid
      | Module @qualid
      | Ltac @qualid
      | Library @qualid
      | File @string

   Displays the full name of objects whose name is a prefix
   of the specified value, and consequently the |Coq| module in
   which they are defined. It searches for objects from the different
   qualified namespaces of |Coq|: terms, modules, Ltac, etc.

   The alternatives have the following behavior:

   -  :n:`Locate @smart_qualid` - displays the full name of objects whose name is a prefix
      of :n:`@smart_qualid`, thereby showing the module they are defined in.
   -  :n:`Locate Term @smart_qualid` - limits the search to terms
   -  :n:`Locate Module @qualid` - limits the search to modules
   -  :n:`Locate Ltac @qualid` - reports whether the specified module has been loaded
   -  :n:`Locate Library @qualid` - gives the status of the specified |Coq| module. It reports whether
      the module is loaded and, if not, searches for the module in the load path and reports
      where it is found.
   -  :n:`Locate File @string` - displays the location of a file specified by :n:`@string`
      Typically, :n:`@string` is a ``.cmo`` or ``.vo`` or ``.v`` file, such as :n:`Nat.v`.

      .. todo: "Data" also works (parent in dirpath of Nat.v)

   .. example::

      .. coqtop:: all

         Locate nat.
         Locate Datatypes.O.
         Locate Init.Datatypes.O.
         Locate Coq.Init.Datatypes.O.
         Locate I.Dont.Exist.

.. seealso:: Section :ref:`locating-notations`

.. _printing-flags:

Printing flags
-------------------------------

.. flag:: Fast Name Printing

   When turned on, |Coq| uses an asymptotically faster algorithm for the
   generation of unambiguous names of bound variables while printing terms.
   While faster, it is also less clever and results in a typically less elegant
   display, e.g. it will generate more names rather than reusing certain names
   across subterms. This flag is not enabled by default, because as Ltac
   observes bound names, turning it on can break existing proof scripts.


.. _loading-files:

Loading files
-----------------

|Coq| offers the possibility of loading different parts of a whole
development stored in separate files. Their contents will be loaded as
if they were entered from the keyboard. This means that the loaded
files are ASCII files containing sequences of commands for |Coq|’s
toplevel. This kind of file is called a *script* for |Coq|. The standard
(and default) extension of |Coq|’s script files is .v.


.. cmd:: Load {? Verbose } {| @string | @ident }

   Loads a file.  If :n:`@ident` is specified, the command loads a file
   named :n:`ident`.v, searching successively in
   each of the directories specified in the *loadpath*. (see Section
   :ref:`libraries-and-filesystem`)

   If :n:`@string` is specified, it must specify a complete filename.
   `~` and .. abbreviations are
   allowed as well as shell variables. If no extension is specified, |Coq|
   will use the default extension ``.v``.

   Files loaded this way can't leave proofs open, nor can :cmd:`Load`
   be used inside a proof.

   We discourage the use of :cmd:`Load`; use :cmd:`Require` instead.
   :cmd:`Require` loads `.vo` files that were previously
   compiled from `.v` files.

   :n:`Verbose` displays the |Coq| output for each command and tactic
   in the loaded file, as if the commands and tactics were entered interactively.

   .. seealso:: Section :ref:`controlling-display`.

   .. exn:: Can’t find file @ident on loadpath.
      :undocumented:

   .. exn:: Load is not supported inside proofs.
      :undocumented:

   .. exn:: Files processed by Load cannot leave open proofs.
      :undocumented:

.. _compiled-files:

Compiled files
------------------

This section describes the commands used to load compiled files (see
Chapter :ref:`thecoqcommands` for documentation on how to compile a file). A compiled
file is a particular case of module called *library file*.


.. cmd:: Require {? {| Import | Export } } {+ @qualid }

   Loads compiled modules into the |Coq| environment.  The command
   searches the loadpath for the modules named by the :n:`@qualid`\s.  Each :n:`@qualid`
   has the form :n:`@dirpath.@ident`.  The commmand maps :n:`@dirpath` to a physical
   directory (see Section :ref:`libraries-and-filesystem`) and loads the compiled
   file :n:`@ident.vo` from that directory.  (Compiling :n:`@ident.v` generates :n:`@ident.vo`.)

   The process is applied recursively to all the loaded modules.
   The compiled files must have been compiled with the same version of |Coq|.
   The compiled files are neither replayed nor rechecked.

   * :n:`Import` - TBA
   * :n:`Export` - TBA

   .. todo: I can't make sense of the old explanations for Import and Export

   The mapping between
   physical directories and logical names at the time of requiring the
   file must be consistent with the mapping used to compile the file. If
   several files match, one of them is picked in an unspecified fashion.

   .. todo both sentences still true?  Second sentence is unsettling

   .. todo: remove the following 3 variants

   .. cmdv:: Require Import @qualid
      :name: Require Import

      This loads and declares the module :n:`@qualid`
      and its dependencies then imports the contents of :n:`@qualid` as described
      for :cmd:`Import`. It does not import the modules that
      :n:`@qualid` depends on unless these modules were themselves required in module
      :n:`@qualid`
      using :cmd:`Require Export`, or if they are recursively required
      through a series of :cmd:`Require Export`\s.  If the required module has
      already been loaded, :cmd:`Require Import` :n:`@qualid` simply imports it, as
      :cmd:`Import` :n:`@qualid` would.

   .. cmdv:: Require Export @qualid
      :name: Require Export

      This command acts as :cmd:`Require Import` :n:`@qualid`,
      but if a further module, say `A`, contains a command :cmd:`Require Export` `B`,
      then the command :cmd:`Require Import` `A` also imports the module `B.`

   .. cmdv:: Require {| Import | Export } {+ @qualid }

      This loads the
      modules named by the :token:`qualid` sequence and their recursive
      dependencies. If
      ``Import`` or ``Export`` is given, it also imports these modules and
      all the recursive dependencies that were marked or transitively marked
      as ``Export``.

.. cmd:: From @qualid Require {? {| Import | Export } } {+ @qualid }
   :name: From ... Require ...

   Works like :cmd:`Require`, but picks
   any library whose absolute name is of the form :n:`@dirpath.@dirpath’.@qualid`
   for some :n:`@dirpath’`. This is useful to ensure that the :token:`qualid` library
   comes from a given package by making explicit its absolute root.

   .. exn:: Cannot load qualid: no physical path bound to dirpath.
      :undocumented:

   .. exn:: Cannot find library foo in loadpath.

      The command did not find the
      file foo.vo. Either foo.v exists but is not compiled or foo.vo is in a
      directory which is not in your LoadPath (see Section :ref:`libraries-and-filesystem`).

   .. exn:: Compiled library @ident.vo makes inconsistent assumptions over library qualid.

      The command tried to load library file :n:`@ident`.vo that
      depends on some specific version of library :n:`@qualid` which is not the
      one already loaded in the current |Coq| session. Probably :n:`@ident.v` was
      not properly recompiled with the last version of the file containing
      module :token:`qualid`.

   .. exn:: Bad magic number.

      The file :n:`@ident.vo` was found but either it is not a
      |Coq| compiled module, or it was compiled with an incompatible
      version of |Coq|.

   .. exn:: The file :n:`@ident.vo` contains library dirpath and not library dirpath’.

      The library file :n:`@dirpath’` is indirectly required by the
      ``Require`` command but it is bound in the current loadpath to the
      file :n:`@ident.vo` which was bound to a different library name :token:`dirpath` at
      the time it was compiled.


   .. warn:: Require inside a module is deprecated and strongly discouraged. You can Require a module at toplevel and optionally Import it inside another one.

      Note that the :cmd:`Import` and :cmd:`Export` commands can be used inside modules.

      .. seealso:: Chapter :ref:`thecoqcommands`

.. cmd:: Print Libraries

   This command displays the list of library files loaded in the
   current |Coq| session. For each of these libraries, it also tells if it
   is imported.


.. cmd:: Declare ML Module {+ @string }

   This commands dynamically loads the specified OCaml compiled files.
   It is mainly used to load tactics dynamically. The
   files are searched into the current OCaml loadpath (see the
   command :cmd:`Add ML Path`).

   .. todo "searched into" meaning?

   This command supports the :attr:`local` attribute.  If present,
   the listed files are not exported, even if they're outside a section.

   .. exn:: File not found on loadpath: @string.
      :undocumented:

   .. exn:: Loading of ML object file forbidden in a native Coq.
      :undocumented:


.. cmd:: Print ML Modules

   This prints the name of all OCaml modules loaded with :cmd:`Declare ML Module`.
   To know from where these module were loaded, the user
   should use the command :cmd:`Locate` ``File``.


.. _loadpath:

Loadpath
------------

Loadpaths are preferably managed using |Coq| command line options (see
Section `libraries-and-filesystem`) but there remain vernacular commands to manage them
for practical purposes. Such commands are only meant to be issued in
the toplevel, and using them in source files is discouraged.


.. cmd:: Pwd

   This command displays the current working directory.


.. cmd:: Cd {? @string }

   If :n:`@string` is specified, changes the current directory according to :token:`string` which
   can be any valid path.  Otherwise, it displays the current directory.

   .. todo Not like Linux `cd`.  Maybe change?


.. cmd:: Add LoadPath @string as @dirpath

   .. insertprodn dirpath dirpath

   .. prodn::
      dirpath ::= @ident
      | @dirpath @field_ident

   This command is equivalent to the command line option
   :n:`-Q @string @dirpath`. It adds the physical directory string to the current
   |Coq| loadpath and maps it to the logical directory dirpath.


.. cmd:: Add Rec LoadPath @string as @dirpath

   This command is equivalent to the command line option
   :n:`-R @string @dirpath`. It adds the physical directory string and all its
   subdirectories to the current |Coq| loadpath.


.. cmd:: Remove LoadPath @string

   This command removes the path :n:`@string` from the current |Coq| loadpath.


.. cmd:: Print LoadPath {? @dirpath }

   This command displays the current |Coq| loadpath.  If :n:`@dirpath` is specified,
   displays only the paths that extend that prefix.


.. cmd:: Add ML Path @string

   This command adds the path :n:`@string` to the current OCaml
   loadpath (see the command `Declare ML Module`` in Section :ref:`compiled-files`).


.. cmd:: Print ML Path

   This command displays the current OCaml loadpath. This
   command makes sense only under the bytecode version of ``coqtop``, i.e.
   using option ``-byte``
   (see the command Declare ML Module in Section :ref:`compiled-files`).


.. _backtracking:

Backtracking
----------------

The backtracking commands described in this section can only be used
interactively, they cannot be part of a vernacular file loaded via
``Load`` or compiled by ``coqc``.


.. cmd:: Reset @ident

   This command removes all the objects in the environment since :n:`@ident`
   was introduced, including :n:`@ident`. :n:`@ident` may be the name of a defined or
   declared object as well as the name of a section. One cannot reset
   over the name of a module or of an object inside a module.

   .. exn:: @ident: no such entry.
      :undocumented:

.. cmd:: Reset Initial

   Goes back to the initial state, just after the start
   of the interactive session.


.. cmd:: Back {? @num }

   This command undoes all the effects of the last vernacular command.
   Commands read from a vernacular file via a :cmd:`Load` are considered as a
   single command. Proof management commands are also handled by this
   command (see Chapter :ref:`proofhandling`). For that, Back may have to undo more than
   one command in order to reach a state where the proof management
   information is available. For instance, when the last command is a
   :cmd:`Qed`, the management information about the closed proof has been
   discarded. In this case, :cmd:`Back` will then undo all the proof steps up to
   the statement of this proof.

   If :n:`@num` is specified, the command undoes :n:`@num` vernacular commands.

   Back will not reopen a closed proof, but instead will go to the state just before that
   proof.

   .. exn:: Invalid backtrack.

      The user wants to undo more commands than available in the history.

.. cmd:: BackTo @num

   This command brings back the system to the state labeled :n:`@num`,
   forgetting the effect of all commands executed after this state. The
   state label is an integer which grows after each successful command.
   It is displayed in the prompt when in -emacs mode. Just as :cmd:`Back` (see
   above), the :cmd:`BackTo` command now handles proof states. For that, it may
   have to undo some extra commands and end on a state :n:`@num′ ≤ @num` if
   necessary.

.. _quitting-and-debugging:

Quitting and debugging
--------------------------

.. cmd:: Quit

   Causes |Coq| to exit.  Valid only in coqtop.


.. cmd:: Drop

   This command temporarily enters the OCaml toplevel.
   It is a debug facility used by |Coq|’s implementers.  Valid only in coqtop.
   The OCaml command:

   ::

      #use "include";;

   adds the right loadpaths and loads some toplevel printers for all
   abstract types of |Coq|- section_path, identifiers, terms, judgments, ….
   You can also use the file base_include instead, that loads only the
   pretty-printers for section_paths and identifiers. You can return back
   to |Coq| with the command:

   ::

      go();;

   .. warning::

      #. It only works with the bytecode version of |Coq| (i.e. `coqtop.byte`,
         see Section `interactive-use`).
      #. You must have compiled |Coq| from the source package and set the
         environment variable COQTOP to the root of your copy of the sources
         (see Section `customization-by-environment-variables`).


.. TODO : command is not a syntax entry

.. cmd:: Time @command

   This command executes the vernacular command :n:`@command` and displays the
   time needed to execute it.


.. cmd:: Redirect @string @command

   This command executes the vernacular command :n:`@command`, redirecting its
   output to ":n:`@string`.out".


.. cmd:: Timeout @num @command

   This command executes the vernacular command :n:`@command`. If the command
   has not terminated after the time specified by the :n:`@num` (time
   expressed in seconds), then it is interrupted and an error message is
   displayed.

   .. opt:: Default Timeout @num
      :name: Default Timeout

      This option controls a default timeout for subsequent commands, as if they
      were passed to a :cmd:`Timeout` command. Commands already starting by a
      :cmd:`Timeout` are unaffected.


.. cmd:: Fail @command

   For debugging scripts, sometimes it is desirable to know whether a
   command or a tactic fails. If the given :n:`@command` fails, then
   :n:`Fail @command` succeeds (excepts in the case of
   critical errors, like a "stack overflow"), without changing the
   proof state, and in interactive mode, the system prints a message
   confirming the failure.

   .. exn:: The command has not failed!

      If the given :n:`@command` succeeds, then :n:`Fail @command`
      fails with this error message.

.. note:: The commands that include a :n:`@command` (currently
   :cmd:`Time`, :cmd:`Redirect`, :cmd:`Timeout` and :cmd:`Fail`) can't
   use :cmd:`Drop`, :cmd:`Quit`, :cmd:`BackTo`, :cmd:`Show Goal`
   or :cmd:`Show Proof` as the :n:`@command`.  This is a syntatic restriction.
   The documentation describes all of these as :n:`@command`\s for simplicity.

.. _controlling-display:

Controlling display
-----------------------

.. flag:: Silent

   This flag controls the normal displaying.

.. opt:: Warnings "{+, {? {| - | + } } @ident }"
   :name: Warnings

   This option configures the display of warnings. It is experimental, and
   expects, between quotes, a comma-separated list of warning names or
   categories. Adding - in front of a warning or category disables it, adding +
   makes it an error. It is possible to use the special categories all and
   default, the latter containing the warnings enabled by default. The flags are
   interpreted from left to right, so in case of an overlap, the flags on the
   right have higher priority, meaning that `A,-A` is equivalent to `-A`.

.. flag:: Search Output Name Only

   This flag restricts the output of search commands to identifier names;
   turning it on causes invocations of :cmd:`Search`, :cmd:`SearchHead`,
   :cmd:`SearchPattern`, :cmd:`SearchRewrite` etc. to omit types from their
   output, printing only identifiers.

.. opt:: Printing Width @num
   :name: Printing Width

   This command sets which left-aligned part of the width of the screen is used
   for display. At the time of writing this documentation, the default value
   is 78.

.. opt:: Printing Depth @num
   :name: Printing Depth

   This option controls the nesting depth of the formatter used for pretty-
   printing. Beyond this depth, display of subterms is replaced by dots. At the
   time of writing this documentation, the default value is 50.

.. flag:: Printing Compact Contexts

   This flag controls the compact display mode for goals contexts. When on,
   the printer tries to reduce the vertical size of goals contexts by putting
   several variables (even if of different types) on the same line provided it
   does not exceed the printing width (see :opt:`Printing Width`). At the time
   of writing this documentation, it is off by default.

.. flag:: Printing Unfocused

   This flag controls whether unfocused goals are displayed. Such goals are
   created by focusing other goals with bullets (see :ref:`bullets` or
   :ref:`curly braces <curly-braces>`). It is off by default.

.. flag:: Printing Dependent Evars Line

   This flag controls the printing of the “(dependent evars: …)” information
   after each tactic.  The information is used by the Prooftree tool in Proof
   General. (https://askra.de/software/prooftree)


.. _vernac-controlling-the-reduction-strategies:

Controlling the reduction strategies and the conversion algorithm
----------------------------------------------------------------------


|Coq| provides reduction strategies that the tactics can invoke and two
different algorithms to check the convertibility of types. The first
conversion algorithm lazily compares applicative terms while the other
is a brute-force but efficient algorithm that first normalizes the
terms before comparing them. The second algorithm is based on a
bytecode representation of terms similar to the bytecode
representation used in the ZINC virtual machine :cite:`Leroy90`. It is
especially useful for intensive computation of algebraic values, such
as numbers, and for reflection-based tactics. The commands to fine-
tune the reduction strategies and the lazy conversion algorithm are
described first.

.. cmd:: Opaque {+ @smart_qualid }

   This command accepts the :attr:`global` attribute.  By default, the scope
   of :cmd:`Opaque` is limited to the current section or file.

   This command has an effect on unfoldable constants, i.e. on constants
   defined by :cmd:`Definition` or :cmd:`Let` (with an explicit body), or by a command
   assimilated to a definition such as :cmd:`Fixpoint`, :cmd:`Program Definition`, etc,
   or by a proof ended by :cmd:`Defined`. The command tells not to unfold the
   constants in the :n:`@smart_qualid` sequence in tactics using δ-conversion (unfolding
   a constant is replacing it by its definition).

   :cmd:`Opaque` has also an effect on the conversion algorithm of |Coq|, telling
   it to delay the unfolding of a constant as much as possible when |Coq|
   has to check the conversion (see Section :ref:`conversion-rules`) of two distinct
   applied constants.

   .. seealso::

      Sections :ref:`performingcomputations`, :ref:`tactics-automating`,
      :ref:`proof-editing-mode`

   .. exn:: The reference @qualid was not found in the current environment.

      There is no constant referred by :n:`@qualid` in the environment.
      Nevertheless, if you asked :cmd:`Opaque` `foo` `bar` and if `bar` does
      not exist, `foo` is set opaque.

.. cmd:: Transparent {+ @smart_qualid }

   This command accepts the :attr:`global` attribute.  By default, the scope
   of :cmd:`Transparent` is limited to the current section or file.

   This command is the converse of :cmd:`Opaque` and it applies on unfoldable
   constants to restore their unfoldability after an Opaque command.

   Note in particular that constants defined by a proof ended by Qed are
   not unfoldable and Transparent has no effect on them. This is to keep
   with the usual mathematical practice of *proof irrelevance*: what
   matters in a mathematical development is the sequence of lemma
   statements, not their actual proofs. This distinguishes lemmas from
   the usual defined constants, whose actual values are of course
   relevant in general.

   .. exn:: The reference @smart_qualid was not found in the current environment.

      There is no constant :n:`@smart_qualid` defined in the environment.

      .. seealso::

         Sections :ref:`performingcomputations`,
         :ref:`tactics-automating`, :ref:`proof-editing-mode`

.. _vernac-strategy:

.. cmd:: Strategy {+ @strategy_level [ {+ @smart_qualid } ] }

   .. insertprodn strategy_level strategy_level

   .. prodn::
      strategy_level ::= opaque
      | @int
      | expand
      | transparent

   This command accepts the :attr:`local` attribute, which limits its effect
   to the current section or module, in which case the section and module
   behavior is the same as :cmd:`Opaque` and :cmd:`Transparent` (without :attr:`global`).

   This command generalizes the behavior of the :cmd:`Opaque` and :cmd:`Transparent`
   commands. It is used to fine-tune the strategy for unfolding
   constants, both at the tactic level and at the kernel level. This
   command associates a :n:`@strategy_level` with the qualified names in the :n:`@smart_qualid`
   sequence. Whenever two
   expressions with two distinct head constants are compared (for
   instance, this comparison can be triggered by a type cast), the one
   with lower level is expanded first. In case of a tie, the second one
   (appearing in the cast type) is expanded.

   Levels can be one of the following (higher to lower):

    + ``opaque`` : level of opaque constants. They cannot be expanded by
      tactics (behaves like +∞, see next item).
    + :n:`@num` : levels indexed by an integer. Level 0 corresponds to the
      default behavior, which corresponds to transparent constants. This
      level can also be referred to as transparent. Negative levels
      correspond to constants to be expanded before normal transparent
      constants, while positive levels correspond to constants to be
      expanded after normal transparent constants.
    + ``expand`` : level of constants that should be expanded first (behaves
      like −∞)
    + ``transparent`` : TBA

.. cmd:: Print Strategy @smart_qualid

   This command prints the strategy currently associated with :n:`@smart_qualid`. It
   fails if :n:`@smart_qualid` is not an unfoldable reference, that is, neither a
   variable nor a constant.

   .. exn:: The reference is not unfoldable.
      :undocumented:

.. cmd:: Print Strategies

   Print all the currently non-transparent strategies.


.. cmd:: Declare Reduction @ident := @red_expr

   This command allows giving a short name to a reduction expression, for
   instance ``lazy beta delta [foo bar]``. This short name can then be used
   in :n:`Eval @ident in` or ``eval`` directives. This command
   accepts the
   ``Local`` modifier, for discarding this reduction name at the end of the
   file or module. For the moment, the name is not qualified. In
   particular declaring the same name in several modules or in several
   functor applications will be rejected if these declarations are not
   local. The name :n:`@ident` cannot be used directly as an Ltac tactic, but
   nothing prevents the user from also performing a
   :n:`Ltac @ident := @red_expr`.

   .. seealso:: :ref:`performingcomputations`


.. _controlling-locality-of-commands:

Controlling the locality of commands
-----------------------------------------

.. attr:: global
          local

   Some commands support a :attr:`local` or :attr:`global` attribute
   to control the scope of their effect.  There is also a legacy (and
   much more commonly used) syntax using the ``Local`` or ``Global``
   prefixes (see :n:`@legacy_attr`).  There are four kinds of
   commands:

   + Commands whose default is to extend their effect both outside the
     section and the module or library file they occur in.  For these
     commands, the :attr:`local` attribute limits the effect of the command to the
     current section or module it occurs in.  As an example, the :cmd:`Coercion`
     and :cmd:`Strategy` commands belong to this category.
   + Commands whose default behavior is to stop their effect at the end
     of the section they occur in but to extend their effect outside the module or
     library file they occur in. For these commands, the :attr:`local` attribute limits the
     effect of the command to the current module if the command does not occur in a
     section and the :attr:`global` attribute extends the effect outside the current
     sections and current module if the command occurs in a section. As an example,
     the :cmd:`Arguments`, :cmd:`Ltac` or :cmd:`Notation` commands belong
     to this category. Notice that a subclass of these commands do not support
     extension of their scope outside sections at all and the :attr:`global` attribute is not
     applicable to them.
   + Commands whose default behavior is to stop their effect at the end
     of the section or module they occur in.  For these commands, the :attr:`global`
     attribute extends their effect outside the sections and modules they
     occur in.  The :cmd:`Transparent` and :cmd:`Opaque` commands
     belong to this category.
   + Commands whose default behavior is to extend their effect outside
     sections but not outside modules when they occur in a section and to
     extend their effect outside the module or library file they occur in
     when no section contains them. For these commands, the :attr:`local` attribute
     limits the effect to the current section or module while the :attr:`global`
     attribute extends the effect outside the module even when the command
     occurs in a section.  The :n:`Set` and :cmd:`Unset` commands belong to this
     category.

.. attr:: export

   Some commands support an :attr:`export` attribute.  The effect of
   the attribute is to make the effect of the command available when
   the module containing it is imported.  It is supported in
   particular by the :cmd:`Hint`, :n:`Set` and :cmd:`Unset`
   commands.

.. _controlling-typing-flags:

Controlling Typing Flags
----------------------------

.. flag:: Guard Checking

   This flag can be used to enable/disable the guard checking of
   fixpoints. Warning: this can break the consistency of the system, use at your
   own risk. Decreasing argument can still be specified: the decrease is not checked
   anymore but it still affects the reduction of the term. Unchecked fixpoints are
   printed by :cmd:`Print Assumptions`.

.. flag:: Positivity Checking

   This flag can be used to enable/disable the positivity checking of inductive
   types and the productivity checking of coinductive types. Warning: this can
   break the consistency of the system, use at your own risk. Unchecked
   (co)inductive types are printed by :cmd:`Print Assumptions`.

.. flag:: Universe Checking

   This flag can be used to enable/disable the checking of universes, providing a
   form of "type in type".  Warning: this breaks the consistency of the system, use
   at your own risk.  Constants relying on "type in type" are printed by
   :cmd:`Print Assumptions`. It has the same effect as `-type-in-type` command line
   argument (see :ref:`command-line-options`).

.. cmd:: Print Typing Flags

   Print the status of the three typing flags: guard checking, positivity checking
   and universe checking.

.. example::

   .. coqtop:: all reset

        Unset Guard Checking.

        Print Typing Flags.

        Fixpoint f (n : nat) : False
          := f n.

        Fixpoint ackermann (m n : nat) {struct m} : nat :=
          match m with
          | 0 => S n
          | S m =>
            match n with
            | 0 => ackermann m 1
            | S n => ackermann m (ackermann (S m) n)
            end
          end.

        Print Assumptions ackermann.

   Note that the proper way to define the Ackermann function is to use
   an inner fixpoint:

   .. coqtop:: all reset

        Fixpoint ack m :=
          fix ackm n :=
          match m with
          | 0 => S n
          | S m' =>
            match n with
            | 0 => ack m' 1
            | S n' => ack m' (ackm n')
            end
          end.


.. _internal-registration-commands:

Internal registration commands
--------------------------------

Due to their internal nature, the commands that are presented in this section
are not for general use. They are meant to appear only in standard libraries and
in support libraries of plug-ins.

.. _exposing-constants-to-ocaml-libraries:

Exposing constants to OCaml libraries
````````````````````````````````````````````````````````````````

.. cmd:: Register @qualid as @qualid

   This command exposes the constant :n:`@qualid__1` to OCaml libraries under
   the name :n:`@qualid__2`.  This constant can then be dynamically located
   calling :n:`Coqlib.lib_ref "@qualid__2"`; i.e., there is no need to known
   where is the constant defined (file, module, library, etc.).

   As a special case, when the first segment of :n:`@qualid__2` is :g:`kernel`,
   the constant is exposed to the kernel. For instance, the `Int63` module
   features the following declaration:

   .. coqdoc::

      Register bool as kernel.ind_bool.

   This makes the kernel aware of what is the type of boolean values. This
   information is used for instance to define the return type of the
   :g:`#int63_eq` primitive.

   .. seealso:: :ref:`primitive-integers`

Inlining hints for the fast reduction machines
````````````````````````````````````````````````````````````````

.. cmd:: Register Inline @qualid

   This command gives as a hint to the reduction machines (VM and native) that
   the body of the constant :n:`@qualid` should be inlined in the generated code.

Registering primitive operations
````````````````````````````````

.. cmd:: Primitive @ident {? : @term } := @register_token

   .. insertprodn register_token register_token

   .. prodn::
      register_token ::= #int63_type
      | #float64_type
      | #int63_head0
      | #int63_tail0
      | #int63_add
      | #int63_sub
      | #int63_mul
      | #int63_div
      | #int63_mod
      | #int63_lsr
      | #int63_lsl
      | #int63_land
      | #int63_lor
      | #int63_lxor
      | #int63_addc
      | #int63_subc
      | #int63_addcarryc
      | #int63_subcarryc
      | #int63_mulc
      | #int63_diveucl
      | #int63_div21
      | #int63_addmuldiv
      | #int63_eq
      | #int63_lt
      | #int63_le
      | #int63_compare
      | #float64_opp
      | #float64_abs
      | #float64_eq
      | #float64_lt
      | #float64_le
      | #float64_compare
      | #float64_classify
      | #float64_add
      | #float64_sub
      | #float64_mul
      | #float64_div
      | #float64_sqrt
      | #float64_of_int63
      | #float64_normfr_mantissa
      | #float64_frshiftexp
      | #float64_ldshiftexp
      | #float64_next_up
      | #float64_next_down

   Declares :n:`@ident__1` as the primitive operator :n:`#@ident__2`. When
   running this command, the type of the primitive should be already known by
   the kernel (this is achieved through this command for primitive types and
   through the :cmd:`Register` command with the :g:`kernel` name-space for other
   types).
