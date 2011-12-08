.. _Source_Navigation:

*****************
Source Navigation
*****************

.. index:: source navigation
.. index:: navigation

.. _Support_for_Cross-References:

Support for Cross-References
============================

.. index:: cross-references

GPS provides cross-reference navigation for program entities, such as types,
procedures, functions, variables, ..., defined in your application. The
cross-reference support in GPS relies on the compiler generated xref
information, which means that you need to either compile your project first
before being able to navigate, or use the menu `Build->Recompute Xref info`.
Similarly when your sources have been modified, you need to rebuild and
recompute xref information so that your changes are taken into account.

Here are language specific information about source navigation:

*Ada*
  .. index:: Ada
  .. index:: GNAT

  The GNAT compiler is used to generate the cross-references information needed
  by GPS by default, unless you are using the `-gnatD` or `-gnatx` switches, in
  which case no cross reference information will be available.

  .. index:: -gnatQ
  .. index:: -k

  If you need to navigate through sources that do not compile (e.g after
  modifications, or while porting an application), GNAT can still generate
  partial cross-reference information if you specify the `-gnatQ` compilation
  option. Along with the `-k` option of gnatmake, it is then possible to
  generate as much relevant information as possible for your non compilable
  sources.

  .. index:: ALI

  There are a few special cases where GPS cannot find the external file (called
  :file:`ALI file`) that contains the cross-reference information. Most likely,
  this is either because you haven't compiled your sources yet, or because the
  source code has changed since the :file:`ALI file` was generated.

  .. index:: project

  It could also be that you haven't included in the project the object
  directories that contain the :file:`ALI files`.

  .. index:: separate unit

  In addition, one special case cannot be handled automatically. This is for
  separate units, whose file names have been crunched through the *gnatkr*
  command. To handle this, you should force GPS to parse all the :file:`ALI
  files` in the appropriate object directory. This is done by right-clicking on
  the object directory in the project view (left-side panel on the main
  window), and selecting the menu "Parse all xref information".


*C/C++*
  .. index:: C
  .. index:: C++

  The GCC C and C++ compilers that come with GNAT need to be used to generate
  the cross-references information needed by GPS, via the `-fdump-xref` switch.
  This means that you need to first add the `-fdump-xref` switch to your
  project's switches for C and C++ sources, and compile your application before
  you browse through the cross-references or view various graphs in GPS.  If
  sources have been modified, you should recompile the modified files.

Loading xref info in memory
---------------------------

.. index:: Load xref info in memory

The cross-reference information, as mentioned above, is generated either by the
compiler when you recompile your sources, or explicitly when you select the
menu `Build->Recompute Xref info`.

This information will be loaded in memory automatically by GPS when it needs
it, and as little as possible, to limit the memory footprint. However, this
means that some operations, for instance searching for all the references to a
global entity, will need to parse most, if not all, of the cross-reference
information. This will slow done the search the first time (and then the
information is in memory and the search is fast, unless the cross-reference
information has been regenerated on the disk).

You can select the menu `Build->Load xref info in memory` to force GPS to load
all the available information immediately in memory. This will speed up future
queries.

Note that GPS always loads all xref information for C and C++ sources in order
to provide accurate source navigation, so this menu is mainly relevant for Ada
sources.

A preference can be set to have GPS load the cross-information automatically on
startup, :ref:`The_Preferences_Dialog`.

Ada xrefs heuristics
--------------------

.. index:: Ada xrefs heuristics

GPS is able to provide some basic navigation support for Ada sources in the
absence of information coming from the compiler. It uses a built-in Ada parser
parsing the Ada files at startup and allowing navigation in simple cases.

In this mode, GPS is able to navigate to an entity body from the declaration,
and to an entity declaration from the body. In case of other references, GPS
will navigate to the declaration on simple cases, namely if the heuristics
provide an information without ambiguity. In particular, it may not work with
overloaded entities.

These heuristics are not used in global reference searching operations or call
graphs.

Note that this parser is also used to provide the Ada outline view, code
completion and entity view.

.. _The_Navigate_Menu:

The Navigate Menu
=================

.. index:: navigate

*Find or Replace...*
  .. index:: find
  .. index:: search
  .. index:: replace

  Open the find and replace dialog. :ref:`Searching_and_Replacing`.

*Find Next*
  .. index:: find next

  Find next occurrence of the current search. :ref:`Searching_and_Replacing`.

*Find Previous*
  .. index:: find previous

  Find previous occurrence of the current search.
  :ref:`Searching_and_Replacing`.


*Find All References*
  .. _Find_All_References:

  .. index:: find all references

  Find all the references to the current entity in the project. The search is
  based on the semantic information extracted from the sources, this is not a
  simple text search. The result of the search is displayed in the location
  window, see :ref:`The_Locations_View`.

*Goto Declaration*
  .. index:: goto declaration

  Go to the declaration/spec of the current entity. The current entity is
  determined by the word located around the cursor.  This item is also
  accessible through the editor's contextual menu directly.  This capability
  requires the availability of cross-reference information.
  :ref:`Support_for_Cross-References`.

*Goto Body*
  .. index:: goto body

  Go to the body/implementation of the current entity. If the current entity is
  the declaration of an Ada subprogram imported from C it goes to the location
  where the C function is defined.  This item is also accessible through the
  editor's contextual menu directly.  This capability requires the availability
  of cross-reference information.  :ref:`Support_for_Cross-References`.

*Goto Matching Delimiter*
  .. index:: goto matching delimiter

  Go to the delimiter matching the one right before (for a closing delimiter) or
  right after (for an opening delimiter) the cursor if any.

*Goto Line...*
  .. index:: goto line

  Open a dialog where you can type a line number,  in order to jump to a
  specific location in the current source editor.

*Goto Entity...*
  .. index:: goto entity

  Open a dialog allowing browsing of the entities loaded in the project.  This
  dialog functions similarly to :ref:`The_Entity_View`.

*Goto File Spec<->Body*
  .. index:: goto file spec/body
  .. index:: Ada

  Open the corresponding spec file if the current edited file is a body file,
  or body file otherwise. This option is only available for the Ada language.
  This item is also accessible through the editor's contextual menu

  This capability requires support for cross-references.  This item is also
  accessible through the editor's contextual menu

*Start Of Statement*
  .. index:: Start Of Statement

  Move the cursor position to the start of the current statement, move to the
  start of the enclosing statement if the cursor position is already at the
  start of the statement.


*End Of Statement*
  .. index:: End Of Statement

  Move the current cursor position to the end of the statement, move to the end
  of the enclosing statement if the cursor position is already at the end of
  the statement.

*Previous Subprogram*
  .. index:: Previous Subprogram

  Move the current cursor position to the start of the previous procedure,
  function, task, protected record or entry.

*Next Subprogram*
  .. index:: Next Subprogram

  Move the current cursor position to the start of the next procedure,
  function, task, protected record or entry.

*Previous Tag*
  .. index:: tag
  .. index:: previous tag
  .. index:: locations view

  Go to previous tag/location. :ref:`The_Locations_View`.

*Next Tag*
  .. index:: tag
  .. index:: next tag
  .. index:: locations view

  Go to next tag/location. :ref:`The_Locations_View`.

*Back*
  .. index:: Back

  Go to previous location.

*Forward*
  .. index:: Forward

  Go to next location.

.. _Contextual_Menus_for_Source_Navigation:

Contextual Menus for Source Navigation
======================================

.. index:: contextual menu

This contextual menu is available from any source editor.  If you right click
over an entity, or first select text, the contextual menu will apply to this
selection or entity.

*Goto declaration of *entity**
  .. index:: goto declaration

  Go to the declaration/spec of *entity*. The current entity is determined by
  the word located around the cursor or by the current selection if any.  This
  capability requires support for cross-references.

*Goto full declaration of *entity**
  .. index:: goto declaration

  This contextual menu appears for a private or limited private types. Go to
  the full declaration/spec of *entity*. The current entity is determined by
  the word located around the cursor or by the current selection if any.  This
  capability requires support for cross-references.

*Goto type declaration of *entity**
  .. index:: goto type declaration

  Go to the type declaration of *entity*. The current entity is determined by
  the word located around the cursor or by the current selection if any.  This
  capability requires support for cross-references.

*Display type hierarchy for *entity**
  .. index:: display type hierarchy

  This contextual menu appears for derived or access types. Output the type
  hierarchy for *entity* into the location view. The current entity is
  determined by the word located around the cursor or by the current selection
  if any.  This capability requires support for cross-references.

*Goto body of *entity**
  .. index:: goto body

  Go to the body/implementation of *entity*. If *entity* is the declaration of
  an Ada subprogram imported from C it goes to the the location where the C
  function is defined.  This capability requires support for cross-references.

*Goto declarations of *entity**
  .. index:: goto declaration

  This contextual menu appears when you are clicking on a subprogram call that
  is a dispatching call. In such a case, there is no possibility for GPS to
  know what subprogram will actually be called at run time, since that depends
  on dynamic information. It therefore gives you a list of all entities in the
  tagged type hierarchy, and lets you choose which of the declarations you want
  to jump to. See also the :file:`methods.py` plug-in (enabled by default)
  which, given an object, lists all its primitive operations in a contextual
  menu so that you can easily jump to them. See also the contextual menu
  :file:`References/Find References To...` which allows you to find all calls
  to a subprogram or to one of its overriding subprograms.

*Goto bodies of *entity**
  .. index:: goto body

  This is similar to *Goto declarations of*, but applies to the bodies of the
  entities.

*Goto file spec/body*
  .. index:: goto file spec/body
  .. index:: Ada

  Open the corresponding spec file if the current edited file is a body file,
  or body file otherwise. This option is only available for the Ada language.

*Entity* calls
  Display a list of all subprograms called by *entity* in a tree view. This is
  generally more convenient than using the corresponding Browsers/ submenu if
  you expect lots of references, :ref:`The_Callgraph_View`.

*Entity* is called by
  Display a list of all subprograms calling *entity* in a tree view. This is
  generally more convenient than using the correponding Browsers/ submenu if
  you expect lots of references, :ref:`The_Callgraph_View`.

References
  .. index:: references

  This item gives access to different capabilities related to listing or
  displaying references to the current entity or selection.

*Find all references to *entity**
    .. index:: find all references

    :ref:`Find all references <Find_All_References>` to *entity* in all the
    files in the project.

*Find all references...*
    This menu is similar to the one above, except it is possible to select more
    precisely what kind of reference should be selected. It is also possible to
    indicate the scope of the search, and whether the context (or caller) at
    each reference should be displayed. Computing the caller information will
    take slightly longer though.

    .. index:: primitive operation

    This dialog has an option `Include overriding and overriden operations`,
    which, when activated, will include references to overriden or overriding
    entities of the one you selected.

    This is particularly useful when you are wondering whether you can easily
    modify the profile of a primitive operation, since you can then see what
    other entities will also be impacted. If you select only the `declaration`
    check box, you will see the list of all related primitive operations.

    .. index:: imported entities

    This dialog also allows you to find out which entities are imported from a
    given file/unit. Click on any entity from that file (for instance on the
    `with` line for Ada code), then select the `All entities imported from same
    file` toggle button. This will display in the location window the list of
    all entities imported from the same file as the entity selected.

    In addition, if you have selected the `Show context` option, you will get a
    list of all the exact references to these entities within the file.
    Otherwise, you just get a pointer to the declaration of the imported
    entities.

*Find all local references to *entity**
    .. index:: find all local references

    :ref:`Find all references <Find_All_References>` to *entity* in the current
    file (or in the current top level unit for Ada sources).  details.

*Variables used in *entity**
    .. index:: variables used

    Find all variables (local or global) used in *entity* and list each first
    reference in the locations window.

*Non Local variables used in *entity**
    Find all non-local variables used in the entity.

*Methods of *entity**
  .. index:: methods
  .. index:: primitive operations

  This submenu is only visible if you have activated the plug-in
  :file:`methods.py` (which is the case by default), and when you click on a
  tagged type or an instance of a tagged type. This menu lists all the
  primitive operations of that type, and you can therefore easily jump to the
  declaration of any of these operations.

*Browsers*
  .. index:: browsers

  This item gives access to graph representations of callers and callees for
  subprograms.

  *Entity* calls
    .. index:: call graph
    .. index:: calls

    Open or raise the call graph browser on the specified entity and display
    all the subprograms called by *entity*. :ref:`Call_Graph`.

  *Entity* calls (recursively)
    .. index:: call graph

    .. index:: calls

    Open or raise the call graph browser on the specified entity and display
    all the subprograms called by *entity*, transitively for all subprograms.
    Since this can take a long time to compute and generate a very large graph,
    an intermediate dialog is displayed to limit the number of subprograms to
    display (1000 by default). :ref:`Call_Graph`.

  *Entity* is called by
    .. index:: call graph
    .. index:: called by

    Open or raise the call graph browser on the specified entity and display
    all the subprograms calling *entity*. :ref:`Call_Graph`.

    Note that this capability requires a global look up in the project
    cross-references, which may take a significant amount of time the first
    time.  After a global look up, information is cached in memory, so that
    further global queries will be faster.

Expanded code
  Present for Ada files only. This menu generates a .dg file using your gnat
  compiler (using the -gnatGL switch) and displays the expanded code. This can
  be useful when investigating low-level issues and tracing precisely how the
  source code is transformed by the GNAT front-end.

  *Show subprogram*
    Display expanded code for the current subprogram in the current editor.

  *Show file*
    Display expanded code for the current file in the current editor.

  *Show in separate editor*
    Display expanded code for the current file in a new editor.

  *Clear*
    Remove expanded code from the current editor.

  For Ada files only, this entry will generate, and will open this file
  at the location corresponding to the current source line.

*Open <filename>*
  When you click on a filename (for instance a C' `#include`, or an error
  message in a log file), this menu gives you a way to open the corresponding
  file. If the file name was followed by ":" and a line number, the
  corresponding line is activated.

.. _Navigating_with_hyperlinks:

Navigating with hyperlinks
==========================

.. index:: hyperlinks

When the Control key is pressed and you start moving the mouse, entities in the
editors under the mouse cursor become hyperlinks and the mouse cursor aspect
changes.

Left-clicking on a reference to an entity will open a source editor on the
declaration of this entity, and left-clicking on an entity declaration will
open an editor on the implementation of this entity.

Left-clicking on the Ada declaration of a subprogram imported from C will open
a source editor on the definition of the corresponding C entity. This
capability requires support for cross-references.

Clicking with the middle button on either a reference to an entity or the
declaration of an entity will jump directly to the implementation or type
completion) of this entity.

Note that for efficiency, GPS may create hyperlinks for some entities which
have no associated cross reference. In this case, clicking will have no effect,
even though an hyperlink may have been displayed.

This behavior is controlled by the `Hyper links` preference.

.. _Highlighting_dispatching_calls:

Highlighting dispatching calls
==============================

.. index:: dispatching

Dispatching calls in Ada and C++ source code are highlighted by default in GPS
via the `dispatching.py` plug-in.

Based on the cross-reference information, this plug-in will highlight (with a
special color that you can configure in the preferences dialog) all calls that
are dispatching (or calls to virtual methods in C++).  A dispatching call, in
Ada, is a subprogram call where the actual subprogram that is called is not
known until run time, and is chosen based on the tag of the object (so this of
course only exists when you are using object-oriented programming).

To disable this highlighting (which might sometimes be slow if you are using
big sources, even though the highlighting itself is done in the background),
you can go to the `/Tools/Plug-ins` menu, and disable the `dispatching.py`
plug-in.
