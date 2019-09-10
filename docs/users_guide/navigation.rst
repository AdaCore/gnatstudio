.. highlight:: ada
.. _Source_Navigation:

*****************
Source Navigation
*****************

.. index:: source navigation
.. index:: navigation
.. index:: cross-references
.. _Support_for_Cross-References:

Support for Cross-References
============================

GNAT Studio provides cross-reference navigation for program entities defined in
your application such as types, procedures, functions, and variables.


.. index:: hyperlinks
.. _Navigating_with_hyperlinks:

Navigating with hyperlinks
==========================

When you press the :kbd:`Control` key and start moving the mouse, entities
in the editors under the pointer become hyperlinks and the form of the
pointer changes.

Left-clicking on a reference to an entity opens a source editor on the
declaration of the entity and left-clicking on an entity declaration opens
an editor on the implementation of the entity.  Left-clicking on the Ada
declaration of a subprogram imported from C opens a source editor on the
definition of the corresponding C entity. This capability requires support
for cross-references.

For efficiency, GNAT Studio may create hyperlinks for some entities which have
no associated cross reference. In this case, clicking has no effect even
though a hyperlink is displayed.

.. index:: preferences; general --> hyper links

This behavior is controlled by the :menuselection:`General --> Hyper links`
preference.


.. index:: dispatching
.. index:: plugins; dispatching.py
.. _Highlighting_dispatching_calls:

Highlighting dispatching calls
==============================

By default, GNAT Studio highlights dispatching calls in Ada and C++ source
code via the :file:`dispatching.py` plugin.  Based on the cross-reference
information, this plugin highlights (with a special color you can
configure in the preferences dialog) all Ada dispatching calls or calls to
virtual methods in C++.  A dispatching call in Ada is a subprogram call
where the actual subprogram called is not known until run time and is
chosen based on the tag of the object.

Disable this highlighting (which may be slow if you have large sources) by
using the :menuselection:`Edit --> Preferences...` menu, going to the Plugins
section and disabling the :file:`dispatching.py` plugin.

Contextual Menus for Source Navigation
======================================

This contextual menu is available from any source editor.  If you
right-click on an entity or selected text, the contextual menu applies to
the selection or entity. Most of these menus requires support for
cross-references. Here are the main ones regarding navigation.

* :menuselection:`Jump to Specification/Implementation File`

  Open the corresponding spec file if the current edited file is a body
  file, or the body file otherwise. This entry is only available for the
  Ada language.

* :menuselection:`Go To Declaration`

  Go to the declaration (spec) of the clicked entity.

.. index:: plugins; methods.py

* :menuselection:`Go To Body or Full Declaration`

  This entry appears for a private on subprograms or limited private types.
  Go to body or the full declaration (spec) of the clicked entity.

* :menuselection:`Go To Type Declaration`

  Go to the type declaration of the clicked entity.

* :menuselection:`Find All References`

  Finds all the references to the clicked entity.

  .. index:: primitive operations
  .. index:: overriding operations
  .. index:: methods

  The option :guilabel:`Include overriding and overridden operations`
  includes references to overridden or overriding entities.  This is
  particularly useful if you need to know whether you can easily modify
  the profile of a primitive operation or method since you can see which
  other entities would also be changed. If you select only the
  :guilabel:`declaration` check box, you see the list of all related
  primitive operations.

  .. index:: imported entities

  This dialog allows you to determine which entities are imported from a
  given file or unit. Click on any entity from that file (for example on
  the :command:`with` line for Ada code) and select the :guilabel:`All
  entities imported from same file` toggle, which displays in the
  :guilabel:`Location` view the list of all entities imported from the
  same file.

  Selecting the :guilabel:`Show context` option produces a list of all
  the references to these entities within the file.  If it is not
  selected, you just get a pointer to the declaration of the imported
  entities.

* :menuselection:`Call Trees --> *Entity* is called by`

  Display a list of all subprograms calling the clicked entity in a tree view.


The gnatinspect cross-reference database (deprecated)
=====================================================

Previous versions of GNAT Studio relied on the compiler for navigation
information; this section covers specifics for this engine, which is now
deprecated.

The cross-reference database
----------------------------

GNAT Studio parses the cross-reference information generated by the compiler
(the :file:`.ali`) files into one or several :program:`sqlite` databases (e.g:
if your project uses Ada and C). These database files can become quite large
and should preferably be on a fast local disk.

By default, GNAT Studio places these database files in the object directory of
the currently-loaded root project, or, if specified, in the directory
designated by the relative or absolute path given by the
:guilabel:`Artifacts_Dir` attribute of the :guilabel:`IDE` package of your
project file::

   --  assume this is in /home/user1/work/default.gpr
   project Default is
      for Object_Dir use "obj";

      package IDE is
         for Artifacts_Dir use "artifacts";
         --  All the artifacts generated by GNAT Studio
         --  (including the xref databases)
         --  will be put in the /home/user1/work/artifacts/ directory.
         --
         --  We could also have specified an absolute path here
         --  (e.g: for Artifacts_Dir use "/home/user1/work/artifacts/").
      end IDE;
   end Default;

If you want to have more advanced control regarding the naming of the Ada
cross-references database file, you can use the :guilabel:`Xref_Database` in
the :guilabel:`IDE` package of your project file, either as an absolute path or
a path relative to the location of the project file. We recommend this path to
be specific to each use, and to each project this user might be working on, as
in the following examples::

   --  assume this is in /home/user1/work/default.gpr
   project Default is
      for Object_Dir use "obj";

      package IDE is
         for Xref_Database use "xref_database.db";
         --  This would be /home/user1/work/xref_database.db

         for Xref_Database use Project'Object_Dir & "/xref_database.db";
         --  This would be /home/user1/work/obj/xref_database.db
         --  This is the default when this attribute is not specified

         for Xref_Database use external("HOME") & "/prj1/database.db";
         --  This would be /home/user1/prj1/database.db
      end IDE;
   end Default;

One drawback in altering the default location is that :program:`gprclean`
will not remove these database files when you clean your project.  But it might
speed up GNAT Studio if your project is not on a fast local disk and you can
put the databases there.

**WARNING**: You should not store this file in a directory that is accessed via
a network filesystem, like NFS, or Clearcase's MVFS. If your obj directory is
on such a filesystem, be sure to specify a local directory for IDE'Artifacts_Dir
project attribute or, if you project only uses Ada, a custom local file path for
the IDE'Xref_Database project attribute.


Cross-references and partially compiled projects
------------------------------------------------

The cross-reference engine works best when the cross-reference
information generated by the compiler (the :file:`.ali` files) is
fully up to date.

If you start from such a state and then modify the spec or body of an Ada
package and recompile only that file, any reference to entities declared in
that spec in other packages might no longer be found (until you recompile
those other packages, as :program:`gprbuild` would).

This is because GNAT Studio has no way to know for sure whether an entity
:samp:`Foo` in the spec is the same entity as before or is a new one with
the same name. It uses an approximate algorithm where the references are
only preserved if an entity with the same name remains at precisely the
same location in the new version of the source. But if a blank line in the
file will change the declaration line for all entities declared further in
the file, so those will lose their references from other source files.

.. index:: cross-references; runtime files

Cross-reference and GNAT runtime
--------------------------------

By default, GNAT Studio does not parse the GNAT runtime files because there is
a large number of them and doing so would significantly slow down GNAT Studio,
while producing only a minimal gain for most users.  However, the location of
subprograms in those runtime files is available from the :file:`.ali` files
corresponding to the sources of your project.

From your own sources, you can navigate to one of the runtime files (for
example, if you have a reference to :func:`Put_Line`, you will jump to its
declaration in :file:`a-textio.ads`). But you cannot perform
cross-reference queries from a runtime file itself.

If you need this capability, enable the preference :guilabel:`Project/Cross
References in Runtime Files`.
