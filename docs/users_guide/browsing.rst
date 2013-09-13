.. _Source_Browsing:

***************
Source Browsing
***************

.. index:: source browsing

Dependency Browser
==================

.. index:: dependency browser

The dependency browser shows the dependencies between source files. Each item
in the browser represents one source file.

.. index:: screen shot
.. image:: dependency-browser.jpg

In this browser, clicking on the right arrow in the title bar will display the
list of files that the selected file depends on. A file depend on another one
if it explicitly imports it (`with` statement in Ada, or `#include` in C/C++).
Implicit dependencies are currently not displayed in this browser, since the
information is accessible by opening the other direct dependencies.

Clicking on the left arrow in the title bar will display the list of files that
depend on the selected file.

This browser is accessible through the contextual menu in the project view and
the source editor, by selecting one of the following items:

*Show dependencies for *file**
  .. index:: show dependencies for

  This has the same effect as clicking on the right arrow for a file already in
  the browser, and will display the direct dependencies for that file.

*Show files depending on *file**
  .. index:: show files depending on

  This has the same effect as clicking on the left arrow for a file already in
  the browser, and will display the list of files that directly depend on that
  file.

The background contextual menu in the browser adds a few entries to the
standard menu:

*Open file...*

  This menu entry will display an external dialog in which you can select the
  name of a file to analyze.

*Recompute dependencies*

  .. index:: recompute dependencies

  This menu entry will check that all links displays in the dependency browser
  are still valid. If not, they are removed. The arrows in the title bar are
  also reset if necessary, in case new dependencies were added for the files.

  The browser is not refreshed automatically, since there are lots of cases
  where the dependencies might change (editing source files, changing the
  project hierarchy or the value of the scenario variables, ...)

  It also recomputes the layout of the graph, and will change the current
  position of the boxes.

*Show system files*
  .. index:: show system files

  This menu entry indicates whether standard system files (runtime files for
  instance in the case of Ada) are displayed in the browser. By default, these
  files will only be displayed if you explicitly select them through the `Open
  file` menu, or the contextual menu in the project view.

*Show implicit dependencies*
  .. index:: show implicit dependencies

  This menu entry indicates whether implicit dependencies should also be
  displayed for the files. Implicit dependencies are files that are required to
  compile the selected file, but that are not explicitly imported through a
  `with` or `#include` statement. For instance, the body of generics in Ada is
  an implicit dependency.  Any time one of the implicit dependencies is
  modified, the selected file should be recompiled as well.

The contextual menu available by right clicking on an item also adds a
number of entries:

*Analyze other file*
  .. index:: analyze other file

  This will open a new item in the browser, displaying the complement file for
  the selected one. In Ada, this would be the body if you clicked on a spec
  file, or the opposite. In C, it depends on the naming conventions you
  specified in the project properties, but you would generally go from a
  :file:`.h` file to a :file:`.c` file and back.

*Show dependencies for *file**
  .. index:: show files depending on file

  These play the same role as in the project view contextual menu

.. _Elaboration_Cycles_Browser:

Elaboration Cycles Browser
==========================

GPS can detect elaboration cycles reported by build processes, and
construct a visual representation of elaboration dependencies, in an 
Elaboration Cycles Browser.

This visual representation represents program units as items in the browsers,
and direct dependencies between program units as links.
All units involved in a dependency cycle caused by the presence of a
pragma Elaborate_All (whether explicit or implicit) are also presented
in the browser and connected by links with labels "body" and "with".

The preference `Browsers/Show elaboration cycles` controls whether to
automatically create a graph from cycles listed in build output.

.. index:: screen shot
.. image:: elaboration-graph.jpg

.. _Entity_Browser:

Entity Browser
==============

.. index:: entity browser

The entity browser displays static information about any source entity.

The exact content of the items depend on the type of the item. For instance:

*Ada record / C struct*

  The list of fields, each as an hyper link, is displayed. Clicking on
  one of the fields will open a new item for the type.

*Ada tagged type / C++ class*

  The list of attributes and methods is displayed. They are also
  click-able hyper-links.

*Subprograms*

  The list of parameters is displayed

*Packages*

  The list of all the entities declared in that package is displayed

*and more...*

.. index:: screen shot
.. image:: entity-browser.jpg

This browser is accessible through the contextual menu in the project view and
source editor, when clicking on an entity:

*Browsers/Examine entity *entity**
  .. index:: examine entity

  Open a new item in the entity browser that displays information for the
  selected entity.

Most information in the items are click-able (by default, they appear as
underlined blue text). Clicking on one of these hyper links will open a new
item in the entity browser for the selected entity.

This browser can display the parent entities for an item. For instance, for a
C++ class or Ada tagged type, this would be the types it derives from. This is
accessible by clicking on the up arrow in the title bar of the item.

Likewise, children entities (for instance types that derive from the item) can
be displayed by clicking on the down arrow in the title bar.

An extra button appear in the title bar for the C++ class or Ada tagged types,
which toggles whether the inherited methods (or primitive operations in Ada)
should be displayed. By default, only the new methods, or the ones that
override an inherited one, are displayed. The parent's methods are not shown,
unless you click on this title bar button.

