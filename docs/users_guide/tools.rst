.. _Tools:

*****
Tools
*****

.. index:: tools

.. _The_Tools_Menu:

The Tools Menu
==============

The `Tools` menu gives access to additional tools.  If an option is
disabled, it means it's a planned tool that's not yet available.

The list of active items includes:

*Views*


  *Bookmarks*
    .. index:: bookmark

    :ref:`Bookmarks`.

  *Call Trees*
    Open a tree view of displaying function callers and callees. See also
    :ref:`Call_Graph`.

  *Clipboard*
    :ref:`The_Clipboard_View`.

  *Coverage Report*
    :ref:`Coverage Report <Coverage_Report>`.

  *Files*
    Open a file system explorer in the left area.

    :ref:`The_File_View`.

  *File Switches*
    :ref:`File Switches <File_Switches>`.

  *Outline*
    Open a view of the current source editor.
    :ref:`The_Outline_View`.

  *Messages*
    Open the :guilabel:`Messages` view.
    :ref:`The_Messages_View`.

  *Project*
    :ref:`The_Project_View`.

  *Remote*
    :ref:`Setup_a_remote_project`.

  *Scenario*
    :ref:`Scenarios_and_Configuration_Variables`.

  *Tasks*
    :ref:`The_Task_Manager`.

  *VCS Activities*
    :ref:`The_VCS_Activities`.

  *VCS Explorer*
    :ref:`The_VCS_Explorer`.

  *Windows*
    Open a view containing all currently opened files.
    :ref:`The_Window_View`.

*Browsers*

  *Call Graph*
    :ref:`Call_Graph`.

  *Dependency*
    :ref:`The_Dependency_Browser`.

  *Elaboration Cycles*
    :ref:`Elaboration_Cycles_Browser`.

  *Entity*
    :ref:`Entity_Browser`.

*Coding Standard*
  .. index:: Coding Standard

  :ref:`Coding_Standard`.

*Compare*
  .. index:: visual diff

  :ref:`Visual_Comparison`.

*Consoles*

  *GPS Shell*
    .. index:: shell

    Open a shell console in the bottom area of the GPS window.  This not an
    OS shell console, but a GPS shell console, where you can type GPS
    specific commands such as `help`.

    :ref:`The_Shell_and_Python_Consoles`.

  *Python*
    .. index:: python

    Open a python console to access the python interpreter.
    :ref:`The_Shell_and_Python_Consoles`.

  *OS Shell*
    .. index:: shell

    Open an OS (Windows or Unix) console, using the environment variables
    `SHELL` and `COMSPEC` to determine which shell to use.
    :ref:`The_Shell_and_Python_Consoles`.

    On Unix, this terminal behaves a lot like a standard Unix terminal.
    You need to make sure your shell will output all necessary
    information. In some cases, the configuration of your shell
    (:file:`.bashrc` if you are running bash for instance) deactivates the
    echo of what you type. Since GPS is not writing anything on its own,
    but just showing what the shell is outputing, you need to ensure that
    your shell always echos what you type.  You can do this by running the
    command::

      stty echo
      
    in such cases.  You can normally safely put this in your
    :file:`.bashrc`

  *Auxiliary Builds* Open the console containing auxiliary buils
    output. Only cross-reference automated generation output is currently
    sent to this console.  :ref:`Working_with_two_compilers`.

*Coverage*
  .. index:: code coverage

  :ref:`Code_Coverage`.

*Documentation*
  .. index:: documentation

  :ref:`Documentation_Generation`.

*GNATtest*
  .. index:: gnattest

  :ref:`Working_With_Unit_Tests`.

*Stack Analysis*
  .. index:: stack analysis

  :ref:`Stack_Analysis`.

*Macro*
  .. index:: macros

  :ref:`Recording_and_replaying_macros`.

*Metrics*
  .. index:: metrics

  :ref:`Metrics`.

*Plug-ins*
  .. index:: plug-ins

  :ref:`The_Plug-ins_Editor`.

*Interrupt*
  .. index:: interrupt

  Interrupt the last task launched such as a compilation or vcs operation.

.. _Coding_Standard:

Coding Standard
===============

.. index:: coding standard

Use the Coding Standard menu to edit your coding standard file and run it
against your code, to verifiy its compliance with this coding standard.
This file is the input to the `gnatcheck` tool.  You can also use the
contextual menu to check the conformance of a particular project or source
file against a Coding Standard.

Access the Coding standard editor using the :menuselection:`Tools->Coding
Standard->Edit Rules File` menu.  You can select an existing coding
standard file or create a new one. The editor adapts itself to the version
of `gnatcheck` on your local machine.

The rules currently in use are summarized in the bottom of the editor. Once
all rules are defined, check the box 'Open rules file after exit' to
manually verify the created file.  Once you've created the Coding Standard
file, set it as the default coding standard file for a project by going to
the project editor, selecting the 'Switches' tab, and specifying this file
in the 'Gnatcheck' section.

.. _Visual_Comparison:

Visual Comparison
=================

.. index:: visual diff

The visual comparison, available either from the VCS menus or from the
Tools menu, provides a way to graphically display differences between two
or three files or two different versions of the same file.

The 2-file comparison tool uses the standard command `diff`, available on
all Unix systems. Under Windows, a default implementation is provided with
GPS, called :file:`gnudiff.exe`.  You may want to provide an alternate
implementation, for example by installing a set of Unix tools such as
Cygwin (`http://www.cygwin.com <http://www.cygwin.com>`_).  The 3-file
comparison tool is based on the text command `diff3`, available on all Unix
systems. Under Windows, this tool is not provided with GPS, but is
available as part of Cygwin.

GPS displays visual comparisons in either Side-by-Side or Unified mode.  In
side-by-side mode, the user area displays editors for the files involved in
the comparison side by side.  By default, GPS places the reference file on
the left. In Unified mode, GPS doesn't open a new editor, but shows all the
changes in the original editor.  Unified mode is used only when comparing
two files; when comparing three files, only side-by-side mode is available.

Lines in the file editors are highlighted with various colors.  In
side-by-side mode, only the right editor (for the modified file) has
different colors.

*gray*
 Used for all the chunks on the reference (left) file.

*yellow*
  Used to display lines modified from the reference file. Small differences
  within one line are shown in a brighter yellow.

*green*
  Used to display lines not originally in the reference file but added to
  the modified file.

*red*
  Used to display lines present in the reference file but deleted from the
  modified file.

You can configure these colors. :ref:`The_Preferences_Dialog`.

Like all highlighted lines in GPS, the visual differences highlights are
visible in the Speed Column at the left of the editors.

GPS adds blank lines in one editor in places corresponding to existing
lines in the other editors.  Vertical and horizontal scrolling are
synchronized between the editors involved in a visual comparison.  If you
close one of the editors involved in a visual comparison, GPS removes the
highlighting, blank lines, and scrolling in the other editors.

When you create a visual comparison, GPS populates the
:guilabel:`Locations` view with the entries for each chunk of differences.
You can use those to navigate between the differences.

Editors involved in a visual comparison have a contextual menu `Visual
diff` containing the following entries:

*Recompute*
  Regenerate the visual comparison.  Use this when one of the files in an
  editor has been modified by hand while involved in a visual comparison.

*Hide*
  Remove the highlighting corresponding to the visual comparison from all
  involved editors.

*Close editors*
  Closes all editors involved in this visual comparison

*Use this editor as reference*
  Make this editor the reference. (This is only visible when displaying a
  visual comparison involving 3 files).

.. index:: screen shot
.. image:: visual-diff.jpg

.. _Code_Fixing:

Code Fixing
===========

.. index:: code fixing
.. index:: wrench icon

GPS provides an interactive way to correct or improve your source code
based on messages (errors and warnings) generated by the GNAT compiler.
This capability is integrated with the :guilabel:`Locations` view (see
:ref:`The_Locations_View`): when GPS can make use of a compiler message, it
adds an icon to the left side of the line.

If a wrench icon is displayed and you left-click on it, the code is fixed
automatically and you'll see the change in the corresponding source editor.
This occurs when a simple fix, such as the addition of a missing semicolon,
is sufficient to resolve the error.

Right-click on the icon to display a contextual menu with a text explaining
the action that would be on a left-click.  Displaying a contextual menu
anywhere else on the message line provides an option called 'Auto Fix' that
gives you access to the same information. For the previous example of a
missing semicolon, the menu contains an entry labelled 'Add expected string
";"'.  You can choose to 'Apply to this occurrence' or 'Apply to all
similar errors'.  The latter choice applies the same simple fix to all
errors which are the same, based on parsing the error message.  The wrench
icon is no longer displayed once the code change has been made.

For more complex error, where more than one change is possible, GPS
displays a wrench icon with a blue plus sign.  Clicking on the icon
displays the contextual menu listing the possible fixes. For example, this
is displayed when an ambiguity in resolving an entity is reported by the
compiler.

Right-clicking on a message with a fix opens a contextual menu with an
entry "Auto Fix". Fixes that can be applied by clicking on the wrench are
also available through that menu. In addiditon, if GPS considers one of the
fixes to be safe, additional options are provided to apply fixes on
multiple messages:

*Fix all simple style errors and warnings*
  Offered only when the selected message is a warning and a style error.
  Fixes all other warnings and style errors for which a unique simple fix is
  available.

*Fix all simple errors*
  Fixes all errors messages for which a unique simple fix is available

.. _Documentation_Generation:

Documentation Generation
========================

.. index:: documentation generation

GPS provides a documentation generator that processes source files and
generates annotated HTML files.

This generator uses the source cross-reference information (for example,
that's generated by the GNAT compiler for Ada files) so you must ensure
cross-reference information has been generated before generating
documentation.  It relies on standard comments that it extracts from the
source code, but, unlike other similar tools, you don't need to put any
special token or macro in your comments. The engine in charge of extracting
the comments, coupled with the cross-reference engine, gives GPS all the
information it needs to generate accurate documentation.

.. highlight:: ada

By default, GPS puts the documentation into a directory called :file:`doc`,
created under the object directory of the root project loaded in GPS. If no
object directory exists, it's created in the same directory as the root
project. This behavior can be modified by specifying the attribute
Documentation_Dir in the package IDE of your root project::

  project P is
     package IDE is
        for Documentation_Dir use "html";
     end IDE;
  end P;

Once the documentation is generated, the main documentation file is loaded in
your default browser.

The documentation generator uses a set of templates files to control the
final rendering, so can precisely control the formatting of the generated
documentation. The templates used for generating the documentation are
found in :file:`<install_dir>/share/gps/docgen2`.  You can change those
files if you need a different layout than the default.

You can also use user-defined structured comments to improve the generated
documentation. These use xml-like tags. To define your own set of tags,
refer to the GPS python extension documentation (from GPS Help menu, choose
'Python extensions').  The string values inside those tags are handled
similar to regular XML: duplicated spaces and line returns are ignored. One
exception is that the layout is preserved in the following cases:

*The line starts with "- " or "* "*
  GPS makes sure a proper line return precedes the line. This allows
  lists in comments.

*The line starts with a capital letter*
  GPS keeps the preceding line return.

GPS defines some default tags in
:file:`<install_dir>/share/gps/plug-ins/docgen_base_tags.py`. The tags
handled are:

*summary*
  Short summary of what a package or method is doing.

*description*
  Full description of what a package or method is doing.

*parameter (attribute "name" is expected)*
  Description of the parameter named "name".

*exception*
  Description of possible exceptions raised by the method.

*seealso*
  Reference to another object, such as package, method, or type.

*c_version*
  For bindings, the version of the C file.

*group*
  For packages, this builds an index of all packages in the project grouped by
  categories.

*code*
  When the layout of the value of the node needs to be preserved. The text is
  displayed using a fixed font (monospace).

The following sample shows how those tags are translated::

  --  <description>
  --    This is the main description for this package. It can contain a complete
  --    description with some xml characters as < or >.
  --  </description>
  --  <group>Group1</group>
  --  <c_version>1.0.0</c_version>
  package Pkg is

     procedure Test (Param : Integer);
     --  <summary>Test procedure with a single parameter</summary>
     --  <parameter name="Param">An Integer</parameter>
     --  <exception>No exception</exception>
     --  <seealso>Test2</seealso>

     procedure Test2 (Param1 : Integer; Param2 : Natural);
     --  <summary>Test procedure with two parameters</summary>
     --  <parameter name="Param1">An Integer</parameter>
     --  <parameter name="Param2">A Natural</parameter>
     --  <exception>System.Assertions.Assert_Failure if Param1 < 0</exception>
     --  <seealso>Test</seealso>

  end Pkg;
  
Its documentation will be:

.. index:: screen shot
.. image:: docgen.jpg

You can invoke the documentation generator from the
:guimenu:`Tools->Documentation` menu:

*Generate project*
  Generate documentation for all files in the loaded project.

*Generate projects & subprojects*
  Generate documentation for all files in the loaded project and its
  subprojects.

*Generate current file*
  Generate documentation for the file currently being edited.

*Generate for...*
  Open a File Selector Dialog (:ref:`The_File_Selector`) and generate
  documentation for the specified file.

In addition, when relevant (depending on the context), right-clicking
displays a `Documentation` contextual menu.

You can select the option called 'Generate for <filename>' from a source
file contextual menu that generate documentations for that file, and if it
exists, its corresponding body (:ref:`The_Preferences_Dialog`).  From a
project contextual menu (:ref:`The_Project_View`), you have the choice
between generating documentation for all files in the selected project only
or from the selected project and all subprojects.  Find the list of all
documentation options in :ref:`The_Preferences_Dialog`.

The documentation generator relies on the ALI files created by GNAT.
Depending on the version of GNAT used, the following restrictions may or
may not apply:

* A type named *type* may be generated in the type index.

* Parameters and objects of private generic types may be considered to be
  types.

.. _Working_With_Unit_Tests:

Working With Unit Tests
=======================

GPS uses `gnattest`, a tool that creates unit-test stubs as well as a test
driver infrastructure (harness).  It can generate harnesses for a project
hierarchy, a single project or a package.  You can launch harness
generation process from the :guimenu:`Tools->GNATtest` menu or a contextual
menu.

After a harness project has been generated, GPS switches to it, allowing
you to implement tests, compile and run the harness.  You can
exit the harness project and return to original project at any point.

The GNATtest Menu
-----------------

The `GNATtest` submenu is found in the :guimenu:`Tools` global menu and
contains the following options:

*Generate unit test setup*
  Generate harness for the root project.

*Generate unit test setup recursive*
  Generate harness for the root project and subprojects.

*Show not implemented tests*
  Find tests that have have never been modified and list them in the
  :guilabel:`Locations` view. This menu is active in harness project only.

*Exit from harness project*
  Return from harness to original project.

The Contextual Menu
-------------------

When relevant to the context, right-clicking displays GNATtest-related
contextual menu entries.  From a source file containing a library
package declaration, there's an option called "GNATtest/Generate unit
test setup for <file>" that generates the harness for this single
package.  From a project contextual menu (:ref:`The_Project_View`),
there's an option "GNATtest/Generate unit test setup for <project>"
that generates the harness for the entire project.  The
"GNATtest/Generate unit test setup for <project> recursive" option
generates a harness for whole hierarchy of projects. If a harness
project already exists, the "GNATtest/Open harness project" option
switches GPS to the harness project.

While a harness project is opened, you can easily navigate between the
tested routine and its test code.  Clicking on the name of a tested routine
produces the options "GNATtest/Go to test case", "GNATtest/Go to test
setup", and "GNATtest/Go to test teardown".  The contextual menu for source
files of test cases or setup and teardown code has an option called
"GNATtest/Go to <routine>" to go to code being tested.

Project Properties
------------------

You configure gnattest's behavior through the GNATtest page in
(:ref:`The_Project_Properties_Editor`).

.. _Metrics:

Metrics
=======

.. index:: Metrics

GPS provides an interface to the GNAT software metrics generation tool
`gnatmetric`.  Metrics can be computed for the one source file, for
the current project, or for the current project and all its imported
subprojects

Invoke the metrics generator from the :guimenu:`Tools->Metrics` menu
or the contextual menu.

The Metrics Menu
----------------

The `Metrics` submenu is available from the :guimenu:`Tools` global menu and
contains:

*Compute metrics for current file*
  Generate metrics for the current source file.

*Compute metrics for current project*
  Generate metrics for all files in the current project.

*Compute metrics for current project and subprojects*
  Generate metrics for all files in the current project and subprojects.

The Contextual Menu
-------------------

When relevant to the context, right-clicking displays metrics-related
contextual menu entries.  A source file contextual menu has an option
"Metrics for file" that generates the metrics for the current file.  A
project contextual menu (:ref:`The_Project_View`) has an option
"Metrics for project" that generates the metrics for all files in the
project.

After computing the requested metrics, GPS displays a new window in the
left-side area showing the computed metrics in a hierarchical tree
form. The metrics are arranged first by files and then by scopes inside the
files.  Double-clicking on any of the files or scopes displayed opens the
appropriate source location in the editor. Any errors encountered during
metrics computation will be displayed in the :gnulabel:`locations` view.

.. _Code_Coverage:

Code Coverage
=============

.. index:: Code Coverage

GPS provides a tight integration with Gcov, the GNU code coverage utility.

Code coverage information can be computed from, loaded and visualized in GPS.
This can be done file by file, for each files of the current project, project
by project (in case of dependencies) or for the entire project hierarchy
currently used in GPS.

Once computed then loaded, the coverage information is summarized in a
graphical report (shaped as a tree-view with percentage bars for each item) and
used to decorate source code through mechanisms such as line highlighting or
coverage annotations.

All the coverage related operations are reachable via the
`Tools->Coverage` menu.

In order to be loaded in GPS, the coverage information need to be computed
before, using the `Tools->Coverage->Gcov->Compute coverage files` menu for
instance.

At each attempt, GPS automatically tries to load the needed information and
reports errors for missing or corrupted :file:`.gcov` files.

To be able to produce coverage information from Gcov, your project must have
been compiled with the *-fprofile-arcs* and *-ftest-coverage*" switches,
respectively "Instrument arcs" and "Code coverage" entries in
:ref:`The_Project_Properties_Editor`, and run once.

Coverage Menu
-------------

The `Tools->Coverage` menu has a number of entries, depending on the
context:

*Gcov->Compute coverage files*
  Generates the *.gcov* files of current and properly compiled and run
  projects.

*Gcov->Remove coverage files*
  Deletes all the *.gcov* of current projects.

*Show report*
  Open a new window summarizing the coverage information currently loaded in
  GPS.

*Load data for all projects*
  Load or re-load the coverage information of every projects and subprojects.

*Load data for project `XXX`*
  Load or re-load the coverage information of the project `XXX`.

*Load data for :file:`xxxxxxxx.xxx`*
  Load or re-load the coverage information of the specified source file.

*Clear coverage from memory*
  Drop every coverage information loaded in GPS.

The Contextual Menu
-------------------

When clicking on a project, file or subprogram entity (including the entities
listed in the coverage report), you have access to a *Coverage* submenu.

This submenu contains the following entries, adapted to the entity selected.
For instance, if you click on a file, you will have:

*Show coverage information*
  Append an annotation column to the left side of the current source editor.
  This column indicates which lines are covered and which aren't. Unexecuted
  lines are also listed in the :ref:`The_Locations_View`.

*Hide coverage information*
  Withdraw from the current source editor a previously set coverage annotation
  column and clear :ref:`The_Locations_View` from the eventually listed
  uncovered lines.

*Load data for :file:`xxxxxxxx.xxx`*
  Load or re-load the coverage information of the specified source file.

*Remove data of :file:`xxxxxxxx.xxx`*
  Remove the coverage information of the specified source file from GPS memory.

*Show Coverage report*
  Open a new window summarizing the coverage information. (This entry appears
  only if the contextual menu has been created from outside of the Coverage
  Report.)

The Coverage Report
-------------------

.. _Coverage_Report:

When coverage information is loaded, a graphical coverage report is displayed.
This report contains a tree of Projects, Files and Subprograms with
corresponding coverage information for each node in sided columns.

.. index:: screen shot
.. image:: report-of-analysis_tree.jpg

The contextual menus generated on this widget contain, in addition to the
regular entries, some specific Coverage Report entries.

These entries allow you to expand or fold the tree, and also to display flat
lists of files or subprograms instead of the tree. A flat list of file will
look like:

.. index:: screen shot
.. image:: report-of-analysis_flat.jpg

GPS and Gcov both support many different programming languages, and so code
coverage features are available in GPS for many languages. But, note that
subprogram coverage details are not available for every supported languages.

Note also that if you change the current main project in GPS, using the
*Project->Open* menu for instance, you will also drop every loaded coverage
information as they are related to the working project.

.. _Stack_Analysis:

Stack Analysis
==============

.. index:: Stack Analysis

GPS provides an interface to `GNATstack`, the static stack analysis tool.  This
interface is enabled only if you have the *gnatstack* executable installed on
your system and available on the path.

Stack usage information can be computed from, loaded and visualized in GPS for
the entire project hierarchy used in GPS. Stack usage information for unknown
and unbounded calls can be edited in GPS.

Once computed and loaded, the stack usage information is summarized in a
report, and used to decorate source code through stack usage annotations. The
largest stack usage path is filled into the :ref:`The_Locations_View`.

Stack usage information for undefined subprograms can be specified by adding a
:file:`.ci` file to the set of GNATStack switches in the `Switches` attribute
of the `Stack` package of your root project, e.g::

  project P is
     package Stack is
        for Switches use ("my.ci");
     end Stack;
  end P;
  

You can also specify this information by using the `GNATStack` page of the
`Switches` section in the :ref:`The_Project_Properties_Editor`. Several files
can be specified.

:ref:`The Stack Usage Editor <The_Stack_Usage_Editor>` can be used to edit
stack usage information for undefined subprograms.

The Stack Analysis Menu
-----------------------

All stack analysis related operations are reachable via the `Tools->Stack Analysis` menu:

*Analyze stack usage*
  Generates stack usage information for the root project.

*Open undefined subprograms editor*
  Opens undefined subprograms editor.

*Load last stack usage*
  Loads or re-loads last stack usage information for the root project.

*Clear stack usage data*
  Removes stack analysis data loaded in GPS and any associated information such
  as annotations in source editors.


The Contextual Menu
-------------------

When clicking on a project, file or subprogram entity (including the entities
listed in the coverage report), you have access to a *Stack Analysis* submenu.

This submenu contains the following entries, related to the entity selected:

*Show stack usage*
  Shows stack usage information for every subprogram of currently selected file.

*Hide stack usage*
  Hides stack usage information for every subprogram of currently selected file.

*Call tree for xxx*
  Opens Call Tree view for currently selected subprogram.

The Stack Usage Report
----------------------

.. _The_Stack_Usage_Report:

When the stack usage information is loaded, a report is displayed containing
a summary of the stack analysis.

The Stack Usage Editor
----------------------

.. _The_Stack_Usage_Editor:

The Stack Usage Editor allows to specify stack usage for undefined subprograms
and use these values to refine results of future analysis.

.. index:: screen shot
.. image:: stack-usage-editor.jpg

The Stack Usage Editor consists of two main areas. The notebook in the top area
allows to select the file to edit. It displays the contents of the file and
allows changing the stack usage of subprograms. The table in the bottom area
displays all subprograms whose stack usage information is not specified so that
they can be set.

Stack usage information for subprograms can be specified or changed by clicking
in the stack usage column on the right of the subprogram's name.  When a value
is specified in the bottom area table, the subprogram is moved to the top table
of the currently selected file. When a negative value is specified, the
subprogram is moved to the bottom table.

All changes are saved when the stack usage editor window is closed.

