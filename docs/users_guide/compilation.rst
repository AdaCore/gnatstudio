.. _Compilation/Build:

*****************
Compilation/Build
*****************

.. index:: compilation
.. index:: build

This chapter describes how to compile files, build executables and run them.
Most capabilities can be accessed through the `Build` menu item, or through the
`Build` and `Run` contextual menu items, as described in the following section.

When compiler messages are detected by GPS, an entry is added in the *Locations
View*, allowing you to easily navigate through the compiler messages (see
:ref:`The_Locations_View`), or even to automatically correct some errors or
warnings (see :ref:`Code_Fixing`).

Compiler messages also appear as icons on the side of lines in the source
editors. When the mouse pointer is left on these icons, a tooltip appears,
listing the error messages found on this line. When GPS is capable of
automatically correcting the errors, clicking on the icon will apply the fix to
the source code. The icons on the side of editors are removed when the
corresponding entries are removed from :ref:`The_Locations_View`.

.. _The_Build_Menu:

The Build Menu
==============

The build menu gives access to capabilities related to checking, parsing and
compiling files, as well as creating and running executables.  note that this
menu is fully configurable via the `Targets` dialog, so what is documented in
this manual are the default menus.

See :ref:`The_Target_Configuration_Dialog`.

*Check Syntax*
  Check the syntax of the current source file. Display an error message in
  the *Messages* window if no file is currently selected.

*Check Semantic*
  Check the semantic of the current source file. Display an error message in
  the *Messages* window if no file is currently selected.

*Compile File*

  Compile the current file.

  By default, will display an intermediate dialog where you can add extra
  switches, or simply press :kbd:`Enter` to get the standard (or previous)
  switches.  Display an error message in the *Messages* window if no file is
  selected.

  If errors or warnings occur during the compilation, the corresponding
  locations will appear in the Locations View. If the corresponding Preference
  is set, the source lines will be highlighted in the editors (see
  :ref:`The_Preferences_Dialog`).  To remove the highlighting on these lines,
  remove the files from the Locations View using either the contextual menu
  (`Remove category`) or by closing the Locations View.

*Project*

  *Build <main>*
    The menu will list of all mains defined in your project hierarchy.
    Each menu item will build the selected main.

  *Build All*

    Build and link all main units defined in your project.  If no main unit is
    specified in your project, build all files defined in your project and
    subprojects recursively.  For a library project file, compile sources and
    recreate the library when needed.

  *Compile All Sources*

    Compile all source files defined in the top level project.

  *Build <current file>*

    Consider the currently selected file as a main file, and build it.

  *Custom Build...*

    Display a text entry where you can enter any external command. This menu is
    very useful when you already have existing build scripts, make files, ...
    and want to invoke them from GPS. If the `SHELL` environment variable is
    defined (to e.g. `/bin/sh`), then the syntax used to execute the command is
    the one for this shell. Otherwise, the command will be spawned directly by
    GPS without any shell interpretation.

*Clean*


  *Clean All*

    Remove all object files and other compilation artifacts associated to all
    projects related to the current one. It allows to restart a complete build
    from scratch.

  *Clean Root*

    Remove all object files and other compilation artifacts associated to the
    root project. It does not clean objects from other related projects.

*Makefile*

  If you have the *make* utility in your PATH, and have a file called
  :file:`Makefile` in the same directory as your project file is, or if you've
  set the `makefile` property in the `Make` section of the project properties
  (see :ref:`The_Project_Properties_Editor`), this menu will be displayed,
  giving access to all the targets defined in your makefile.

*Ant*

  If you have the *ant* utility in your PATH, and have a file called
  :file:`build.xml` in the same directory as your project file is, or if you've
  set the `antfile` property in the `Ant` section of the project properties
  (see :ref:`The_Project_Properties_Editor`), this menu will be displayed,
  giving access to all the targets defined in your ant file.

*Run*

  *main*

    For each main source file defined in your top level project, an entry is
    listed to run the executable associated with this main file.  Running an
    application will first open a dialog where you can specify command line
    arguments to your application, if needed. You can also specify whether the
    application should be run within GPS (the default), or using an external
    terminal.

    When running an application from GPS, a new execution window is added in
    the bottom area where input and output of the application is handled. This
    window is never closed automatically, even when the application terminates,
    so that you can still have access to the application's output. If you
    explicitly close an execution window while an application is still running,
    a dialog window will be displayed to confirm whether the application should
    be terminated.

    When using an external terminal, GPS launches an external terminal utility
    that will take care of the execution and input/output of your application.
    This external utility can be configured in the preferences dialog
    (*External Commands->Execute command*).

    The GPS execution windows have several limitations compared to external
    terminals. In particular, they do not handle signals like :kbd:`ctrl-z` and
    :kbd:`control-c`. In general, if you are running an interactive
    application, we strongly encourage you to run in an external terminal.

    Similarly, the `Run` contextual menu accessible from a project entity
    contains the same entries.

  *Custom...*

    Similar to the entry above, except that you can run any arbitrary
    executable.  If the `SHELL` environment variable is defined (to e.g.
    `/bin/sh`), then the syntax used to execute the command is the one for this
    shell. Otherwise, the command will be spawned directly by GPS without any
    shell interpretation.

*Recompute Xref info*
  .. index:: C
  .. index:: C++

  Recompute the cross-reference information for Ada, C and C++ source files.
  :ref:`Support_for_Cross-References`.

*Load xref info in memory*
  .. index:: C
  .. index:: C++

  Load all the cross-reference information in memory. This menu is generally
  not needed, :ref:`Support_for_Cross-References`.

*Settings*

  *Targets*

    .. index:: Targets

    This opens the Target Configuration Dialog.
    :ref:`The_Target_Configuration_Dialog`.

  *Toolchains*

    .. index:: Toolchains

    Open a dialog allowing the configuration of GPS for working with two
    compilation toolchains. This is particulary useful when compiling a project
    with an old compiler, while wanting up-to-date functionalities from the
    associated tools (gnatmetric, gnatcheck and so on).
    :ref:`Working_with_two_compilers`.

The `Tools->Interrupt` menu can be used to interrupt the last compilation or
run command. Once you have interrupted that last operation, you can interrupt
the previous one by selecting the same menu again.

However, the easiest way to interrupt a specific operation, no matter if it was
started last or not, is to use the `Task Manager`, through the
`Tools->Views->Tasks` menu. It will show one line per running process, and
right-clicking on any of these lines gives the possibility to interrupt that
process.

If your application is build through a Makefile, you should probably load the
:file:`Makefile.py` startup script (see the menu `/Tools/Plug-ins`).

.. _The_Target_Configuration_Dialog:

The Target Configuration Dialog
===============================

.. index:: Targets

GPS provides an interface for launching operations like building projects,
compiling individual files, performing syntax or semantic checks, and so on.
All these operations have in common that they involve launching an external
command, and parsing the output for error messages. In GPS, these operations
are called "Targets", and can be configured either through the Target
Configuration dialog, or through XML configuration.
:ref:`Customizing_build_Targets_and_Models`.

.. index:: screen shot
.. image:: target-configuration-dialog.jpg

This dialog is divided in two areas: on the left, a tree listing Targets, and,
in the main area, a panel for configuring the Target which is currently
selected in the tree.

The Targets tree
----------------

The Tree contains a list of targets, organized by categories.

On top of the tree are three buttons:

* The Add button creates a new target.
* The Remove button removes the currently selected target. Note that only
  user-defined targets can be removed, the default targets created by GPS cannot
  be removed.
* The Clone button creates a new user-defined target which is identical
  to the currently selected target.

The configuration panel
-----------------------

On top of the configuration panel, one can select the Target model.  The Model
determines the graphical options available in the "Command line" frame.

The "Revert" button resets all target settings to their original value.

The "Options" frame contains a number of options that are available for all
Targets.

* The Launch mode indicates the way the target is launched:

  * Manually:
    the target is launched when clicking on the corresponding icon
    in the toolbar, or when activating the corresponding menu item.
    In the latter case, a dialog is displayed, allowing last-minute
    modifications of the command line.

  * Manually with dialog:
    same as Manually, but the dialog is always displayed, even when
    clicking on the toolbar icon.

  * Manually with no dialog:
    same as Manually, but the dialog is never displayed, even when
    activating the menu item.

  * On file save:
    the Target is launched automatically by GPS when a file is saved.
    The dialog is never displayed.

  * In background:
    the Target is launched automatically in the background after each
    modification in the source editor. See `Background compilations`
    below.

* Icon: the icon to use for representing this target in the menus and in the
  toolbar. To use one of your icons, you must register a icons using the
  `<stock>` XML customization node. (:ref:`Adding_stock_icons`). Then, use
  "custom" choice and enter in the text field the ID of the icon.

* Target type: type of target described. If empty, or set to `Normal`,
  represents a simple target. If set to another value, represents multiple
  subtargets.  For example, if set to `main`, each subtarget corresponds to a
  Main source as defined in the currently loaded project.  Other custom values
  may be defined, and then handled via the `compute_build_targets` hook.

The "Display" frame indicates where the launcher for this target should be
visible.

* in the toolbar: when active, a button is displayed in the main toolbar,
  allowing to quickly launch a Target.

* in the main menu: whether to display a menu item corresponding to the Target
  in the main GPS menu. By default, Targets in the "File" category are listed
  directly in the Build menu, and Targets in other categories are listed in a
  submenu corresponding to the name of the category.

* in contextual menus for projects: whether to display an item in the
  contextual menu for projects in the Project View

* in contextual menus for files: whether to display an item in the contextual
  menus for files, for instance in file items in the Project View or directly
  on source file editors.

The "Command line" contains a graphical interface for some configurable
elements of the Target, which are specific to the Model of this Target.

The full command line is displayed at the bottom. Note that it may contain
Macro Arguments. For instance if the command line contains the string "%PP",
GPS will expand this to the full path to the current project. For a full list
of available Macros, see :ref:`Macro_arguments`.

Background compilations
-----------------------

GPS is capable of launching compilation targets in the background. This means
that GPS will launch the compiler on the current state of the file in the
editor.

Error messages resulting from background compilations are not listed in the
Locations view or the Messages window. The full messages are listed in the
Background Build console, accessible from the menu `Tools->Console`.  Error
messages which contain a source location indication are shown as icons on the
side of lines in editors, and the exact location is highlighted directly in the
editor. On both of these places, tooltips show the contents of the error
messages.

Messages from background compilations are removed automatically either when a
new background compilation has finished, or when a non-background compilation
is launched.

GPS will launch background compilations for all targets that have a `Launch
mode` set to `In background`, after modifications occur in a source editor.
Background compilation is useful mostly for targets such as `Compile File` or
`Check Syntax`. For targets that work on Mains, the last main that was used in
a non-background is considered, defaulting to the first main defined in the
project hierarchy.

Background compilations are not launched while GPS is already listing results
from non-background compilations, ie as long as there are entries in the
Locations View showing entries in the `Builder results` category.

.. _The_Build_Mode:

The Build Mode
==============

.. index:: Mode

GPS provides an easy way to build your project with different options, through
the Mode selection, located in the main toolbar.

When the Mode selection is set to `default`, the build is done using the
switches defined in the project. When the Mode selection is set to another
value, then specialized parameters are passed to the builder. For instance, the
`gcov` Mode adds all the compilation parameters needed to instrument the
produced objects and executables to work with the `gcov` tool.

In addition to changing the build parameters, the Mode selection has the effect
of changing the output directory for objects and executables. For instance,
objects produced under the `debug` mode will be located in the `debug`
subdirectories of the object directories defined by the project.  This allows
switching from one Mode to another without having to erase the objects
pertaining to a different Mode.

It is possible to define new Modes using XML customization, see
:ref:`Customizing_build_Targets_and_Models`.

Note that the Build Mode affects only builds done using recent versions of
gnatmake and gprbuild. The Mode selection has no effect on builds done through
Targets that launch other builders.

.. _Working_with_two_compilers:

Working with two compilers
==========================

.. index:: Toolchains

This functionality is intended for people whose projects need to be compiled
with a specific (old) version of the GNAT toolchain, while still desiring to
take full advantage of up-to-date associated tools for non-compilation actions,
such as checking the code against a coding standard, getting better
cross-reference browsing in GPS, computing metrics and so on.

GPS now allows you to handle this case. To configure GPS to make it handle two
compiler toolchains, you need to use the `Build->Settings->Toolchains` menu.
This will open a dialog where you can activate the multiple-toolchains mode.

.. index:: screen shot
.. image:: toolchains-config.jpg

In this dialog, two paths need to be configured: the compiler path and the
tools path. The first one is used to actually compile the code, while the
second one is used to run up-to-date tools to get more functionalities or
accurate results.

Note that GPS will only enable the `OK` button when the two paths are set to
different location, since otherwise it does not make sense to enable the
multiple toolchains set up.

From this dialog, you can also activate an automated cross-reference
generation. The cross-reference files are the .ali files generated by the GNAT
compiler together with the compiled object. Those files are used by GPS for
several functionalities, such as cross-reference browsing or documentation
generation. Having those .ali files produced by a recent compiler helps having
more accurate results with those functionalities, but might interract badly
with an old compiler also reading those .ali files for compiling a project.

If the automated xref generation is activated, then GPS will generate those
.ali files using the compiler found in the tools path, and place them in a
directory distinct from the one used by the actual compiler. This allows GPS to
take full benefit of up-to-date cross-reference files, while keeping the old
toolchain happy as its .ali files remain untouched.

Note that the cross-reference files generation does not output anything in the
"Messages" window, so as to not confuse the output of the regular build
process. If needed, you can see the output of the cross-ref generation command
by selecting the `Tools->Consoles->Auxiliary Builds` menu.

Interaction with the remote mode
--------------------------------

The ability to work with two compilers has impacts on the remote mode
configuration: paths defined here are local paths, so they have no meaning on
the server side.

To handle the case of using a specific compiler version on the remote side
while still wanting up-to-date tools, the following behavior is applied when
both a remote compilation server is defined, and the multiple toolchains mode
is activated:

* The compiler path is ignored when a remote build server is defined. All
  compilation actions are then performed normally on the build server.
* The tools path is however taken into account, and all related actions
  are performed on the local machine using this path.
* The cross-reference files are taken care of by the rsync mechanism
  so that they don't get overwritten during local and remote host
  synchronisations, as build and cross-reference generation actions occur at
  the same time, on the local machine and on the distant server.

