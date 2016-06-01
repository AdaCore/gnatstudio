********
Projects
********


Project Wizard
==============

Go to the menu `Project->New...`: this is a standard wizard, with various
steps listed on the left area of the window.

The first page of the wizard allows you to select what kind of project you
want to build, depending on the information you have. Select the default
choice `Single Project`, and press `Forward`.

Type *sdc2* in the project name field.

Click on `Forward`: we are now on the language selection page.
It is possible to create a multi-language project by e.g. selecting the C or C++
check box.

Click on `Forward`: this is the source directories selection,
used to specify the project's sources. Click on the `Add` button,
and select the `struct` directory, then click on `OK` to validate.

Click on `Forward`: we are now on the `VCS page`. *VCS* stands for *Version
Control System*.  GPS provides a generic framework for *VCS* which allows it to
support new systems easily. Systems supported by default are CVS, ClearCase,
Subversion and GIT. Select `Auto`, which means that GPS will automatically
detect the version control system used, if any.

Click on `Forward`: this is the `Build` and `Exec` directory
selection, used to store object, ali files, ...

Click on the first `Browse` button, then click on
`obj`, and finally click on `OK`.

Click on `Forward`: this is the main units selection, used mainly for
building executables and debugging.

Click on `Add`, open the `common` directory and select
`sdc.adb`.

Click on `Forward`: this is the naming scheme editor.
GNAT is very flexible and can use any kind of naming scheme for Ada files.
In particular, you can easily set the default file
extensions (e.g by using one of the predefined schemes) and you
can also specify exceptions that use non standard file names.

Click on `Forward`: we're now in the switch selector. Go on the `Builder`
switch page and select `Recompile if switches changed`.

Select the `Ada` switch page.

Select `Full errors` and `Overflow checking`.  The boxes and the command line
(the text entry at the bottom of the page) are fully synchronized, e.g if you
click on the command line, and change `-gnatf` to `-gnat`, the `Full errors`
check box is unselected; now type `a` to get `-gnata`, and notice that `Enable
assertions` is now selected.

We've now created a project similar to the one used in this tutorial.

Click on `Cancel` to close the wizard.

Clicking on `Apply` instead would have created the project file
and loaded it in GPS.

.. _Project_properties:

Project properties
==================

In the project view, on the project *sdc*, use the contextual menu
`Project->Properties`.  All the properties set in the project wizard can be
found here as well.  You can switch between pages by clicking on the tabs
located along the left side of the window.

Once you're done exploring the property pages, click on the `Cancel`
button to close the properties window.

.. _Variable_editor:

Variable editor
===============

Select the window titled "Scenario".  If not available, you can open it
using the menu `Tools->Views->Scenario`.
This window contains a `Build` label.

This is a configuration variable. With GPS and the GNAT
project facility, you can define as many configuration variables as you want,
and modify any project settings (e.g. switches, sources, ...) based on the
values of configuration variables. These variables can also take any
number of different values.

The `Build` variable demonstrates a typical `Debug/Production`
configuration where we've set different switches for the two modes.

Right click on the `Build` label and select
`Edit properties of Build...`: this opens the
variable editor, where values can be added or renamed.
Close the variable editor by clicking on the `Cancel` button.

Now, let's take a look at the switches set in the project.

.. _Switch_editor:

Switch editor
=============

Select the menu item `Project->Edit File Switches`: a global switch editor is
displayed in the working area, showing the switches associated with each file
in the `sdc` project.

The editor lists the switches associated with each file in the project.  Gray
entries indicate default (global) switches.  Notice that
:file:`screen_output.adb` has specific switches, which are highlighted using a
different font.

Switch between `Debug` and `Production` mode in the `Build` combo box: the
switches are updated automatically.

Back to our project, let's now examine the dependencies between sources.

.. _Source_dependencies:

Source dependencies
===================

Select :file:`sdc.adb` in the `Project View` and then the contextual menu item
`Show dependencies for sdc.adb`: this will open a new graph showing the
dependencies between sources of the project.

Click on the right arrow of :file:`tokens.ads` to display the files that
:file:`tokens.ads` depends on. Similarly, click on the right arrow of
:file:`stack.ads`.

.. _Project_dependencies:

Project dependencies
====================

Back in the project view, on the *Sdc* project, select the contextual menu
`Project->Dependencies`, then on the `Add From File`, then open the *tutorial*
directory and click on the `projects` subdirectory. Select the file `prj1.gpr`.
Click on `Apply` to validate the change.

You can see the new dependency added in the project view, as a tree of
projects. In particular, project dependencies are duplicated: if you open the
`prj1` icon by clicking on the triangle, and then similarly
open the `prj2` icon, you will notice that the project `prj4` is displayed
twice: once as a dependency of `prj2`, and once as a dependency of `prj1`.

GPS can also display the graph of dependencies between projects: on *Sdc*
project, use the contextual menu `Show projects imported by Sdc`: this will
open a project hierarchy browser.

On the `Sdc.gpr` project, select the contextual menu `Show projects imported by
Sdc recursively`.

In the browser, you can move the project items, and select them to highlight
the dependencies.
