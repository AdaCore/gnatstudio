********
Projects
********


Project Wizard
==============

Go to the menu `File->New Project...`: this opens up the GPS project
creation wizard.

The first page of the wizard allows you to select a pre-defined project
template in the left-hand pane. These project templates are organized
according to the technology they use (e.g: `AWS`) or the platform that
is targeted (e.g: `STM32F4 compatible`). The description of the currently
selected project is displayed on the right-hand side pane.

Select a project template and click on `Next`: a page asking you the name and
the location of your project will appear. This page may also list project
template-specific options.

Once completed, click on `Apply` to actually create the project. Note that you
can still can customize your newly created project after is creation using the
:ref:`Project_properties` editor.

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
using the menu `View->Scenario`.
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

Select the menu item `View->File Switches`: a global switch editor is
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
directory and click on the `projects` subdirectory. Select the file `prj1.gpr`,
click on `OK`.  Click on `Apply` to validate the change.

You can see the new dependency added in the project view, as a list (or tree,
if 'Show flat view' is enabled in local configuration menu) of projects. In
particular, project dependencies are duplicated when tree view is used: if you
open the `prj1` icon by clicking on the triangle, and then similarly open the
`prj2` icon, you will notice that the project `prj4` is displayed twice: once
as a dependency of `prj2`, and once as a dependency of `prj1`.

GPS can also display the graph of dependencies between projects: on *Sdc*
project, use the contextual menu `Show projects imported by Sdc`: this will
open a project hierarchy browser.

On the *Sdc* project, select the contextual menu `Show projects imported by
Sdc recursively`.

In the browser, you can move the project items, and select them to highlight
the dependencies.
