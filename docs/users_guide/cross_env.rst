.. _Working_in_a_Cross_Environment:

******************************
Working in a Cross Environment
******************************

.. index:: cross environment

This chapter explains how to adapt your project and configure GPS when working
in a cross environment.

.. _Customizing_your_Projects:

Customizing your Projects
=========================

.. index:: project

This section describes some possible ways to customize your projects when
working in a cross environment. For more details on the project capabilities,
see :ref:`Project_Handling`.

Two areas of the project editor to modify the project's properties are
particularly relevant to cross environments: the :guilabel:`Toolchain`
page, and the :guilabel:`Embedded` page.

In the :guilabel:`Toolchains` page, the toolchains that have been found by
GPS while scanning your host are displayed: you can select the one
corresponding to your cross environment, or use the :guilabel:`+`
button and manually select the desired cross environment.

If needed, you can also manually modify some of the tools defined in
this toolchain in the :guilabel:`Tools` section of the
:guilabel:`Toolchains` page.

For example, assume you have an Ada project using a Powerpc VxWorks
configuration. You should see the toolchain :command:`powerpc-wrs-vxworks`
appear in the :guilabel:`Toolchains` section.  Selecting this toolchain changes
the :guilabel:`Tools` section, displaying the relevant tools (e.g., changing
:guilabel:`Gnatls` to :command:`powerpc-wrs-vxworks-gnatls` and
:guilabel:`Debugger` to :command:`powerpc-wrs-vxworks-gdb`).

You can modify the list of toolchains that can be selected when using the
:guilabel:`+` button and their default values via a custom XML file. See
:ref:`Customizing_and_Extending_GPS` and in particular
:ref:`Toolchains_customization` for further information.

The :guilabel:`Runtimes` section allows you to choose a particular runtime
for your project. The runtimes that have been found by GPS for the
selected toolchain are directly displayed in the combobox. If you want to
use a custom runtime (e.g: a runtime which is not packaged with the
selected toolchain), specify its path in the combobox's entry.

To modify your project to support configurations such as multiple targets
or multiple hosts, create scenario variables and modify the setting of the
Toolchains parameters based on the value of these variables. See
:ref:`Scenarios_and_Configuration_Variables` for more information on these
variables.

For example, you may want to create a variable called :samp:`Target`
to handle the different kind of targets handled in your project:

*Target*

  Native, Embedded

*Target*

  Native, PowerPC, M68K

Similarly, you may define a :samp:`Board` variable listing the different boards
used in your environment and change the :guilabel:`Program host` and
:guilabel:`Protocol` settings accordingly.

In some cases, you may want to provide a different body file for a
specific package (e.g., to handle target-specific differences). A
possible approach in this case is to use a configuration variable
(e.g. called :samp:`TARGET`) and specify a different naming scheme for
this body file (in the project properties :guilabel:`Naming` tab)
based on the value of :samp:`TARGET`.

.. _Debugger_Issues:

Debugger Issues
===============

.. index:: debugger

This section describes debugger issues specific to cross
environments. You will find more information on debugging at
:ref:`Debugging`.

To automatically connect to the correct remote debug agent when
starting a debugging session (using the menu
:menuselection:`Debug --> Initialize`), be sure to specify the
:guilabel:`Program host` and :guilabel:`Protocol` project properties,
which can be found in the :guilabel:`Embedded` page. You can also connect
(or reconnect) to the remote agent at any time via the
:menuselection:`Debug --> Debug --> Connect to Board...` menu.

For example, if you are using the *Tornado* environment, with a target
server called :samp:`target_ppc`, set the :guilabel:`Protocol` to
:command:`wtx` and the :guilabel:`Program host` to :command:`target_ppc`.

GPS waits for a certain amount of time when trying to connect to a
target: if GDB does not asnwer during this time period, GPS interupts the
current debugger command and assumes that we failed to connect to the target.
You can set this time period with the
:menuselection:`Debugger --> Connection timeout` preference.

To load a new module on the target, select the
:menuselection:`Debug --> Debug --> Load File...` menu.

If a module has been loaded on the target and is not known to the current
debug session, use the :menuselection:`Debug --> Debug --> Add Symbols...`
menu to load the symbol tables in the current debugger.

For bare-metal development, all these steps can be done at once using the
:guilabel:`Flash to Board` and :guilabel:`Debug on Board` toolbar buttons.
These buttons allow you to build, flash and/or debug your software on the
board, spawning the remote debug agent set in the :guilabel:`Connection tool`
project property from the :guilabel:`Embedded` page. GPS currently supports
:guilabel:`OpenOCD`, :guilabel:`st-util` and :guilabel:`py-ocd` as connection
tools.
You can leave the :guilabel:`Connection tool` attribute empty if you are
using a connection tool that is not supported by GPS: in that case, GPS
will still try to connect to the board and everything should work fine
if your connection tool has been spawned correctly.
