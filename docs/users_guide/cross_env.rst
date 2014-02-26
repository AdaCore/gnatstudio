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
particularly relevant to cross environments: the :guilabel:`Cross
environment` section of the :guilabel:`General` page, and the
:guilabel:`Toolchains` section of the :guilabel:`Languages` page.

In the :guilabel:`Toolchains` section, you typically either scan your
system to display toolchains found by GPS and select the one
corresponding to your cross environment or use the :guilabel:`Add`
button and manually select the desired cross environment.

If needed, you can also manually modify some of the tools defined in
this toolchain in the :guilabel:`Details` section of the
:guilabel:`Languages` page.

For example, assume you have an Ada project using a Powerpc VxWorks
configuration.  When you press scan button, you should see the
toolchain :command:`powerpc-wrs-vxworks` appear in the
:guilabel:`Toolchains` section.  Selecting this toolchain changes the
:guilabel:`Details` section, displaying the relevant tools (e.g.,
changing :guilabel:`Gnatls` to :command:`powerpc-wrs-vxworks-gnatls` and :guilabel:`Debugger` to :command:`powerpc-wrs-vxworks-gdb`).

You can modify the list of toolchains that can be selected when using the
:guilabel:`Add` button and their default values via a custom XML file. See
:ref:`Customizing_and_Extending_GPS` and in particular
:ref:`Toolchains_customization` for further information.

If you are using an alternative run time, e.g. a *soft float* run time,
you need to add the option :command:`--RTS=soft-float` to the
:guilabel:`Gnatls` property, e.g: :command:`powerpc-wrs-vxworks-gnatls
--RTS=soft-float` and add this same option to the :guilabel:`Gnatmake`
switches in the switch editor.  See :ref:`Switches <Switches>` for
more details on the switch editor.

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
:menuselection:`Debug->Initialize`), be sure to specify the
:guilabel:`Program host` and :guilabel:`Protocol` project properties,
as described in the previous section.

For example, if you are using the *Tornado* environment, with a target
server called :samp:`target_ppc`, set the :guilabel:`Protocol` to
:command:`wtx` and the :guilabel:`Program host` to :command:`target_ppc`.

Once the debugger is initialized, connect to a remote agent by using the
:menuselection:`Debug->Debug->Connect to Board...` manu. This opens a
dialog where you can specify the target name (e.g. the name of your
.. index:: board
board or debug agent) and the communication protocol.

To load a new module on the target, select the
:menuselection:`Debug->Debug->Load File...` menu.

If a module has been loaded on the target and is not known to the current
debug session, use the :menuselection:`Debug->Debug->Add Symbols...` menu
to load the symbol tables in the current debugger.

Similarly, if you are running the underlying debugger (gdb) on a remote
machine, specify the name of this machine by setting the
:guilabel:`Tools host` field of the project properties.
