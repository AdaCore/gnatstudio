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

When using the project editor to modify the project's properties, two areas are
particularly relevant to cross environments: `Cross environment` part of the
`General` page, and `Toolchains` part of the `Languages` page.

In the `Toolchains` section, you will typically either scan your system to
display found toolchains, and select the one corresponding to your cross
environment or use the Add button and manually select the desired cross
environment.

If needed, you can also modify manually some of the tools defined in this
toolchain in the `Details` part of the `Languages` page.

For example, assuming you have an Ada project, and using a powerpc VxWorks
configuration. Hitting the scan button, you should see the toolchain
`powerpc-wrs-vxworks` appearing in the `Toolchains` section.  Selecting this
toolchain will change the `Details` part, displaying the relevant tools (e.g.
*Gnatls* to `powerpc-wrs-vxworks-gnatls` and *Debugger* to
`powerpc-wrs-vxworks-gdb` ...).

The list of toolchains and their default values that can be selected when using
the Add button can be modified via a custom xml file. See
:ref:`Customizing_and_Extending_GPS` and in particular
:ref:`Toolchains_customization` for further information.

If you are using an alternative run time, e.g. a *soft float* run time, you
need to add the option `--RTS=soft-float` to the *Gnatls* property, e.g:
`powerpc-wrs-vxworks-gnatls --RTS=soft-float`, and add this same option to the
*Gnatmake* switches in the switch editor.  See :ref:`Switches <Switches>` for
more details on the switch editor.

To modify your project to support configurations such as multiple targets, or
multiple hosts, you can create scenario variables, and modify the setting of
the Toolchains parameters based on the value of these variables. See
:ref:`Scenarios_and_Configuration_Variables` for more information on these
variables.

For example, you may want to create a variable called `Target` to handle
the different kind of targets handled in your project:

*Target*
  Native, Embedded

*Target*
  Native, PowerPC, M68K

Similarly, you may define a `Board` variable listing the different boards used
in your environment and change the *Program host* and *Protocol* settings
accordingly.

In some cases, it is useful to provide a different body file for a given
package (e.g. to handle target specific differences). A possible approach in
this case is to use a configuration variable (e.g. called `TARGET`), and
specify a different naming scheme for this body file (in the project
properties, `Naming` tab), based on the value of `TARGET`.

.. _Debugger_Issues:

Debugger Issues
===============

.. index:: debugger

This section describes some debugger issues that are specific to cross
environments. You will find more information on debugging by reading
:ref:`Debugging`.

To connect automatically to the right remote debug agent when starting a
debugging session (using the menu `Debug->Initialize`), be sure to specify the
`Program host` and `Protocol` project properties, as described in the previous
section.

For example, if you are using the *Tornado* environment, with a target server
called `target_ppc`, set the `Protocol` to `wtx` and the `Program host` to
`target_ppc`.

Once the debugger is initialized, you can also connect to a remote agent by
using the menu `Debug->Debug->Connect to Board...`. This will open a dialog
where you can specify the target name (e.g. the name of your
.. index:: board

board or debug agent) and the communication protocol.

In order to load a new module on the target, you can select the menu
`Debug->Debug->Load File...`.

If a module has been loaded on the target and is not known to the current debug
session, use the menu `Debug->Debug->Add Symbols...` to load the symbol tables
in the current debugger.

Similarly, if you are running the underlying debugger (gdb) on a remote
machine, you can specify the name of this machine by setting the `Tools host`
field of the project properties.
