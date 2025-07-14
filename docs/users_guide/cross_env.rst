.. _Working_in_a_Cross_Environment:

******************************
Working in a Cross Environment
******************************

.. index:: cross environment

This chapter explains how to adapt your project and configure GNAT Studio when
working in a cross environment.

.. _Customizing_your_Projects:

Customizing your Projects
=========================

.. index:: project

This section describes some possible ways to customize your projects when
working in a cross environment. For more details on the project capabilities,
see :ref:`Project_Handling`.

Several GPR project attributes are particularly relevant to
cross environments: the :guilabel:`Target` and the :guilabel:`Runtime` attributes.
These attributes will determine which compiler and runtime will be used to build your program.

You can read the `GNAT User’s Guide Supplement for Cross Platforms <https://docs.adacore.com/live/wave/gnat_ugx/html/gnat_ugx/gnat_ugx.html>`_
for more information about development in cross environments.

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
:guilabel:`IDE'Program_Host` and :guilabel:`IDE'Protocol` project attributes.
You can also connect (or reconnect) to the remote agent at any time via
the :menuselection:`Debug --> Debug --> Connect to Board...` menu.

For example, if you are using the *Tornado* environment, with a target
server called :samp:`target_ppc`, set the :guilabel:`IDE'Protocol` to
:command:`wtx` and the :guilabel:`IDE'Program_Host` to :command:`target_ppc`.

GNAT Studio waits for a certain amount of time when trying to connect to a
target: if GDB does not asnwer during this time period, GNAT Studio interupts
the current debugger command and assumes that we failed to connect to the
target. You can set this time period with the
:menuselection:`Debugger --> Connection timeout` preference.

To load a new module on the target, select the
:menuselection:`Debug --> Debug --> Load File...` menu.

If a module has been loaded on the target and is not known to the current
debug session, use the :menuselection:`Debug --> Debug --> Add Symbols...`
menu to load the symbol tables in the current debugger.

For bare-metal development, all these steps can be done at once using the
:guilabel:`Flash to Board` and :guilabel:`Debug on Board` toolbar buttons.
These buttons allow you to build, flash and/or debug your software on the
board, spawning the remote debug agent set in the :guilabel:`IDE'Connection_Tool`
project attribute.
GNAT Studio currently supports :guilabel:`OpenOCD`, :guilabel:`st-util` and :guilabel:`py-ocd`
as connection tools.
You can leave the :guilabel:`IDE'Connection_Tool` project attribute empty if you are
using a connection tool that is not supported by GNAT Studio: in that case,
GNAT Studio will still try to connect to the board and everything should work
fine if your connection tool has been spawned correctly.
