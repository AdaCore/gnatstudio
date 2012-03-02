.. _Environment:

***********
Environment
***********

.. highlight:: ada

.. index:: environment

.. _Command_Line_Options:

Command Line Options
====================

.. index:: command line
.. index:: options
.. index:: example

The command line options are::

  Usage:
     gps [options] [-Pproject-file] [[+line] source1] [[+line] source2] ...
  Options:
     --help              Show this help message and exit
     --version           Show the GPS version and exit
     --debug[=program]   Start a debug session and optionally load the
                         program with the given arguments
     --debugger debugger Specify the debugger's command line
     --hide              Hide GPS main window
     --host=tools_host   Use tools_host to launch tools (e.g. gdb)
     --target=TARG:PRO   Load program on machine TARG using protocol PRO
     --load=lang:file    Execute an external file written in the
                         language lang
     --eval=lang:file    Execute an in-line script written in the
                         language lang
     --readonly          Open all files in read-only mode
     --server=port       Start GPS in server mode, opening a socket on the
                         given port
     --tracelist         Output the current configuration for logs
     --traceon=name      Activate the logs for a given module
     --traceoff=name     Deactivate the logs for a given module
     --tracefile=file    Parse an alternate configuration file for the logs

  Source files can be absolute or relative pathnames.
  If you prepend a file name with '=', this file will be
  searched anywhere on the project's source path

  To open a file at a given line, use the '+line' prefix, e.g.
  gps +40 source.adb

  `tools_host` corresponds to a remote host's nickname as defined in
  :ref:`Setup_the_remote_servers`.

.. _Environment_Variables:

Environment Variables
=====================

.. index:: environment

.. index:: environment variables

The following environment variables can be set to override some default
settings in GPS:

*GPS_HOME*
  .. index:: GPS_HOME
  .. index:: Windows

  Override the variable HOME if present. All the configuration files and
  directories used by GPS are either relative to $HOME/.gps (%HOME%\\.gps under
  Windows) if GPS_HOME is not set, or to $GPS_HOME/.gps (respectively
  %GPS_HOME%\\.gps) if set.

*GPS_DOC_PATH*
  .. index:: GPS_DOC_PATH

  Set the search path for the documentation. :ref:`Adding_New_Help_Files`.

*GPS_CUSTOM_PATH*
  .. index:: GPS_CUSTOM_PATH

  Contains a list of directories to search for custom files. See
  :ref:`Customizing_through_XML_and_Python_files` for more details.

*GPS_CHANGELOG_USER*
  .. index:: GPS_CHANGELOG_USER

  Contains the user and e-mail to use in the global ChangeLog files. Note that
  the common usage is to have two spaces between the name and the e-mail. Ex:
  "John Does  <john.doe@home.com>"

*GPS_STARTUP_PATH*
  .. index:: GPS_STARTUP_PATH

  Contains the value of the `PATH` environment variable just before GPS was
  started. This is used by GPS to restore the proper environment before
  spawning applications, no matter what particular directories it needed to set
  for its own purpose.

*GPS_STARTUP_LD_LIBRARY_PATH*
  .. index:: GPS_STARTUP_LD_LIBRARY_PATH

  Same as `GPS_STARTUP_LD_LIBRARY_PATH` but for the `LD_LIBRARY_PATH`
  variable.

*GPS_MEMORY_MONITOR*
  .. index:: GPS_MEMORY_MONITOR

  If set, GPS will add special code on every allocation and deallocation, thus
  slowing things down a bit, that makes it possible to check where the biggest
  amount of memory is allocated, through the `GPS.debug_memory_usage` python
  command.

*GPS_PYTHONHOME*
  .. index:: GPS_PYTHONHOME

  If set, the Python interpreter will look for libraries in the subdirectory
  lib/python<version> of the directory contained in `GPS_PYTHONHOME`.

*GNAT_CODE_PAGE*
  .. index:: GNAT_CODE_PAGE

  This variable can be set to `CP_ACP` or `CP_UTF8` and is used to control the
  code page used on Windows platform. The default is `CP_UTF8` to support more
  languages. If file or directory names are using accents for example it may be
  necessary to set this variable to `CP_ACP` which is the default Windows ANSI
  code page.

*GPS_ROOT*
  .. index:: GPS_ROOT

  Override and hardcode the default root installation directory.
  This variable should in general not be needed, except by GPS developers,
  in some rare circumstances. GPS will find all its resource files (e.g.
  images, plug-ins, xml files) from this root prefix, so setting GPS_ROOT
  to a wrong value will cause GPS to misbehave.

.. _Running_GPS_on_Mac_OS_X:

Running GPS on Mac OS X
=======================

.. index:: Mac OS

The current version of GPS on Mac OS X requires an X11 server. Such a server is
distributed with Mac OS X Panther and Mac OS X Tiger.

Additionally, if you are launching GPS from a standard Terminal, you need to
specify the display on which to launch GPS, by typing::

   export DISPLAY=:0

before launching GPS.

Note: GPS does not support files with line endings in CR.

.. _Files:

Files
=====

.. index:: files

*$HOME/.gps*
  .. index:: Windows
  .. index:: HOME

  GPS state directory. Defaults to C:\\.gps under Windows systems if HOME or
  USERPROFILE environment variables are not defined.

*$HOME/.gps/log*
  .. index:: log

  .. _log_file:

  Log file created automatically by GPS.  When GPS is running, it will create a
  file named :file:`log.<pid>`, where :file:`<pid>` is the GPS process id, so
  that multiple GPS sessions do not clobber each other's log. In case of a
  successful session, this file is renamed :file:`log` when exiting; in case of
  an unexpected exit (a bug box will be displayed), the log file is kept under
  its original name.

  Note that the name of the log file is configured by the :file:`traces.cfg`
  file.

*$HOME/.gps/aliases*
  .. index:: aliases

  File containing the user-defined aliases (:ref:`Defining_text_aliases`).


*$HOME/.gps/plug-ins*
  Directory containing files with user-defined plug-ins.  All xml and python
  files found under this directory are loaded by GPS during start up.  You can
  create/edit these files to add your own menu/tool-bar entries in GPS, or
  define support for new languages.
  :ref:`Customizing_through_XML_and_Python_files` and
  :ref:`Adding_support_for_new_languages`.

*$HOME/.gps/keys.xml*
  Contains all the key bindings for the actions defined in GPS or in the
  custom files. This only contains the key bindings overridden through the
  key shortcuts editor (see :ref:`The_Key_Manager_Dialog`).

*$HOME/.gps/gtkrc*
  .. index:: Dynamic Key Binding

  .. index:: gtkrc

  Configuration and theme file for gtkrc. This file can be edited to activate
  gtk+ specific aspects, or change the look of GPS in some measure. Mostly,
  everything can be done through the standard GPS preferences, but this file
  can be used to get access to the old GPS preference "Dynamic Key Binding".
  This preference activated a gtk+ behavior were key shortcuts for menu can be
  changed by simply pressing the appropriate key combination when the mouse is
  over that menu. It has various dangereous aspects and is not fully supported
  by GPS, so was removed as a preference, but you can add the following line in
  :file:`gtkrc` to get this back::

     gtk-can-change-accels=1
    
*$HOME/.gps/actions.xml*
  Contains the definition of all the actions that were defined through the
  graphical interface. This is loaded last, and overrides all actions defined
  elsewhere.

*$HOME/.gps/perspectives.xml*
  Desktop file in XML format (using the menu `File->Save More->Desktop`),
  loaded automatically if found.

*$HOME/.gps/locations.xml*
  This file contains the list of locations that GPS has previously edited. It
  corresponds to the history navigation (`Navigate->Back` and
  `Navigate->Forward`)

*$HOME/.gps/properties.xml*
  This file is used to store file-specific properties across GPS sessions. In
  particular, it contains the encoding to use for various files when the
  default encoding isn't appropriate.

*$HOME/.gps/histories.xml*
  .. index:: history

  Contains the state and history of combo boxes (e.g. the
  `Run->Custom...` dialog).

*$HOME/.gps/targets.xml*
  .. index:: targets

  Contains the build targets defined by the user.

*$HOME/.gps/preferences*
  .. index:: preferences

  Contains all the preferences in XML format, as specified in the
  preferences menu.

*$HOME/.gps/traces.cfg*
  Default configuration for the system traces. These traces are used to analyze
  problems with GPS.  By default, they are sent to the file
  :file:`$HOME/.gps/log.<pid>`.

  This file is created automatically when the :file:`$HOME/.gps/` directory is
  created. If you remove it manually, it won't be recreated the next time you
  start GPS.

*$HOME/.gps/startup.xml*
  This file contains the list of scripts to load at startup, as well as
  additional code that need to be executed to setup the script.

*$HOME/.gpe/activity_log.tmplt*
  Template file used to generate activities' group commit-log and patch
  file's header. If not present the system wide template (see below) is
  used. The set of configurable tags are described into this template.

  .. index:: activity log template
  .. index:: activity, log template

*prefix*
  The prefix directory where GPS is installed, e.g :file:`/opt/gps`.

*prefix*/bin
  The directory containing the GPS executables.

*prefix*/etc/gps
  The directory containing global configuration files for GPS.

*prefix*/lib
  This directory contains the shared libraries used by GPS.

*prefix*/share/doc/gps/html
  GPS will look for all the documentation files under this directory.

*prefix*/share/examples/gps
  This directory contains source code examples.

*prefix*/share/examples/gps/language
  This directory contains sources showing how to provide a shared library to
  dynamically define a new language. See
  :ref:`Adding_support_for_new_languages`.

*prefix*/share/examples/gps/tutorial
  This directory contains the sources used by the GPS tutorial.

  .. index:: url

  See `gps-tutorial.html <gps-tutorial.html>`_.

*prefix*/share/gps/plug-ins
  Directory containing files with system-wide plug-ins (xml and python files)
  loaded automatically at start-up.

*prefix*/share/gps/library
  Directory containing files with system-wide plug-ins (xml and python files)
  that are not loaded automatically at startup, but can be selected in the
  Plug-ins editor.

*prefix*/share/gps/gps-animation.png
  .. index:: png

  Default image displayed in the top right corner of GPS when GPS is idle.

*prefix*/share/gps/gps-animation.gif
  .. index:: gif

  Animated image displayed in the top right corner of GPS to indicate that
  actions (e.g compilation) are on going. If you remove this file, the idle
  image (:file:`gps-animation.png`) will always be displayed.

*prefix*/share/gps/gps-splash.png
  .. index:: png

  Splash screen displayed by default when GPS is started.

*prefix*/share/gps/perspectives.xml
  .. index:: default desktop
  .. index:: desktop, default

  This is the description of the default desktop that GPS uses when the user
  hasn't defined his own default desktop and no project specific desktop
  exists.  You can modify this file if you want, knowing that this will impact
  all users of GPS sharing this installation.  The format of this file is the
  same as $HOME/.gps/perspectives.xml, which can be copied from your own
  directory if you wish.

*prefix*/share/gps/default.gpr
  .. index:: default project

  Default project used by GPS. Can be modified after installation time to
  provide useful default for a given system or project.

*prefix*/share/gps/readonly.gpr
  Project used by GPS as the default project when working in a read-only
  directory.

*prefix*/share/gps/activity_log.tmplt
  Template file used by default to generate activities' group commit-log
  and patch file's header. This file can be copied into user home
  directory and customized (see above).

*prefix*/share/locale
  Directory used to retrieve the translation files, when relevant.


.. _Reporting_Suggestions_and_Bugs:

Reporting Suggestions and Bugs
==============================

.. index:: suggestions
.. index:: submitting bugs

If you would like to make suggestions about GPS, or if you encountered a bug,
please report it to `mailto:report@gnat.com <mailto:report@gnat.com>`_ if you
are a supported user, and to `mailto:gps-devel@lists.act-europe.fr
<mailto:gps-devel@lists.act-europe.fr>`_ otherwise.

Please try to include a detailed description of the problem, including sources
to reproduce it if possible/needed, and/or a scenario describing the actions
performed to reproduce the problem, as well as the tools (e.g *debugger*,
*compiler*, *call graph*) involved.

The files :file:`$HOME/.gps/log` may also
bring some useful information when reporting a bug.

In case GPS generates a bug box, the log file will be kept under a separate
name (:file:`$HOME/.gps/log.<pid>` so that it does not get erased by further
sessions. Be sure to include the right log file when reporting a bug box.

Solving Problems
================

.. index:: problems
.. index:: solving problems

This section addresses some common problems that may arise when using or
installing GPS.

*Non-privileged users cannot start GPS*
  Q: I have installed GPS originally as super user, and ran GPS successfully,
  but normal users can't.

  A: You should check the permissions of the directory $HOME/.gps and its
  subdirectories, they should be owned by the user.

*GPS crashes whenever I open a source editor*
  This is usually due to font problems. Editing the file
  :file:`$HOME/.gps/preferences` and changing the name of the fonts, e.g
  changing *Courier* by *Courier Medium*, and *Helvetica* by *Sans*
  should solve the problem.

*GPS refuses to start the debugger*
  .. index:: debugger

  If GPS cannot properly initialize the debugger (using the menu
  `Debug->Initialize`), it is usually because the underlying debugger (gdb)
  cannot be launched properly. To verify this, try to launch the 'gdb' command
  from a shell (i.e outside GPS). If gdb cannot be launched from a shell, it
  usually means that you are using a wrong version of gdb (e.g a version of gdb
  built for Solaris 8, but run on Solaris 2.6).

*GPS is frozen during a debugging session*
  .. index:: debugger

  If GPS is no longer responding while debugging an application you should
  first wait a little bit, since some communications between GPS and gdb can
  take a long time to finish. If GPS is still not responding after a few
  minutes, you can usually get the control back in GPS by either typing
  :kbd:`Ctrl-C` in the shell where you've started GPS: this should unblock it;
  if it does not work, you can kill the gdb process launched by GPS using the
  `ps` and `kill`, or the `top` command under Unix,

  .. index:: Unix
  .. index:: Windows

  and the `Task Manager` under Windows: this will terminate your debugging
  session, and will unblock GPS.

*My Ada program fails during elaboration. How can I debug it ?*
  .. index:: -g
  .. index:: gnatmake

  If your program was compiled with GNAT, the main program is generated by the
  binder. This program is an ordinary Ada (or C if the *-C* switch was used)
  program, compiled in the usual manner, and fully debuggable provided that the
  *-g* switch is used on the *gnatlink* command (or *-g* is used in the
  *gnatmake* command itself).

  The name of this package containing the main program is :file:`b~xxx.ads/adb`
  where xxx is the name of the Ada main unit given in the gnatbind command, and
  you can edit and debug this file in the normal manner. You will see a series
  of calls to the elaboration routines of the packages, and you can debug these
  in the usual manner, just as if you were debugging code in your application.

*How can I debug the Ada run-time library ?*

  The run time distributed in binary versions of GNAT hasn't been compiled with
  debug information. Thus, it needs to be recompiled before you can actually
  debug it.

  The simplest is to recompile your application by adding the switches *-a* and
  *-f* to the *gnatmake* command line. This extra step is then no longer
  required, assuming that you keep the generated object and ali files
  corresponding to the GNAT run time available.

  Another possibility on Unix systems is to use the file
  :file:`Makefile.adalib` that can be found in the adalib directory of your
  GNAT installation and specify e.g *-g -O2* for the *CFLAGS* switches.

*The GPS main window is not displayed*

  If when launching GPS, nothing happens, you can try to rename the :file:`.gps`
  directory (see :ref:`Files`) to start from a fresh set up.

*My project have several files with the same name. How can I import it in GPS?*

  GPS's projects do not allow implicit overriding of sources file, i.e.  you
  cannot have multiple times the same file name in the project hierarchy. The
  reason is that GPS needs to know exactly where the file is, and cannot
  reliably guess which occurrence to use.

  There are several solutions to handle this issue:

  *Put all duplicate files in the same project*

    There is one specific case where a project is allowed to have duplicate
    source files: if the list of source directories is specified explicitly.
    All duplicate files must be in the same project. With these conditions,
    there is no ambiguity for GPS and the GNAT tools which file to use, and the
    first file found on the source path is the one hiding all the others. GPS
    only shows the first file.

    You can then have a scenario variable that changes the order of source
    directories to give visibility on one of the other duplicate files.

  *Use scenario variables in the project*

    The idea is that you define various scenarios in your project (For instance
    compiling in "debug" mode or "production" mode), and change the source
    directories depending on this setup.  Such projects can be edited directly
    from GPS (in the project properties editor, this is the right part of the
    window, as described in this documentation). On top of the project view
    (left part of the GPS main window), you have a combo box displayed for each
    of the variable, allowing a simple switch between scenarios depending on
    what you want to build.

  *Use extending projects*

    These projects cannot currently be created through GPS, so you will need to
    edit them by hand. See the GNAT user's guide for more information on
    extending projects.

    The idea behind this approach is that you can have a local overriding of
    some source files from the common build/source setup (if you are working on
    a small part of the whole system, you may not want to have a complete copy
    of the code on your local machine).

*GPS is very slow compared to previous versions under unix (GPS < 4.0.0)*

  GPS versions 4.x need the X RENDER extension when running under unix
  systems to perform at a reasonable speed, so you need to make sure your X
  server properly supports this extension.

*Using the space key brings the smart completion window under Ubuntu*

  This is specific to the way GNOME is configured on Ubuntu distributions.  To
  address this incompatibility, close GPS, then go to the GNOME menu
  *System->Preferences->Keyboard* (or launch *gnome-keyboard-properties*).

  Select the *Layout* tab, click on *Layout Options*. Then click twice on
  *Using space key to input non-breakable space character* and then select
  *Usual space at any level* and then close the dialogs.

*GPS crashes on some GNU/Linux distributions at start up*

  Look at the :file:`~/.gps/log.xxx` file and if there is a message that
  looks like:

    [GPS.MAIN_WINDOW] 1/16 loading gps-animation.png
    [UNEXPECTED_EXCEPTION] 1/17 Unexpected exception: Exception name: CONSTRAINT_ERROR
    _UNEXPECTED_EXCEPTION_ Message: gtk-image.adb:281 access check failed

  Then it means there is a conflict with :file:`~/.local/share/mime/mime.cache`.
  Removing this file will solve this conflict.

