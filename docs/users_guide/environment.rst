.. _Environment:

***********
Environment
***********

.. index:: environment
.. index:: ! command line
.. index:: see: options; command line
.. _Command_Line_Options:

Command Line Options
====================

The command line options are::

  Usage:
     gps [options] [-Pproject-file] [[+line] source1] [[+line] source2] ...
  Options:
     --help                         Show this help message and exit
     --version                      Show the GPS version and exit
     --debug[=program]              Start a debug session and optionally load
                                    the program with the given arguments
     --debugger debugger            Specify the debugger's command line
     --hide                         Hide GPS main window
     --host=tools_host              Use tools_host to launch tools (e.g. gdb)
     --target=TARG:PRO              Load program on machine TARG using protocol
                                    PRO
     --load=lang:file               Execute an external file written in the
                                    language lang
     --eval=lang:file               Execute an in-line script written in the
                                    language lang
     -XVAR=VALUE                    Specify a value for a scenario variable
     --readonly                     Open all files in read-only mode
     --server=port                  Start GPS in server mode, opening a socket
                                    on the given port
     --tracelist                    Output the current configuration for logs
     --traceon=name                 Activate the logs for a given module
     --traceoff=name                Deactivate the logs for a given module
     --tracefile=file               Parse an alternate configuration file for
                                    the logs
     --config=file                  Specify the configuration file (.cgpr) to
                                    load
     --autoconf                     Generate .cgpr automatically if needed
     --configdb=dir                 Extra directories for gprconfig
     --ignore-saved-scenario-values Ignore the scenario values saved in .gps

Source files can be absolute or relative pathnames.
If you prepend a file name with '=', this file will be
searched anywhere on the project's source path

To open a file at a given line, use the :command`+line` prefix, e.g.
:command:`gps +40 source.adb`.

:samp:`tools_host` corresponds to a remote host's nickname as defined
in :ref:`Setup_the_remote_servers`.

By default, files you specify on the command line can have absolute or
relative pathnames.  If you prepend a filename with the :samp:`=`
character, GPS looks for the file in the source search path of the
project.  If you do not specify a project on the command line, GPS
tries to find one.  Otherwise, it displays the :ref:`welcome dialog
<The_Welcome_Dialog>`.


.. index:: environment
.. index:: environment variables
.. _Environment_Variables:

Environment Variables
=====================

You can set the following environment variables to override default
settings in GPS:

* :file:`GPS_HOME`

  .. index:: GPS_HOME
  .. index:: Windows

  Overrides the variable :command:`HOME` if present. All the
  configuration files and directories used by GPS are either relative
  to :file:`$HOME/.gps` (:file:`%HOME%\.gps` on Windows) if *GPS_HOME*
  is not set, or to :file:`$GPS_HOME/.gps` (respectively,
  :file:`%GPS_HOME%\.gps`) if set.

* :file:`GPS_DOC_PATH`

  .. index:: GPS_DOC_PATH

  Sets the search path for the documentation. See :ref:`Adding_Documentation`.

  If you installed GPS in a directory different from that of the GNAT
  compiler, you need to set this variable for GPS to find the documentation
  for GNAT. In the case of the compiler documentation, for example, the
  :file:`gps_index.xml` file installed with GPS assumes `GPS_DOC_PATH`
  points to the directory containing :file:`gnat_ugn.html`, so it should
  contain :file:`gnat_prefix/share/doc/gnat/html`.

* :file:`GPS_CUSTOM_PATH`

  .. index:: GPS_CUSTOM_PATH

  Contains a list of directories to search for custom files. See
  :ref:`Customizing_through_XML_and_Python_files` for more details.

* :file:`GPS_CHANGELOG_USER`

  .. index:: GPS_CHANGELOG_USER

  Contains the user and e-mail to use in the global ChangeLog files.  The
  convention is to have two spaces between the name and the e-mail, such as
  "John Does <john.doe@home.com>"

* :file:`GPS_STARTUP_PATH`

  .. index:: GPS_STARTUP_PATH

  Contains the value of the :command:`PATH` environment variable just
  before GPS was started.  GPS uses this to restore the proper
  environment before spawning applications independently of what
  directories it needs to put into its own path.

* :file:`GPS_STARTUP_LD_LIBRARY_PATH`

  .. index:: GPS_STARTUP_LD_LIBRARY_PATH

  Same as *GPS_STARTUP_LD_LIBRARY_PATH* but for the
  :command:`LD_LIBRARY_PATH` variable.

* :file:`GPS_PYTHONHOME`

  .. index:: GPS_PYTHONHOME

  If set, the Python interpreter looks for libraries in the subdirectory
  :file:`lib/python<version>` of the directory specified.

* :file:`GNAT_CODE_PAGE`

  .. index:: GNAT_CODE_PAGE

  You can set this variable to :samp:`CP_ACP` or :samp:`CP_UTF8`.
  It is used to control the code page used on Windows platform. The
  default is :samp:`CP_UTF8` (to support more languages).  If file or
  directory names are using accents, it may be necessary to set this
  variable to :samp:`CP_ACP` which is the default Windows ANSI code page.

* :file:`GPS_ROOT`

  .. index:: GPS_ROOT

  Overrides and hardcodes the default root installation directory.  You
  usually do not need to set this variable unless you are a GPS developer in
  unusual circumstances. GPS finds all its resource files (e.g., images,
  plugins, and xml files) from this variable, so setting it to an
  incorrect value will cause GPS to misbehave.

* :file:`GPS_MEMORY_MONITOR`

  .. index:: GPS_MEMORY_MONITOR

  If set, GPS adds special code on every allocation and deallocation
  to make it possible to check where the largest amount of memory is
  allocated using the :command:`GPS.debug_memory_usage` Python
  command.  Setting this variable will slow GPS down.

.. _Files:

Files
=====

* :file:`$HOME/.gps`

  .. index:: Windows
  .. index:: HOME

  GPS state directory. Defaults to :file:`C:\.gps` under Windows
  systems if the :command:`HOME` or :command:`USERPROFILE` environment
  variables are not defined.


.. _log_file:

* :file:`$HOME/.gps/log.txt`

  .. index:: log file

  Log file automatically created by GPS.  When GPS is running, it creates a
  file :file:`log.<pid>`, where :file:`<pid>` is the GPS process id, so
  multiple GPS sessions do not clobber each other's log. In case of a
  successful session, this file is renamed to :file:`log` when exiting; in
  case of an unexpected exit (when bug box is displayed) the log file
  retains its original name.  The name of the log file is configured by the
  :file:`traces.cfg` file.


* :file:`$HOME/.gps/aliases`

  .. index:: aliases

  File containing user-defined aliases (see :ref:`Defining_text_aliases`).

* :file:`$HOME/.gps/plug-ins`

  Directory containing files with user-defined plugins.  GPS loads all XML
  and Python files found under this directory during start up.  Create or
  edit these files to add your own menu and/or tool-bar entries in GPS or
  to define support for new languages.  See
  :ref:`Customizing_through_XML_and_Python_files` and
  :ref:`Adding_support_for_new_languages`.

* :file:`$HOME/.gps/key_themes/`

  Directory containing user defined key themes (XML files). These themes are
  loaded through the key shortcuts editor.

* :file:`$HOME/.gps/keys6.xml`

  Contains all key bindings for the actions defined in GPS or custom
  files. This file only contains the key bindings overridden through the
  key shortcuts editor (see :ref:`The_Key_Shortcuts_Editor`).



* :file:`$HOME/.gps/gps.css`

  .. index:: CSS

  Configuration and theme file for gtk. This file can change specific
  aspects of the look of GPS. Its contents overrides any other style
  information set by your default gtk+ theme (as selected in the Preferences
  dialog) and GPS's :file:`prefix/share/gps/gps.css` file.

* :file:`$HOME/.gps/perspectives6.xml`

  Desktop file in XML format (created using the :menuselection:`File -->
  Save More --> Desktop` menu).  It is loaded automatically if found.


* :file:`$HOME/.gps/locations.xml`

  List of locations GPS previously edited. It corresponds to the history
  navigation (:menuselection:`Navigate --> Back` and
  :menuselection:`Navigate --> Forward`) menus.


* :file:`$HOME/.gps/properties.db`

  Stores file-specific properties across GPS sessions. In particular, it
  contains the encoding to use for files where the default encoding is not
  appropriate.


* :file:`$HOME/.gps/histories.xml`

  .. index:: history

  Contains the state and history of combo boxes (for example, the
  :menuselection:`Build --> Run --> Custom...` dialog).


* :file:`$HOME/.gps/targets.xml`

  .. index:: targets

  Contains the build targets defined by the user.


* :file:`$HOME/.gps/preferences.xml`

  .. index:: preferences

  Contains all the preferences in XML format, as specified in the
  preferences menu.

* :file:`$HOME/.gps/traces.cfg`

  Default configuration for system traces. These traces are used to analyze
  problems with GPS.  By default, they are sent to the file
  :file:`$HOME/.gps/log.<pid>.txt`.

  This file is created automatically when the :file:`$HOME/.gps/` directory
  is created. If you remove it manually, it is not recreated the next time
  you start GPS.

* :file:`$HOME/.gps/startup.xml`

  List of scripts to load at startup as well as additional code that needs
  to be executed to set up the scripts.

* :file:`$HOME/.gps/activity_log.tmplt`

  Template file used to generate activities' group commit-log and patch
  file's header. If not present, the system wide template (see below) is
  used. The set of configurable tags are described into this template.

* :file:`prefix`

  Prefix directory where GPS is installed, e.g :file:`/opt/gps`.

* :file:`prefix/bin`

  Directory containing the GPS executables.

* :file:`prefix/etc/gps`

  Directory containing global configuration files for GPS.

* :file:`prefix/lib`

  Directory containing the shared libraries used by GPS.

* :file:`prefix/share/doc/gps/html`

  GPS looks for all the documentation files under this directory.

* :file:`prefix/share/examples/gps`

  Directory containing source code examples.

* :file:`prefix/share/examples/gps/language`

  Directory containing sources showing how to provide a shared library to
  dynamically define a new language. See
  :ref:`Adding_support_for_new_languages`.

* :file:`prefix/share/examples/gps/tutorial`

  Directory containing the sources used by the GPS tutorial.

  See `gps-tutorial.html <http://docs.adacore.com/gps-docs/tutorial/_build/html/>`_.

* :file:`prefix/share/gps/support`

  Directory containing required plugins for GPS that are automatically
  loaded at startup.

* :file:`prefix/share/gps/plug-ins`

  Directory containing files with system-wide plugins (XML and Python
  files) that are loaded automatically at start-up.

* :file:`prefix/share/gps/library`

  Directory containing files with system-wide plugins (XML and Python files)
  that are not loaded automatically at startup but can be selected in the
  :guilabel:`Plugins` section of the preferences editor dialog.

* :file:`prefix/share/gps/key_themes`

  Directory containing the predefined key themes (XML files). These can be
  loaded through the Key shortcuts editor.

* :file:`prefix/share/gps/gps-splash.png`

  Splash screen displayed by default when GPS is started.

* :file:`prefix/share/gps/perspectives6.xml`

  .. index:: default desktop
  .. index:: desktop, default

  Description of the default desktop that GPS uses when the user has not
  defined any default desktop and no project specific desktop exists.  You
  can modify this file if needed, but keep in mind that this will impact
  all users of GPS sharing this installation.  The format of this file is
  the same as :file:`$HOME/.gps/perspectives6.xml`, which can be copied
  from your own directory if desired.

* :file:`prefix/share/gps/default.gpr`

  .. index:: project; default project

  Default project used by GPS, which can be modified after installation to
  provide defaults for a given system or project.

* :file:`prefix/share/gps/readonly.gpr`

  Project used by GPS as the default project when working in a read-only
  directory.

* :file:`prefix/share/gps/activity_log.tmplt`

  Template file used by default to generate activities' group commit-log
  and patch file's header. This file can be copied into a user's home
  directory and customized (see above).

* :file:`prefix/share/locale`

  Directory used to retrieve the translation files, when relevant.

.. _Reporting_Suggestions_and_Bugs:

Reporting Suggestions and Bugs
==============================

.. index:: suggestions
.. index:: submitting bugs

If you would like to make suggestions about GPS or if you encounter a bug,
please send it to `mailto:report@adacore.com <mailto:report@adacore.com>`_
or use GNATtracker if you are a supported user and to
`mailto:gps-devel@lists.act-europe.fr <mailto:gps-devel@lists.act-europe.fr>`_
otherwise.

Please try to include a detailed description of the problem, including
sources to reproduce it if needed, and/or a scenario describing the actions
performed to reproduce the problem as well as listing all the tools (e.g
*debugger*, *compiler*, *call graph*) involved.

The files :file:`$HOME/.gps/log.txt` may also bring some useful information
when reporting a bug.

If GPS generates a bug box, the log file is kept under a separate name
(:file:`$HOME/.gps/log.<pid>.txt` so it does not get erased by further
sessions. Be sure to include the right log file when reporting a bug box.


Solving Problems
================

.. index:: problems
.. index:: solving problems

This section addresses some common problems that may arise when using or
installing GPS.

*GPS crashes on some GNU/Linux distributions at start up*

  Look at the :file:`~/.gps/log.<pid>.txt` file and if there is a message that
  looks like:

    [GPS.MAIN_WINDOW] 1/16 loading gps-animation.png
    [UNEXPECTED_EXCEPTION] 1/17 Unexpected exception: Exception name: CONSTRAINT_ERROR
    _UNEXPECTED_EXCEPTION_ Message: gtk-image.adb:281 access check failed

  it means either that there is a conflict with
  :file:`~/.local/share/mime/mime.cache`, in which case removing this file
  solves this conflict, or that you need to install the
  :command:`shared-mime-info` package on your system.

*Non-privileged users cannot start GPS*

  If you have originally installed GPS as root and can run GPS
  successfully, but normal users cannot, you should check the permissions of
  the directory :file:`$HOME/.gps` and its subdirectories: they should be
  owned by the user.

*GPS crashes whenever I open a source editor*

  This is usually due to font problems. Editing the file
  :file:`$HOME/.gps/preferences.xml` and changing the name of the fonts, e.g
  replacing *Courier* by *Courier Medium*, and *Helvetica* by *Sans* should
  solve the problem.

*GPS refuses to start the debugger*

  .. index:: debugger

  If GPS cannot properly initialize the debugger (using the
  :menuselection:`Debug --> Initialize` menu), it is usually because the
  underlying debugger (gdb) cannot be launched properly. To verify this is
  the problem, try to launch the :program:`gdb` command from a shell (i.e.,
  outside of GPS). If you cannot launch :program:`gdb` from a shell, it
  usually means you are using the wrong version of :program:`gdb` (e.g a
  version of :program:`gdb` built for Solaris 8 but run on Solaris 2.6).

*GPS is frozen during a debugging session*

  .. index:: debugger

  If GPS is no longer responding while debugging an application, you should
  wait a little longer, since some communications between GPS and
  :program:`gdb` can take significant time to finish. If GPS is still not
  responding after a few minutes, you can usually get control back in GPS
  by either typing :kbd:`Ctrl-C` in the shell where you have started GPS,
  which should unblock it. If that does not work, kill the :`program:`gdb`
  process launched by GPS using :program:`ps` and :program:`kill` or the
  :program:`top` command under Unix

  .. index:: Unix
  .. index:: Windows

  and the Tasks view under Windows. This will terminate your debugging
  session and will unblock GPS.

*My Ada program fails during elaboration. How can I debug it?*

  .. index:: GNAT; -g
  .. index:: gnatmake

  If your program was compiled with GNAT, the main program is generated by
  the binder. This program is an ordinary Ada (or C if the :command:`-C`
  switch was used) program, compiled in the usual manner, and fully
  debuggable provided the :command:`-g` switch is used on the
  :program:`gnatlink` command (or ;command:`-g` is used in the
  :program:`gnatmake` command).

  The name of the package containing the main program is
  :file:`b~xxx.ads/adb` where :samp:`xxx` is the name of the Ada main unit
  specified in the :program:`gnatbind` command.  Edit and debug this file
  in the usual manner. You will see a series of calls to the elaboration
  routines of packages.  Debug these in the usual manner, just as if you
  were debugging code in your application.

*How can I debug the Ada run-time library?*

  The run time distributed in binary versions of GNAT has not been compiled
  with debug information, so it needs to be recompiled before you can debug
  it.

  The simplest way is to recompile your application and add the switches
  :command:`-a` and :command:`-f` to the :program:`gnatmake` command
  line. This extra step is only required to be done once assuming you keep
  the generated object and :file:`ali` files corresponding to the GNAT run
  time available.

  Another possibility on Unix systems is to use the file
  :file:`Makefile.adalib`, which is found in the :file:`adalib` directory
  of your GNAT installation, and specify e.g :command:`-g -O2` for the
  :command:`CFLAGS` switches.

*The GPS main window is not displayed*

  If, when launching GPS, nothing happens, try to rename the :file:`.gps`
  directory (see :ref:`Files`) to start from a fresh set up.

*My project have several files with the same name. How can I import it in GPS?*

  GPS's projects do not allow implicit overriding of sources files, so you
  cannot have the same filename multiple times in the project
  hierarchy. This is because GPS needs to know exactly where the file is
  and cannot reliably guess which occurrence to use.

  There are several ways to handle this issue:

  *Put all duplicate files in the same project*

    There is one specific case where a project is allowed to have duplicate
    source files: if the list of source directories is specified
    explicitly.  All duplicate files must be in the same project. Under
    these conditions, there is no ambiguity for GPS and the GNAT tools as to
    which file to use and the first file found on the source path is the
    one hiding all the others. GPS only shows the first file.

    You can then have a scenario variable that changes the order of source
    directories to give visibility to one of the other duplicate files.

  *Use scenario variables in the project*

    Here, you define various scenarios in your project (for example
    compiling in "debug" mode or "production" mode) and change source
    directories depending on the scenario.  Such projects can be edited
    directly from GPS (in the project properties editor, on the right part
    of the window, as described in this documentation). On top of the
    :guilabel:`Project` view (left part of the GPS main window), a combo
    box is displayed for each variable, allowing you to switch between
    scenarios depending on what you want to build.

  *Use extended projects*

    These projects cannot currently be created through GPS, so you need to
    edit them by hand. See the GNAT User's guide for more information on
    extending projects.

    The idea behind this approach is that you can have a local overriding
    of some source files from the common build/source setup (e.g., if
    you are working on a small part of the whole system, you may not want to
    have a complete copy of the code on your local machine).

*GPS is very slow compared to previous versions under Unix (GPS < 4.0.0)*

  GPS versions 4.x need the X RENDER extension when running under Unix
  systems to perform at a reasonable speed, so you need to make sure your X
  server properly supports this extension.

*Using the space key brings the smart completion window under Ubuntu*

  This is specific to the way GNOME is configured on Ubuntu distributions.
  To address this incompatibility, close GPS, then go to the GNOME menu
  :menuselect`System->Preferences->Keyboard` (or launch :program:
  `gnome-keyboard-properties`).

  Select the :guilabel:`Layout` tab and click on :guilabel:`Layout
  Options`. Then click twice on :guilabel:`Using space key to input
  non-breakable space character`, select :guilabel:`Usual space at any
  level`, and then close the dialogs.

*File associations or icons disappear or misbehave under Windows*

  Sometimes file associations get redefined under Windows and no longer
  behave as a GPS user expects (for example, Ada source files become
  associated with a stock file icon or double-clicking on a project file
  opens it like a regular text file.) You may be able to restore the
  expected behavior by reapplying the associations performed during GPS
  installation.  To do this, locate the file
  :file:`registry-gps-{version}.reg` in the root of your GPS installation,
  and double-click it.  Then confirm that you want to apply it in the
  dialog that appears.

*Copy/Paste operations crash GPS running on a forwarded X11 display*

  It is possible to run GPS on a remote machine using the X11 display
  forwarding feature of :command:`ssh`. But a copy/paste operation could
  cause GPS to crash if untrusted forwarding (:command:`ssh -X`) is used.
  Use the :command:`ssh -Y` option or the ForwardX11Trusted directive in
  ssh_config to use trusted X11 forwarding and avoid the GPS crash.

*Working with Xming*

  Some old versions of Xming (such as 6.9.0.31) have an issue in that they create
  "transient" windows larger than the application requests, and do not allow
  the user to resize these windows. To circumvent this, we have added a command line switch
  to tell GPS not to store the window sizes and positions: activate this
  by launching GPS with :command:`--traceoff=STORE_WINDOW_POSITIONS`.

*Buttons placed in dialogs' header bars or missing with GNOME 3.12+*

  GNOME 3.12+ override the Gtk settings set by GPS, including the
  ‘DialogsUseHeaders’ setting, which has for effect to display the buttons at
  the top of dialogs. This leads to some problems with GPS and sometimes some
  buttons are missing on some dialogs (e.g: Add button in the Aliases editor).
  You can run this command from the terminal to force GNOME to disable this
  setting:
  :command:`gsettings set org.gnome.settings-daemon.plugins.xsettings overrides
  "{'Gtk/DialogsUseHeader':<0>}"`

*Floating windows are openened in fullscreen on MacOS Sierra*

  On MacOS Sierra, when GPS is in fullscreen, all the floating windows opened
  from GPS (e.g: :menuselection:`Edit --> Preferences...`) are opened in
  fullscreen too by default.
  This behavior can be disabled by setting the
  :guilabel:`Prefer tabs when opening documents` to :guilabel:`Manually` in the
  :guilabel:`Dock` section of the MacOS Sierra's System Preferences.
