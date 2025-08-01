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
     gnatstudio [options] [-Pproject-file] [[+line] source1] [[+line] source2] ...
  Options:
     --help                         Show this help message and exit
     --version                      Show the GNAT Studio version and exit
     --debug[=program]              Start a debug session and optionally load
                                    the program with the given arguments
     --hide                         Hide GNAT Studio main window
     --target=TARG:PRO              Load program on machine TARG using protocol
                                    PRO
     --load=lang:file               Execute an external file written in the
                                    language lang
     --eval=lang:file               Execute an in-line script written in the
                                    language lang
     -XVAR=VALUE                    Specify a value for a scenario variable
     --readonly                     Open all files in read-only mode
     --server=port                  Start GNAT Studio in server mode, opening a
                                    socket on the given port
     --tracelist                    Output the current configuration for logs
     --traceon=name                 Activate the logs for a given module
     --traceoff=name                Deactivate the logs for a given module
     --tracefile=file               Parse an alternate configuration file for
                                    the logs
     --config=file                  Specify the configuration file (.cgpr) to
                                    load
     --autoconf                     Generate .cgpr automatically if needed
     --configdb=dir                 Extra directories for gprconfig
     --relocate-build-tree          Relocate build directories for the current
                                    project
     --root-dir                     Root directory for the current project:
                                    must be used with relocate-build-tree
     --ignore-saved-scenario-values Ignore the scenario values saved in .gnatstudio

Source files can be absolute or relative pathnames.
If you prepend a file name with '=', this file will be
searched anywhere on the project's source path

To open a file at a given line, use the :command`+line` prefix, e.g.
:command:`gnatstudio +40 source.adb`.

By default, files you specify on the command line can have absolute or
relative pathnames.  If you prepend a filename with the :samp:`=`
character, GNAT Studio looks for the file in the source search path of the
project.  If you do not specify a project on the command line, GNAT Studio
tries to find one.  Otherwise, it displays the :ref:`welcome dialog
<The_Welcome_Dialog>`.


.. index:: environment
.. index:: environment variables
.. _Environment_Variables:

Environment Variables
=====================

You can set the following environment variables to override default
settings in GNAT Studio:

* :file:`GNATSTUDIO_HOME`

  .. index:: GNATSTUDIO_HOME
  .. index:: Windows

  Overrides the variable :command:`HOME` if present. All the
  configuration files and directories used by GPS are either relative
  to :file:`$HOME/.gnatstudio` (:file:`%HOME%\.gnatstudio` on Windows) if
  *GNATSTUDIO_HOME* is not set, or to :file:`$GNATSTUDIO_HOME/.gnatstudio`
  (respectively, :file:`%GNATSTUDIO_HOME%\.gnatstudio`) if set.

* :file:`GNATSTUDIO_DOC_PATH`

  .. index:: GNATSTUDIO_DOC_PATH

  Sets the search path for the documentation. See :ref:`Adding_Documentation`.

  If you installed GNAT Studio in a directory different from that of the GNAT
  compiler, you need to set this variable for GNAT Studio to find the
  documentation for GNAT. In the case of the compiler documentation,
  for example, the :file:`gnatstudio_index.xml` file installed with GNAT Studio
  assumes `GNATSTUDIO_DOC_PATH` points to the directory containing
  :file:`gnat_ugn.html`, so it should contain :file:`gnat_prefix/share/doc/gnat/html`.

* :file:`GNATSTUDIO_CUSTOM_PATH`

  .. index:: GNATSTUDIO_CUSTOM_PATH

  Contains a list of directories to search for custom files. See
  :ref:`Customizing_through_XML_and_Python_files` for more details.

  Can be used to redefine the default project created via
  `Start with default project` startup menu entry, by adding a directory
  containing a new project :file:`default.gpr`.

* :file:`GNATSTUDIO_CHANGELOG_USER`

  .. index:: GNATSTUDIO_CHANGELOG_USER

  Contains the user and e-mail to use in the global ChangeLog files.  The
  convention is to have two spaces between the name and the e-mail, such as
  "John Does <john.doe@home.com>"

* :file:`GNAT_CODE_PAGE`

  .. index:: GNAT_CODE_PAGE

  You can set this variable to :samp:`CP_ACP` or :samp:`CP_UTF8`.
  It is used to control the code page used on Windows platform. The
  default is :samp:`CP_UTF8` (to support more languages).  If file or
  directory names are using accents, it may be necessary to set this
  variable to :samp:`CP_ACP` which is the default Windows ANSI code page.

* :file:`GPS_MEMORY_MONITOR`

  .. index:: GPS_MEMORY_MONITOR

  If set, GPS adds special code on every allocation and deallocation
  to make it possible to check where the largest amount of memory is
  allocated using the :command:`GPS.debug_memory_usage` Python
  command.  Setting this variable will slow GPS down.

* :file:`GPR_CONFIG`

  .. index:: GPR_CONFIG

  If set, this variable points to the configuration file (:samp:`.cgpr`).
  The command line :samp:`--config` option takes precedence over the
  environment variable.

Note that, for backwards compatibility purposes, for all variables of the
form :file:`GNATSTUDIO_<something>`, if this variable is not defined but
the variable :file`GPS_<something>` is, then the value for that one will
be used instead.

.. _Files:

Files
=====

* :file:`$HOME/.gnatstudio`

  .. index:: Windows
  .. index:: HOME

  GNAT Studio state directory. Defaults to :file:`C:\.gnatstudio` under Windows
  systems if the :command:`HOME` or :command:`USERPROFILE` environment
  variables are not defined.


.. _log_file:

* :file:`$HOME/.gnatstudio/log/log.<timestamp>.txt``

  .. index:: log file

  Log file automatically created by GNAT Studio.  When GNAT Studio is running,
  it creates a file :file:`log.<timestamp>.txt` in the `$HOME/.gnatstudio/log/` directory
  (`%USERPROFILE%\.gnatstudio\log` directory on Windows), where :file:`<timestamp>`
  is the GNAT Studio process timestamp, so multiple GNAT Studio sessions do not clobber each
  other's log.
  The name of the log file is configured by the :file:`traces.cfg` file.


* :file:`$HOME/.gnatstudio/aliases`

  .. index:: aliases

  File containing user-defined aliases (see :ref:`Defining_text_aliases`).

* :file:`$HOME/.gnatstudio/plug-ins`

  Directory containing files with user-defined plugins.  GNAT Studio loads all
  XML and Python files found under this directory during start up.  Create or
  edit these files to add your own menu and/or tool-bar entries in GNAT Studio
  or to define support for new languages.  See
  :ref:`Customizing_through_XML_and_Python_files` and
  :ref:`Adding_support_for_new_languages`.

* :file:`$HOME/.gnatstudio/key_themes/`

  Directory containing user defined key themes (XML files). These themes are
  loaded through the key shortcuts editor.

* :file:`$HOME/.gnatstudio/keys.xml`

  Contains all key bindings for the actions defined in GNAT Studio or custom
  files. This file only contains the key bindings overridden through the
  key shortcuts editor (see :ref:`The_Key_Shortcuts_Editor`).


* :file:`$HOME/.gnatstudio/gps.css`

  .. index:: CSS

  Configuration and theme file for gtk. This file can change specific
  aspects of the look of GNAT Studio. Its contents overrides any other style
  information set by your default gtk+ theme (as selected in the Preferences
  dialog) and GNAT Studio's :file:`prefix/share/gnatstudio/gps.css` file.

* :file:`$HOME/.gnatstudio/perspectives6.xml`

  Desktop file in XML format (created using the :menuselection:`File -->
  Save More --> Desktop` menu).  It is loaded automatically if found.


* :file:`$HOME/.gnatstudio/locations.xml`

  List of locations GNAT Studio previously edited. It corresponds to the
  history navigation (:menuselection:`Navigate --> Back` and
  :menuselection:`Navigate --> Forward`) menus.


* :file:`$HOME/.gnatstudio/properties.db`

  Stores file-specific properties across GNAT Studio sessions. In particular, it
  contains the encoding to use for files where the default encoding is not
  appropriate.


* :file:`$HOME/.gnatstudio/histories.xml`

  .. index:: history

  Contains the state and history of combo boxes (for example, the
  :menuselection:`Build --> Run --> Custom...` dialog).


* :file:`$HOME/.gnatstudio/targets.xml`

  .. index:: targets

  Contains the build targets defined by the user.


* :file:`$HOME/.gnatstudio/preferences.xml`

  .. index:: preferences

  Contains all the preferences in XML format, as specified in the
  preferences menu.

* :file:`$HOME/.gnatstudio/traces.cfg`

  Default configuration for system traces. These traces are used to analyze
  problems with GNAT Studio.  By default, they are sent to the file
  :file:`$HOME/.gnatstudio/log/log.<timestamp>.txt`.

  This file is created automatically when the :file:`$HOME/.gnatstudio/` directory
  is created. If you remove it manually, it is not recreated the next time
  you start GNAT Studio.

* :file:`$HOME/.gnatstudio/startup.xml`

  List of scripts to load at startup as well as additional code that needs
  to be executed to set up the scripts.

* :file:`$HOME/.gnatstudio/activity_log.tmplt`

  Template file used to generate activities' group commit-log and patch
  file's header. If not present, the system wide template (see below) is
  used. The set of configurable tags are described into this template.

* :file:`prefix`

  Prefix directory where GNAT Studio is installed, e.g :file:`/opt/gnatstudio`.

* :file:`prefix/bin`

  Directory containing the GNAT Studio executables.

* :file:`prefix/etc/gnatstudio`

  Directory containing global configuration files for GNAT Studio.

* :file:`prefix/lib`

  Directory containing the shared libraries used by GPS.

* :file:`prefix/share/doc/gnatstudio/html`

  GNAT Studio looks for all the documentation files under this directory.

* :file:`prefix/share/examples/gnatstudio`

  Directory containing source code examples.

* :file:`prefix/share/examples/gnatstudio/language`

  Directory containing sources showing how to provide a shared library to
  dynamically define a new language. See
  :ref:`Adding_support_for_new_languages`.

* :file:`prefix/share/examples/gnatstudio/tutorial`

  Directory containing the sources used by the GNAT Studio tutorial.

  See `gps-tutorial.html <http://docs.adacore.com/gps-docs/tutorial/_build/html/>`_.

* :file:`prefix/share/gnatstudio/support`

  Directory containing required plugins for GNAT Studio that are automatically
  loaded at startup.

* :file:`prefix/share/gnatstudio/plug-ins`

  Directory containing files with system-wide plugins (XML and Python
  files) that are loaded automatically at start-up.

* :file:`prefix/share/gnatstudio/library`

  Directory containing files with system-wide plugins (XML and Python files)
  that are not loaded automatically at startup but can be selected in the
  :guilabel:`Plugins` section of the preferences editor dialog.

* :file:`prefix/share/gnatstudio/key_themes`

  Directory containing the predefined key themes (XML files). These can be
  loaded through the Key shortcuts editor.

* :file:`prefix/share/gnatstudio/gnatstudio-splash.png`

  Splash screen displayed by default when GNAT Studio is started.

* :file:`prefix/share/gnatstudio/perspectives6.xml`

  .. index:: default desktop
  .. index:: desktop, default

  Description of the default desktop that GNAT Studio uses when the user has
  not defined any default desktop and no project specific desktop exists.  You
  can modify this file if needed, but keep in mind that this will impact
  all users of GNAT Studio sharing this installation.  The format of this file
  is the same as :file:`$HOME/.gnatstudio/perspectives6.xml`, which can be
  copied from your own directory if desired.

* :file:`prefix/share/gnatstudio/default.gpr`

  .. index:: project; default project

  Default project used by GNAT Studio, which can be modified after installation
  to provide defaults for a given system or project.

* :file:`prefix/share/gnatstudio/readonly.gpr`

  Project used by GNAT Studio as the default project when working in a
  read-only directory.

* :file:`prefix/share/gnatstudio/activity_log.tmplt`

  Template file used by default to generate activities' group commit-log
  and patch file's header. This file can be copied into a user's home
  directory and customized (see above).

* :file:`prefix/share/locale`

  Directory used to retrieve the translation files, when relevant.


.. _The_Ada_Language_Server:

The Ada Language Server
=======================

.. index:: language server

GNAT Studio relies on an external process, acting as a server, for code
intelligence on Ada and SPARK.

The process for this server is called :file:`ada_language_server`
(:file:`ada_language_server.exe` under Windows). It is launched automatically
when GNAT Studio starts, and is terminated by GNAT Studio upon exit. In case of
crash, it's possible that the termination fails; in this case, feel free to
kill any stray :file:`ada_language_server` process which does not seem
associated to a running GNAT Studio session.

One known limitation of this server is that it doesn't support file paths
that are not valid UTF-8.

Activating traces for the Ada Language Server
---------------------------------------------

Each session of the Ada Language Server has its own log file - these are
stored in the :file:`.gnatstudio/log` directory, with the prefix :file:`ada_ls`.

You can configure these traces via the file :file:`.gnatstudio/ada_ls_traces.cfg`.
In particular, you can add these lines to the configuration file::

   ALS.IN=yes
   ALS.OUT=yes

This will cause all requests sent to the server and all output emitted
by the server to be captured in the log for the Ada Language Server.

.. _Reporting_Suggestions_and_Bugs:

Reporting Suggestions and Bugs
==============================

.. index:: suggestions
.. index:: submitting bugs

If you would like to make suggestions about GNAT Studio or if you encounter a
bug, please send it to `mailto:support@adacore.com <mailto:support@adacore.com>`_
or use GNATtracker if you are a supported user.

Please try to include a detailed description of the problem, including
sources to reproduce it if needed, and/or a scenario describing the actions
performed to reproduce the problem as well as listing all the tools (e.g
*debugger*, *compiler*, *call trees*) involved.

The files :file:`$HOME/.gnatstudio/log.<timestamp>.txt` (GNAT Studio log file) and
:file:`$HOME/.gnatstudio/ada_ls_log.<timestamp>.txt` (Ada Language Server log file) may
also bring some useful information when reporting a bug. You can open them directly
in GNAT Studio respectively via the :menuselection:`Help --> Open GNAT Studio Log File`
and :menuselection:`Help --> Open Ada Language Server Log File` menus.

Note that you can use the :menuselection:`Help --> Create Bug Report` menu to help
you creating a bug report archive (tar.gz archive file) that includes all the information
you can provide.

.. image:: bug_report_dialog.png

If GNAT Studio generates a bug box, the log file is kept under a separate name
(:file:`$HOME/.gnatstudio/log.<timestamp>.txt` so it does not get erased by further
sessions. Be sure to include the right log file when reporting a bug box.


System package dependencies
===========================

On Linux, GNAT Studio relies on packages provided by the system. All of them
are installed by default, however, sometimes manual installation may be
required. Below, you will find the list of packages which may require manual
installation, organized by distribution.

*Red Hat Enterprise Linux 7 and 8*

For both versions following packages need to be installed:

 * `glibc`
 * `libX11`
 * `libXau`
 * `libXcomposite`
 * `libXcursor`
 * `libXdamage`
 * `libXext`
 * `libXfixes`
 * `libXi`
 * `libXinerama`
 * `libXrandr`
 * `libXrender`
 * `libuuid`
 * `libxcb`
 * `nss-softokn-freebl`
 * `shared-mime-info`

*SuSE Linux Enterprise Server 12 and 15*

 * `glibc`
 * `libX11-6`
 * `libXau6`
 * `libXcomposite1`
 * `libXcursor1`
 * `libXdamage1`
 * `libXext6`
 * `libXfixes3`
 * `libXi6`
 * `libXinerama1`
 * `libXrandr1`
 * `libXrender1`
 * `libuuid1`
 * `libxcb-render0`
 * `libxcb-shm0`
 * `libxcb1`
 * `shared-mime-info`

*Ubuntu 18.04 LTS, 20.04 LTS, 22.04 LTS*

 * `libbsd0`
 * `libc6`
 * `libcrypt1` (20.04 only)
 * `libuuid1`
 * `libx11-6`
 * `libxau6`
 * `libxcb1`
 * `libx11-xcb1` (22.04 only)
 * `libxcb-render0`
 * `libxcb-shm0`
 * `libxcomposite1`
 * `libxcursor1`
 * `libxdamage1`
 * `libxdmcp6`
 * `libxfixes3`
 * `libxext6`
 * `libxi6`
 * `libxinerama1`
 * `libxrandr2`
 * `libxrender1`
 * `shared-mime-info`


Solving Problems
================

.. index:: problems
.. index:: solving problems

This section addresses some common problems that may arise when using or
installing GNAT Studio.

*GNAT Studio crashes on some GNU/Linux distributions at start up*

  Look at the :file:`$HOME/.gnatstudio/log.<timestamp>.txt` file and if there is a message that
  looks like:

    [GPS.MAIN_WINDOW] 1/16 loading gps-animation.png
    [UNEXPECTED_EXCEPTION] 1/17 Unexpected exception: Exception name: CONSTRAINT_ERROR
    _UNEXPECTED_EXCEPTION_ Message: gtk-image.adb:281 access check failed

  it means either that there is a conflict with
  :file:`~/.local/share/mime/mime.cache`, in which case removing this file
  solves this conflict, or that you need to install the
  :command:`shared-mime-info` package on your system.

*GNAT Studio crashes on Windows at startup*

  Look at the :file:`gnatstudio_error_log.txt` file in the
  Windows :file:`%TEMP%` directory: the exceptions that led to the crash will
  be reported there.

*Non-privileged users cannot start GNAT Studio*

  If you have originally installed GNAT Studio as root and can run GNAT Studio
  successfully, but normal users cannot, you should check the permissions of
  the directory :file:`$HOME/.gnatstudio` and its subdirectories: they should be
  owned by the user.

*GNAT Studio crashes whenever I open a source editor*

  This is usually due to font problems. Editing the file
  :file:`$HOME/.gnatstudio/preferences.xml` and changing the name of the fonts, e.g
  replacing *Courier* by *Courier Medium*, and *Helvetica* by *Sans* should
  solve the problem.

*The signature help window can't be moved around*

Some window managers do not allow popup window such as the signature
help window (i.e: the window that popups when writing a subprogram call)
to be dragged and moved around.
If it's the case with your window manager, you can try to enable the
**GPS.LSP.SIGNATURE_HELP.USE_TOPLEVEL** trace to solve this issue.
Note that using a toplevel window instead might decorate the
signature help window on some window managers.

*GNAT Studio refuses to start the debugger*

  .. index:: debugger

  If GNAT Studio cannot properly initialize the debugger (using the
  :menuselection:`Debug --> Initialize` menu), it is usually because the
  underlying debugger (gdb) cannot be launched properly. To verify this is
  the problem, try to launch the :program:`gdb` command from a shell (i.e.,
  outside of GNAT Studio). If you cannot launch :program:`gdb` from a shell, it
  usually means you are using the wrong version of :program:`gdb` (e.g a
  version of :program:`gdb` built for Solaris 8 but run on Solaris 2.6).

*GNAT Studio is frozen during a debugging session*

  .. index:: debugger

  If GNAT Studio is no longer responding while debugging an application, you
  should wait a little longer, since some communications between GNAT Studio and
  :program:`gdb` can take significant time to finish. If GNAT Studio is still
  not responding after a few minutes, you can usually get control back in
  GNAT Studio by either typing :kbd:`Ctrl-C` in the shell where you have started
  GNAT Studio, which should unblock it. If that does not work, kill the :`program:`gdb`
  process launched by GNAT Studio using :program:`ps` and :program:`kill` or the
  :program:`top` command under Unix

  .. index:: Unix
  .. index:: Windows

  and the Tasks view under Windows. This will terminate your debugging
  session and will unblock GNAT Studio.

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

*The GNAT Studio main window is not displayed*

  If, when launching GNAT Studio, nothing happens, try to rename the
  :file:`.gnatstudio` directory (see :ref:`Files`) to start from a fresh set up.

*My project have several files with the same name. How can I import it in GNAT Studio?*

  GNAT Studio's projects do not allow implicit overriding of sources files,
  so you cannot have the same filename multiple times in the project
  hierarchy. This is because GNAT Studio needs to know exactly where the file
  is and cannot reliably guess which occurrence to use.

  There are several ways to handle this issue:

  *Put all duplicate files in the same project*

    There is one specific case where a project is allowed to have duplicate
    source files: if the list of source directories is specified
    explicitly.  All duplicate files must be in the same project. Under
    these conditions, there is no ambiguity for GNAT Studio and the GNAT tools
    as to which file to use and the first file found on the source path is the
    one hiding all the others. GNAT Studio only shows the first file.

    You can then have a scenario variable that changes the order of source
    directories to give visibility to one of the other duplicate files.

  *Use scenario variables in the project*

    Here, you define various scenarios in your project (for example
    compiling in "debug" mode or "production" mode) and change source
    directories depending on the scenario.  Such projects can be edited
    directly from GNAT Studio (in the project properties editor, on the right
    part of the window, as described in this documentation). On top of the
    :guilabel:`Project` view (left part of the GNAT Studio main window), a combo
    box is displayed for each variable, allowing you to switch between
    scenarios depending on what you want to build.

  *Use extended projects*

    These projects cannot currently be created through GNAT Studio, so you need
    to edit them by hand. See the GNAT User's guide for more information on
    extending projects.

    The idea behind this approach is that you can have a local overriding
    of some source files from the common build/source setup (e.g., if
    you are working on a small part of the whole system, you may not want to
    have a complete copy of the code on your local machine).

*Using the space key brings the smart completion window under Ubuntu*

  This is specific to the way GNOME is configured on Ubuntu distributions.
  To address this incompatibility, close GNAT Studio, then go to the GNOME menu
  :menuselection:`System->Preferences->Keyboard` (or launch :program:
  `gnome-keyboard-properties`).

  Select the :guilabel:`Layout` tab and click on :guilabel:`Layout
  Options`. Then click twice on :guilabel:`Using space key to input
  non-breakable space character`, select :guilabel:`Usual space at any
  level`, and then close the dialogs.

*File associations or icons disappear or misbehave under Windows*

  Sometimes file associations get redefined under Windows and no longer
  behave as a GNAT Studio user expects (for example, Ada source files become
  associated with a stock file icon or double-clicking on a project file
  opens it like a regular text file.) You may be able to restore the
  expected behavior by reapplying the associations performed during GNAT Studio
  installation.  To do this, locate the file
  :file:`registry-gps-{version}.reg` in the root of your GNAT Studio
  installation, and double-click it.  Then confirm that you want to apply it in
  the dialog that appears.

*Copy/Paste operations crash GNAT Studio running on a forwarded X11 display*

  It is possible to run GNAT Studio on a remote machine using the X11 display
  forwarding feature of :command:`ssh`. But a copy/paste operation could
  cause GNAT Studio to crash if untrusted forwarding (:command:`ssh -X`) is
  used. Use the :command:`ssh -Y` option or the ForwardX11Trusted directive in
  ssh_config to use trusted X11 forwarding and avoid the GNAT Studio crash.

*Working with Xming*

  Some old versions of Xming (such as 6.9.0.31) have an issue in that they create
  "transient" windows larger than the application requests, and do not allow
  the user to resize these windows. To circumvent this, we have added a command line switch
  to tell GNAT Studio not to store the window sizes and positions: activate this
  by launching GNAT Studio with :command:`--traceoff=STORE_WINDOW_POSITIONS`.

*Buttons placed in dialogs' header bars or missing with GNOME 3.12+*

  GNOME 3.12+ override the Gtk settings set by GNAT Studio, including the
  'DialogsUseHeaders' setting, which has for effect to display the
  buttons at the top of dialogs. This leads to some problems with GNAT Studio
  and sometimes some buttons are missing on some dialogs (e.g: Add button in
  the Aliases editor). You can run this command from the terminal to force
  GNOME to disable this setting:
  :command:`gsettings set org.gnome.settings-daemon.plugins.xsettings overrides
  "{'Gtk/DialogsUseHeader':<0>}"`

*Floating windows are openened in fullscreen on MacOS Sierra*

  On MacOS Sierra, when GNAT Studio is in fullscreen, all the floating windows
  opened from GNAT Studio (e.g: :menuselection:`Edit --> Preferences...`) are
  opened in fullscreen too by default.
  This behavior can be disabled by setting the
  :guilabel:`Prefer tabs when opening documents` to :guilabel:`Manually` in the
  :guilabel:`Dock` section of the MacOS Sierra's System Preferences.

*GNAT Studio crashes when modifying a local preference with Cygwin Window Manager*

  The Cygwin Window Manager incorreclty reacts when modifying a local
  preference with its tooltip visible. To prevent this issue you can enable
  the trace GPS.INTERNAL.CYGWIN_WINDOW_MANAGER (using --traceon on the command
  line or a config file). The related tooltips will not be shown anymore.

*GNAT Studio crashes when opening a selector dialog on Windows*

  There is a known incompatibility between Windows and GTK_FILE_SELECTOR.
  Launching GNAT Studio with :command:`--traceoff=GPS.INTERNAL.GTK_FILE_SELECTOR`
  will allow GNAT Studio to use a selector dialog compatible with Windows.
  In most cases, you don't need to disable this trace if the preference
  "Use Native Dialogs" is enabled.
  Another workaround is to close GNAT Studio, to remove the file
  %USERPROFILE%\AppData\Local\gtk-3.0\bookmarks and to restart GNAT Studio.
