.. _Online_Help:

***********
Online Help
***********

.. index:: Online help
.. index:: help
.. index:: HTML

By default when you start GPS, the working area contains a welcome page giving
a few starting points in the online help.

Online help for the GNAT tools is available from the `Help` menu item.  GPS
launches an external html browser to view these pages. (See
:ref:`The_Preferences_Dialog` on how to configure this under Unix. Under
Windows systems, the default HTML browser is used.)

.. _The_Help_Menu:

The Help Menu
=============

The Help menu item provides the following entries:

*Welcome*
  Open the GPS Welcome page.

*Contents*
  Open a special HTML file that contains links for all the
  documentation files currently registered in GPS, :ref:`Adding_New_Help_Files`.

*GPS*
  Submenu containing GPS documentation items.

*GNAT Runtime*
  Submenu referencing all GNAT run-time files available, and a direct access
  to the corresponding specs containing embedded documentation.

*Python extensions*
  Gives access to the GPS API available via python.

*About*
  Display a dialog giving information about the versions of GPS and GNAT used:

  .. index:: screen shot
  .. image:: about.jpg


This menu contains a number of additional entries, depending on what
documentation packages were installed on your system. See the next section to
see how to add new help files.

.. _Adding_New_Help_Files:

Adding New Help Files
=====================

.. index:: gps_index.xml

GPS will search for the help files in the list of directories set in the
environment variable `GPS_DOC_PATH` (a colon-separated list of directories on
Unix systems, or semicolon-separated list of directories on Windows systems).
In addition, the default directory `<prefix>/share/doc/gps/html` is also
searched. If the file cannot be found in any of these directories, the
corresponding menu item will be disabled.

The environment variable `GPS_DOC_PATH` can either be set by each user in his
own environment, or can be set system-wide by modifying the small wrapper
script :file:`gps` itself on Unix systems.

It can also be set programmatically through the GPS shell or any of the
scripting languages. This is done with::

  GPS.add_doc_directory ("/home/foo")

The specific list of files shown in the menus is set by reading the index files
in each of the directories in `GPS_DOC_PATH`. These index files must be called
:file:`gps_index.xml`.

The format of these index files is specified in :ref:`Adding_documentation`.
