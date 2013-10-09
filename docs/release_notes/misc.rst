Other enhancements worth noting
-------------------------------

Project wizard uses gnatname
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-J728-022 GPS: the Project Wizard takes advantage of gnatname (2012-11-08)

The project wizard now optionally uses gnatname to search for Ada units in
files with non-standard naming conventions.

Codefix improvements
~~~~~~~~~~~~~~~~~~~~

..  NF-60-M109-036 GPS: autofix on missing constant with preferences (2013-01-21)

Autofix has always done fixes in lowercases. This enhancement performs the
fixes on missing constants following the user defined preferences specified in
the preference :menuselection:`Editor-->Ada/Reserved` word casing.

Preferences
~~~~~~~~~~~

..  NF-60-M617-030 GPS: apply preferences on the fly (2013-06-17)

The Apply button was removed from the preferences dialog.  Instead, GPS will be
refreshed every time any of the settings is modified.

..  NF-60-M530-024 GPS: only save changed preferences (2013-07-04)

GPS will now only save those preferences that have been modified into its
:file:`HOME/.gps/preferences.xml` file. This change should be mostly invisible
to users, except when changing versions of GPS where the new default will be
picked up automatically.

.. NF-60-MA02-018 GPS: change current line color in debugger (2013-10-03)

A new preference has been added to control the color of the current line
in the debugger. This is in particular useful when combined with dark
themes.

.. NF-60-M923-010 GPS: Searching for documentation after declaration (2013-09-23)

A new preference :menuselection:`Documentation-->Leading Comments` was added to
control the order in which GPS looks at the comments (before or after the
declaration) to retrieve the documentation for an entity. This impacts the
tooltips displayed while in the editor, as well as the generation of
documentation via the :menuselection:`Tools-->Documentation` menu.

..  NF-60-M816-002 GPS: display text only in the toolbar (2013-08-19)

Another preference was added to set the toolbar style to "text only".

..  NF-60-M716-040 GPS: new preference to override tooltip colors (2013-07-31)

A new preference was added in the :guilabel:`Windows` page to override the
background of tooltips as set by the gtk+ theme.

Debugger
~~~~~~~~

..  NF-60-M913-009 GPS: persistent contents of Debug/Connect To Board (2013-09-13)

The contents of the dialog to connect to a board (target name and protocol)
is now restored every time the dialog is displayed, avoiding the need to
enter the same information every time.

Command line
~~~~~~~~~~~~

..  NF-60-M806-037 GPS: new command line -X (2013-08-22)

This new switch is similar to -X in gprbuid, and can be used to set the value
of the scenario variables found in your project.
