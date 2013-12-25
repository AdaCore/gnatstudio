.. _Searching_and_Replacing:

***********************
Searching and Replacing
***********************

GPS provides extensive search capabilities. For example, you can search in
the currently edited source file or in all the source files belonging to
the project, even those that are not currently open. You can also search in
the project view (on the left side of the main GPS window).

.. index:: project view
.. index:: search context
.. index:: menu; navigate --> find or replace

All these search contexts are grouped into a single graphical window that
you can open either through the menu :menuselection:`Navigate --> Find or
Replace...`, or the shortcut :kbd:`Ctrl-F`.

By default, the search window is floating and appears as a dialog on top of
GPS. You can put it inside the multiple document interface permanently for
easier access by selecting the menu :menuselection:`Window --> Floating`,
and then dropping the search window in a new location (for example, above
the Project View).

Selecting either of these two options pops up a dialog on the screen,
similar to the following:

.. image:: search-hide.jpg

This dialog contains three fields:

*Search for*
  .. index:: search for

  Where you type the string or pattern to search for. The search widget
  supports two modes, either strings or regular expressions. You can toggle
  between the two modes either by clicking on the :guilabel:`Options`
  button and selecting the appropriate check box or by opening the combo
  box (click on the arrow on the right of the entry field).

  The combo box provides a number of predefined patterns. The top two are
  empty patterns that automatically set up the appropriate fixed strings or
  regular expression mode. The other regular expressions are
  language-specific and will match patterns such as Ada type definitions or
  C++ method declarations.

  .. index:: C++
  .. index:: Ada

*Replace with*
  .. index:: replace with

  Contains the string to replace the occurrences of the pattern defined
  above.  The combo box provides a history of previously used replacement
  strings. If a regular expression is used for search, special escapes
  `\1`, `\2` .. `\9` in this field refer to the corresponding matching
  subexpressions and \0 refers whole matched string.

*Look in*
  .. index:: look in

  The context in which the search should occur.

  .. index:: search context

  GPS automatically selects the most appropriate context when you open the
  search dialog, depending on which component currently has the focus. If
  several contexts are possible for one component (for example, the editor
  has :guilabel:`Current_File`, :guilabel:`Files from Project`,
  :guilabel:`Files...` and :guilabel:`Open Files`) then the last one you've
  been using will be selected. You can change the context to another one if
  needed, by clicking on the arrow on the right. It displays the list of
  all possible contexts, inluding:

  *Project View*
    Search in the project view. An extra *Scope* box is displayed where you
    can specify the scope of your search, which is a set of: `Projects,
    Directories, Files, Entities`. The search in entities may take a long
    time since each file is parsed during the search.


  *Open Files*
    Search in all the files currently open in the source editor. The
    *Scope* entry is described in the `Files...` section below.

  *Files...*

    Search in a given set of files. An extra *Files* box is displayed where
    you specify the files using standard shell (Unix or Windows) regular
    expressions such as :file:`*.ad?` for all files ending with .ad and any
    trailing character. The directory specifies where the search starts and
    the *Recursive search* button whether subdirectories are also searched.

    The *Scope* entry restricts the search to a set of language constructs.
    For example, use this to to avoid matching on comments when you're only
    interested in actual code or to only search strings and comment,
    ignoring the code.

  *Files From Projects*

    Search in all the files from the project, including files from project
    dependencies. The *Scope* entry is described in the `Files...` section
    above.

  *Files From Current Project*

    Search in all the files from the currently selected project, defaulting
    to the root project if no project is currently selected. The currently
    selected project might be the one to which the source file belongs (if
    you;re in an editor) or the selected project (if you are in the Project
    view).  The *Scope* entry is described in the `Files...` section above.

  *Files From Runtime*

    Search in all specification files from GNAT runtime library.
    The *Scope* entry is described in the `Files...` section above.

  *Current File*

    Search in the current source editor.  The *Scope* entry is described in the
    `Files...` section above.

  *Project Browser*

    Search in the project browser (:ref:`The_Project_Browser`).


  .. index:: preferences; search --> preserve search context

  GPS sets the default value for :guilabel:`Look In` in various ways:
  normally, GPS selects a context that matches the currently selected
  window. For example, if you're in an editor and open the search dialog,
  the context is set to :guilabel:`Current File`. If the project view is
  the active window, the context is set to the :guilabel:`Project` view.
  Optionally, GPS can remember the last context that was set (see the
  preference :menuselection:`Search --> Preserve Search Context`). In that
  case, if an editor is selected, GPS remembers whether the last time you
  started a search from an editor you decided to search in (for example)
  :guilabel:`Current File` or :guilabel:`Files From Project`.

  Finally, you can create key shortcuts (through the :menuselection:`Edit -->
  Key Shortcuts` menu, in the :guilabel:`Search` category) to open the search
  dialog and set the context to a specific value.

.. image:: search-options.jpg

The second section in the dialog is a row of five buttons, used to start
the search (or continue to the next occurrence) to display options:


:guilabel:`Regexp`
  .. index:: regular expression

  Toggles between strings and regular expressions for patternns.  You can
  also toggle between these two modes by selecting the arrow on the right
  of the :guilabel:`Search for:` field.  The grammar used by the regular
  expressions is similar to the Perl and Python regular expressions grammar
  and is documented in the GNAT Pro run time file :file:`g-regpat.ads`. To
  open it from GPS, use the :ref:`open from project <Open_From_Project>`
  dialog (:menuselection:`File --> Open From Project...`) and type
  `g-regpat.ads`.

:guilabel:`Whole Word`
  .. index:: whole word

  Force the search engine to ignore substrings. "sensitive" will no longer
  match "insensitive".

:guilabel:`Select on Match`
  .. index:: select window on match

  Gives the focus to the editor containing the match. If the button is not
  selected, the focus remains on the search window.  In that case, press
  Enter to search for the next occurrence.

:guilabel:`Close on Match`
  .. index:: close dialog on match

  This button only appears if the search window is floating. If pressed,
  the search window is automatically closed when an occurrence of the
  search string is found.

:guilabel:`Case Sensitive Search`
  .. index:: case sensitive

  By default, patterns are case insensitive (upper-case letters and
  lower-case letters are considered equivalent).  Change this behavior by
  clicking this check box.

:guilabel:`Case Preserving Replace`
  .. index:: case preserving

  When this is checked, replacements preserve casing. Three casings are
  detected and preserved: all lower, all UPPER, and Mixed_Case (where the
  first character of each word is capitalized).  When the replacement
  pattern is not all lower case, replacement is never case-preserving: the
  original casing of the replacement pattern is used.


Press the :guilabel:`Find` or :guilabel:`Previous` button to perform an
interactive search.  It stops as soon as one occurrence of the pattern is
found.  At that point, the :guilabel:`Find` button is renamed to
:guilabel:`Next`, which you press (or type the equivalent shortcut
:kbd:`Ctrl-N`) to go to the next occurrence.

The :guilabel:`Find all` button starts a search for all occurrences and
puts the results in a view called :guilabel:`Locations`,
:ref:`The_Locations_View`.

The :guilabel:`Replace` and :guilabel:`Replace & Find` buttons are grayed
out if no occurence of the pattern is found. To enable them you must start
a search, for example by pressing the :guilabel:`Find` button. Pressing
:guilabel:`Replace` replaces the current occurrence (grayes out the two
buttons) and :guilabel:`Replace & Find` replaces the occurrence and jump to
the next one, if any. If you don't want to replace the current occurence,
you can jump to the next one by pressing :guilabel:`Next`.

The :guilabel:`Repl all` button replaces all the occurences found. By
default, a popup is displayed that asks for confirmation. You can disable
this popup by either checking the box "Do not ask this question again" or
by going to the Search pannel of the preferences pages and unchecking
"Confirmation for 'Replace all'".

.. index:: Multiple Document Interface

Like most GPS components, the search window is under control of the
multiple document interface and can be integrated into the main GPS window
instead of being an external window.  To do this, open the menu
:menuselection:`Window --> Search` in the list at the bottom of the menu,
and either select :menuselection:`Window --> Floating` or
:menuselection:`Window --> Docked`.

If you save the desktop (:menuselection:`File --> Save More --> Desktop`,
GPS automaticallys reopen the search dialog in its new place when it's
next started.
