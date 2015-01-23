.. _Searching_and_Replacing:

***********************
Searching and Replacing
***********************

GPS provides extensive search capabilities in different contexts. For
example, you can search in the currently edited source file or in all
source files belonging to the project, even those that are not currently
open. You can also search in the project view (on the left side of the main
GPS window).

.. index:: project view
.. index:: search context
.. index:: menu; navigate --> find or replace

All of these search contexts are merged into a single graphical window that
you can open either through the :menuselection:`Navigate --> Find or
Replace...` menu or the shortcut :kbd:`Ctrl-F`.

By default, the search window is floating and appears as a dialog on top of
GPS. Put it inside the multiple document interface for easier access by
selecting the :menuselection:`Window --> Floating` menu and dropping the
search window into a new location (for example, above the
:guilabel:`Project` view).  Selecting either option pops up a dialog on the
screen similar to the following:

.. image:: search-hide.jpg

This dialog contains three fields:

*Search for*
  .. index:: search for

  Type the string or pattern to search for. GPS supports two modes, strings
  or regular expressions. Toggle between the two modes either by clicking
  the :guilabel:`Options` button and selecting the appropriate check box or
  by opening the combo box by clicking on the arrow on the right of the
  entry field.

  The combo box provides a number of predefined patterns. The top two are
  empty patterns that automatically set the appropriate strings or regular
  expression mode. The other regular expressions are language-specific and
  match patterns such as Ada type definitions or C++ method declarations.

  .. index:: C++
  .. index:: Ada

*Replace with*
  .. index:: replace with

  Contains the string to replace the occurrences of the pattern.  The combo
  box provides a history of previously used replacement strings. If a
  regular expression is used for search, special escapes in this field are
  used as:

  * :samp:`\\1`, :samp:`\\2` .. :samp:`\\9` refer to the
    corresponding matching subexpressions;

  * :samp:`\\0` refers to the complete matched string;

  * :samp:`\\i`, :samp:`\\i(start,step)` refers to the sequentially increasing
    number (starting from start and increased by step on each replace).
    
*Look in*
  .. index:: look in

  The context in which the search should occur.

  .. index:: search context

  GPS automatically selects the most appropriate context when you open the
  search dialog by looking at the component that currently has the
  focus. If several contexts are possible for one component (for example,
  the editor has :guilabel:`Current_File`, :guilabel:`Files from Project`,
  :guilabel:`Files...`, and :guilabel:`Open Files`), the last one you used
  is selected.  In most contexts, the :guilabel:`Scope` option restricts
  the search to a set of language constructs.  For example, use this to to
  avoid matching comments when you are only interested in actual code or to
  only search strings and comments, but not code.

Change the context to a different one by clicking on the arrow on the
right, which displays the list of all possible contexts, including:

  * :command:`Project View`

    Search the :guilabel:`Project` view. An extra :guilabel:`Scope` box is
    displayed where you can specify the scope of your search, a set of:
    :guilabel:`Projects`, :guilabel:`Directories`, :guilabel:`Files`,
    :guilabel:`Entities`.  Searching entities can take a long time since
    GPS must parse every file during the search.

  * :command:`Open Files`

    Search all files currently open in the source editor.

  * :command:`Files...`

    Search a specified set of files. An extra :guilabel:`Files` box is
    displayed where you specify the files using standard shell (Unix or
    Windows) regular expressions (such as :file:`*.ad?` for all files
    ending with :samp:`.ad` and any trailing character). The directory
    specifies where the search starts and the :guilabel:`Recursive search`
    button whether subdirectories are also searched.

  * :command:`Files From Projects`

    Search all files from the current project, including files from project
    dependencies.

  * :command:`Files From Current Project`

    Search all files from the current project, defaulting to the root
    project if none. The currently selected project might be the one to
    which the source file belongs (if you are in an editor) or the selected
    project (if you are in the :guilabel:`Project` view).

  * :command:`Files From Runtime`

    Search all specification files from GNAT runtime library

  * :command:`Current File`

    Search the current source editor.

  * :command:`Project Browser`

    Search the :guilabel:`Project` browser (see :ref:`The_Project_Browser`).

  .. index:: preferences; search --> preserve search context

  Normally, GPS sets the default value for :guilabel:`Look In` that matches
  the currently selected window. For example, if you are in an editor and
  open the search dialog, the context is set to :guilabel:`Current
  File`. If the project view is the active window, the context is set to
  the :guilabel:`Project` view.  Optionally, GPS can remember the last
  context that was set (see the preference :menuselection:`Search -->
  Preserve Search Context`). In that case, if an editor is selected, GPS
  remembers whether the last time you started a search from an editor you
  decided to search in (for example) :guilabel:`Current File` or
  :guilabel:`Files From Project`.

  Finally, you can create key shortcuts (through the :menuselection:`Edit -->
  Key Shortcuts` menu, in the :guilabel:`Search` category) to open the search
  dialog and set the context to a specific value.

.. image:: search-options.jpg

The second section in the dialog is a row of five buttons, used to start
the search, or continue to the next occurrence, or set options:

* :guilabel:`Regexp`

  .. index:: regular expression

  Toggles between strings and regular expressions.  Or you can select the
  arrow to the right of the :guilabel:`Search for:` field.  The grammar
  used by regular expressions is similar to the Perl and Python regular
  expressions grammar and is documented in the GNAT run time file
  :file:`g-regpat.ads`. To open it from GPS, use the :ref:`open from
  project <Open_From_Project>` menu (:menuselection:`File --> Open From
  Project...`) and type :file:`g-regpat.ads`.

* :guilabel:`Whole Word`

  .. index:: whole word

  Force the search engine to ignore substrings. For example, "sensitive"
  no longer matches "insensitive".

* :guilabel:`Select on Match`

  .. index:: select window on match

  Gives the focus to the editor containing the match. If not selected, the
  focus remains on the search window.  If so, press :kbd:`Enter` to search
  for the next occurrence.

* :guilabel:`Close on Match`

  .. index:: close dialog on match

  This button only appears if the search window is floating. If pressed,
  the search window is automatically closed when an occurrence of the
  search string is found.

* :guilabel:`Case Sensitive Search`

  .. index:: case sensitive

  By default, patterns are case insensitive (upper-case letters and
  lower-case letters are considered equivalent).  Change this behavior by
  clicking this check box.

* :guilabel:`Case Preserving Replace`

  .. index:: case preserving

  When this is checked, replacements preserve casing. Three casings are
  detected and preserved: all lower, all UPPER, and Mixed_Case (where the
  first character of each word is capitalized).  When the replacement
  pattern is not all lower case, replacement is never case-preserving; the
  original casing of the replacement pattern is used.

Press the :guilabel:`Find` or :guilabel:`Previous` button to perform an
interactive search, which stops as soon as one occurrence of the pattern is
found.  At that point, the :guilabel:`Find` button is renamed to
:guilabel:`Next`, which you press (or type the equivalent shortcut
:kbd:`Ctrl-N`) to go to the next occurrence.

The :guilabel:`Find all` button starts a search for all occurrences and
puts the results in a view called :guilabel:`Locations` view,
see :ref:`The_Locations_View`.

The :guilabel:`Replace` and :guilabel:`Replace & Find` buttons are grayed
out if no occurence of the pattern is found. To enable them, start a
search, for example by pressing the :guilabel:`Find` button. Pressing
:guilabel:`Replace` replaces the current occurrence (grays out the two
buttons) and :guilabel:`Replace & Find` replaces the occurrence and jumps
to the next one, if any. If you do not want to replace the current
occurence, jump to the next one by pressing :guilabel:`Next`.

The :guilabel:`Repl all` button replaces all occurences found. By default,
a popup is displayed asking for confirmation. You can disable this popup by
either checking the box :guilabel:`Do not ask this question again` or going
to the :guilabel:`Search` panel of the preferences pages and unchecking
:guilabel:`Confirmation` for :guilabel:`Replace all`.

.. index:: Multiple Document Interface

Like most GPS components, the search window is under control of the
multiple document interface and can be integrated into the main GPS window
instead of being an external window.  To do this, open the
:menuselection:`Window --> Search` menu in the list at the bottom of the
menu, and either select :menuselection:`Window --> Floating` or
:menuselection:`Window --> Docked`.

If you save the desktop (:menuselection:`File --> Save More --> Desktop`),
GPS automatically reopens the search dialog in its new place when it is next
started.
