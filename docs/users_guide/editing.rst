.. _Editing_Files:

*************
Editing Files
*************

.. index:: editing
.. _General_Information:

General Information
===================

Source editing is one of the central parts of GPS.  It allows access to
many other functionalities, including extended source navigation and source
analyzing tools.  You can have as many editor windows as you need.  Each
editor window receives annotations from other components in GPS, such as a
debugger.

We use the term "pointer" to refer to the mouse pointer and "cursor" to
refer to the text cursor.

.. image:: source-editor.png

The source editor provides an extensive set of features, including:

*Title bar*
  Displays the full name of the file including path information in the title
  bar of the editor window.

*Line number information*
  Located to the left of the source editor, :index:`Line numbers
  <single: preferences; editor --> display line numbers>` can be disabled
  using the :menuselection:`Editor --> Display line numbers` preference.
  This area may also display additional information in some cases, such as
  the current line of execution when debugging or cvs annotations.

*Scrollbar*
  Located to the right of the editor, this allows scrolling through
  the source file.  The highlighted area of the scrollbar corresponds
  to the visible portion of the file.  While you are scrolling, the
  editor displays a tooltip showing the file, line number, and
  subprogram corresponding to the center of the visible portion.

*Speed column*
  This :index:`column <single: preferences; editor --> speed
  column policy>`, when visible, is located on the left of the editor. It
  allows you to view all the highlighted lines in a file at a glance. For
  example, all the lines containing compilation errors are displayed in the
  Speed Column. The preference :menuselection:`Editor --> Speed column
  policy` can be used to control the display of this area. It can sometimes
  be convenient to keep it visible at all times (to avoid resizing the
  editors when new information becomes available) or to hide it
  automatically when it is not needed to save some space on the screen.

*Status bar*
  Gives information about the file. It is divided in two sections, one each
  on the left and right of the window.

  - The left part of the status bar shows the current :index:`subprogram
    <single: preferences; editor --> display subprogram names>` name for
    languages that support this capability. Currently `Ada`, `C` and `C++`
    have this ability. The preference :menuselection:`Editor --> Display
    subprogram names` controls this display.


  - The right section contains multiple items:

    * The box displays the position of the cursor in the file as a line and
      column number. When you've made a selection in the editor, this area
      also displays the size of the selection (number of lines and number
      of characters).

    * Next to the box is an icon that shows whether the file is writable or
      read-only.  Change this state by clicking on the icon, which toggles
      between *Writable* and *Read Only*.  This will not change the
      permissions of the file on disk, it will only change the writability
      of the view in the source editor.

      When you try to save a file which is read only on the disk, GPS will
      ask for confirmation, and if possible, save of the file, keeping its
      read only state.

    * If the file is maintained under version control, and version control
      is supported and enabled in GPS, the next icon will show VCS
      information on the file: the VCS kind (e.g. CVS or subversion)
      followed by the revision number and, if available, the status of the
      file.

*Contextual menu*
  Displayed when you right-click on any area of the source editor.  See in
  particular :ref:`Contextual_Menus_for_Source_Navigation` for more details.

*Syntax highlighting*
  Based on the programming language associated with the file, reserved words
  and languages constructs such as comments and strings are highlighted in
  different colors and fonts.

  By default, GPS knows about many languages. You can also easily add support
  for other languages through plug-ins. Most languages supported by GPS will
  provide syntax highlighting in the editor.

*Automatic indentation*
  When enabled, lines are automatically :index:`indented <indentation>`
  each time you press the :kbd:`Enter` key or the indentation key.  By
  default, the indentation key is :kbd:`Tab`.  Change it in the key manager
  dialog, :ref:`The_Key_Manager_Dialog`.

  If a list of lines is selected when you press the indentation key, GPS
  will indent all the lines.

*Tooltips*
  When you place the pointer over a word in the source editor, the editor
  displays a small :index:`window <tooltip>` if there is relevant
  contextual information to display about that word.  The type of information
  displayed depends on the current state of GPS.

  In normal mode, the editor displays the entity kind and location of the
  declaration when this information is available, i.e., when the
  cross-reference information about the current file has been generated. If
  there is no relevant information, no tooltip is displayed.  See
  :ref:`Support_for_Cross-References` for more information.

  .. highlight:: ada

  In addition, the editor displays documentation for the entity, if
  available.  This is the block of comments immediately before or after the
  entity's declaration (without any intervening blank lines). For example,
  instance, the editor will display the following documentation for Ada::

    --  A comment for A
    A : Integer;

    B : Integer;
    --  A comment for B

    C : Integer;

    --  Not a comment for C, there is a blank linke

  When comments appear both before and after the entity, GPS choses the one
  given by the :index:`preference <preferences; documentation --> leading
  documentation>` :menuselection:`Documentation --> Leading documentation`.

  In debugging mode, the editor shows the value of the variable under the
  pointer if the variable is known to the debugger.  Otherwise, the normal
  information (see above) is displayed.

  Disable the automatic pop up of tool tips via the :index:`preference
  <preferences; editor --> tooltips>` :menuselection:`Editor --> Tooltips`.

*Code completion*
  GPS provides two kinds of code :index:`completion`: a :ref:`smart code
  completion <Smart_Completion>` based on semantic information and a text
  completion.

  Simple text completion is useful when editing a file using the same words
  repeatedly, where it provides automatic word completion.  When you type
  the :kbd:`Ctrl-/` key combination (customizable through the key manager
  dialog) after a partial word, GPS will insert the next potential
  completion into the editor. Typing this key again will cycle through the
  list of potential completions.  GPS searches for text completions in all
  currently open files.

*Delimiter highlighting*
  When the cursor is moved before an opening :index:`delimiter` or after a
  closing delimiter, the editor will highlight both delimiters.  The
  following characters are considered delimiters: ()[]{}.  Disable
  highlighting of delimiters with the :index:`preference
  <preferences;editor --> highlight delimiters>` :menuselection:`Editor -->
  Highlight delimiters`.

  Jump to a corresponding delimiter by invoking the `jump to matching
  delimiter` action (which can be bound to a key in the key shortcuts
  editor).  Invoking this action a second time moves the cursor back to its
  original position.

*Current line highlighting*
  Configure the editor to highlight the :index:`current line` with a
  specified color (see the :index:`preference <preferences; editor -->
  fonts & colors --> current line color>` :menuselection:`Editor --> Fonts
  & Colors --> Current line color`).

*Current block highlighting*
  If the :index:`preference <preferences;editor --> block highlighting>`
  :menuselection:`Editor --> Block highlighting` is enabled, the editor will
  highlight the current block of code, e.g. the current `begin...end`
  block, or loop statement by placing a vertical bar to its left.

  Block highlighting also takes into account the changes made in your
  source code and is recomputed to determine the current block when needed.
  This capability is currently implemented for the Ada, C, and C++
  languages.

*Block folding*
  When the :index:`preference <preferences;editor --> block folding>`
  :menuselection:`Editor --> Block folding` is enabled, the editor will display
  `-` icons on the left side corresponding to the beginning of blocks. If
  you click on one of these icons, all lines corresponding to this block
  are hidden except the first.  Like block highlighting, these icons are
  recomputed automatically when you modify your sources.

  This capability is currently implemented for Ada, C and C++ languages.

*Auto save*
  Configure the editor to periodically save modified files.  See
  :ref:`Autosave delay <autosave_delay>` for a full description of this
  capability.

*Automatic highlighting of entities*
  When the pointer is positioned on an entity in the source editor, GPS will
  highlight all references to this entity in the current editor.

  When the pointer is moved away from the entity, the highlighting is
  removed.

  .. index:: plug-ins; auto_highlight_occurrences.py

  This is controlled by the plugin :file:`auto_highlight_occurrences.py`: it
  can be deactivated by disabling the plugin (:ref:`The_Plug-ins_Editor`).

  Details such as presence of indications in the Speed Column or highlighting
  color can be customized in the `Plugins` section of
  :ref:`The_Preferences_Dialog`.

.. index:: emacs

GPS also integrates with existing third party editors such as `Emacs` or `vi`.
:ref:`Using_an_External_Editor`.

.. index:: editing
.. index:: source file
.. _Editing_Sources:

Editing Sources
===============

.. index:: key

Key bindings
------------

In addition to the standard keys used to navigate in the editor (up, down,
right, left, page up, page down), the integrated editor provides a number of
key bindings allowing easy navigation in the file.

There are also several ways to define new key bindings, see
:ref:`Defining_text_aliases` and :ref:`Binding_actions_to_keys`.

.. index:: hexadecimal
.. index:: ASCII


+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-Shift-u`     | Pressing these three keys and then holding Ctrl-Shift allow you to enter |
|                         | characters using their hexadecimal value. For example, pressing          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-Shift-u-2-0` | will insert a space character (ASCII 32, which is 20 in hexadecimal).    |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-x`           | Cut to clipboard                                                         |
| :kbd:`Shift-delete`     |                                                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-c`           | Copy to clipboard                                                        |
| :kbd:`Shift-insert`     |                                                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-v`           | Paste from clipboard                                                     |
| :kbd:`Shift-insert`     |                                                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-s`           | Save file to disk                                                        |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-z`           | Undo previous insertion/deletion                                         |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-r`           | Redo previous insertion/deletion                                         |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Insert`           | Toggle overwrite mode                                                    |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-a`           | Select the whole file                                                    |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Home`             | Go to the beginning of the line                                          |
| :kbd:`Ctrl-Pgup`        |                                                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`End`              | Go to the end of the line                                                |
| :kbd:`Ctrl-Pgdown`      |                                                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-Home`        | Go to the beginning of the file                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-End`         | Go to the end of the file                                                |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-up`          | Go to the beginning of the line or to the previous line if already at    |
|                         | the beginning of the line.                                               |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-down`        | Go to the end of the line or to the beginning of the next line if        |
|                         | already at the end of the line.                                          |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-delete`      | Delete to the end of the current word.                                   |
+-------------------------+--------------------------------------------------------------------------+
| :kbd:`Ctrl-backspace`   | Delete to the beginning of the current word.                             |
+-------------------------+--------------------------------------------------------------------------+

.. _Menu_Items:

Menu Items
==========

The main menus that give access to extended functionality related to source
editing are described in this section.

.. _The_File_Menu:

The :menuselection:`File` Menu
------------------------------

.. index:: menu; file --> new

:menuselection:`File --> New`
  Open a new untitled source editor.  No syntax highlighting is performed until
  the file is saved since GPS needs to know the file name in order to choose
  the programming language associated with a file.

  When you save a new file for the first time, GPS asks you to enter the
  name of the file. If have started typing Ada code, GPS tries to guess a
  name for the new file based on the first main entity in the editor and
  the current naming scheme.

.. index:: menu; file --> new view

:menuselection:`File --> New View`
  Create a new view of the current editor. The new view shares the same
  contents: if you modify one of the source views, the other view is
  updated at the same time. This is particularly useful when you want to
  display two different parts of the same file, for example a function spec
  and its body.

  You can also create a new view by holding the :kbd:`shift` key down while
  drag-and-dropping the editor (see :ref:`Moving_Windows`). This second
  method is preferred becasue you can specify where you want to put the new
  view. The default when using the menu is to put the new view on top of
  the current editor.

.. index:: menu; file --> open

:menuselection:`File --> Open...`
  Open a file selection dialog where you can select a file to edit. On
  Windows, this is the standard file selector. On other platforms, this is a
  built-in file selector described in :ref:`The_File_Selector`.

.. index:: menu; file --> open from project
.. _open_from_project:

:menuselection:`File --> Open From Project...`
  Move the focus to the :ref:`omni_search` field, where you can immediately
  start typing part of the file name you want to open. This is the fastest
  way to select files to open.

.. index:: menu; file --> open from host
.. _Open_From_Host:

:menuselection:`File --> Open From Host...`
  Open a file selector dialog where you can specify a remote host, as
  defined in :ref:`The_remote_configuration_dialog`. If you have access to
  a remote host file system, you can specify a file which can be edited in
  GPS. When you press the save button or menu item, the file will be saved
  on the remote host.

  See :ref:`Using_GPS_for_Remote_Development` for a more efficient way to
  work locally on remote files.

.. index:: menu; file --> recent

:menuselection:`File --> Recent`
  Open a submenu containing a list of the ten most recent files opened
  in GPS.

.. index:: menu; file --> save

:menuselection:`File --> Save`
  Save the file corresponding to current source editor, if there are changes.

.. index:: menu; file --> save as

:menuselection:`File --> Save As...`
  Save the current file under a different name, using the file selector
  dialog.  :ref:`The_File_Selector`.

.. index:: menu; file --> save more

:menuselection:`File --> Save More`
  Give access to additional save capabilities:

  - :menuselection:`File --> Save More --> All`
     Save all items, including projects.

  - :menuselection:`File --> Save More -->Desktop`
     Save the desktop to a file. The desktop includes information about
     files, graphs and their window sizes and positions in GPS. One desktop
     is saved per top level project so that when you reload the same
     project you get back to into the same state you were in when you left
     GPS. If you load a different project, another desktop will be loaded
     (or the default desktop).  Request GPS to automatically save this
     desktop when you quit with the :index:`preference <preferences;
     general --> save desktop on exit>` :menuselection:`General-->Save
     Desktop On Exit`.

.. index:: menu; file --> change directory

:menuselection:`File --> Change Directory...`
  Open a directory selection dialog that lets you change the current working
  directory.

.. index:: menu; file --> locations

:menuselection:`File --> Locations`
  This submenu gives access to functionalities related to the
  :guilabel:`Locations` window.

  - :menuselection:`File --> Locations --> Export Locations to Editor`
     List the contents of the :guilabel:`Locations` view in an editor.

.. index:: menu; file --> print
.. index:: print

:menuselection:`File --> Print`
  Print the current window contents, optionally saving it if it
  has been modified. The Print Command specified in the preferences is used if
  it is defined. On Unix this command is required; on Windows it is optional.

  On Windows, if no command is specified in the preferences, GPS
  displays the standard Windows print dialog box, which allows you to
  specify the target printer, the properties of the printer, which
  pages to print (all, or a specific range of pages), the number of
  copies to print, and, when more than one copy is specified, whether
  the pages should be collated.  Pressing the :guilabel:`Cancel`
  button on the dialog box returns to GPS without printing the window
  contents. Each page is printed with a header containing the name of
  the file (if the window has ever been saved).  The page number is
  printed on the bottom of each page.

  See also:ref:`Print Command <Print_Command>`.

.. index:: menu; file --> close

:menuselection:`File --> Close`
  Close the current window. This applies to all GPS windows, not just source
  editors.

.. index:: menu; file --> exit

:menuselection:`File --> Exit`
  Exit GPS after confirmation and if needed, confirmation about saving modified
  windows and editors.

.. _The_Edit_Menu:

The :menuselection:`Edit` Menu
------------------------------

.. index:: menu; edit --> cut

:menuselection:`Edit --> Cut`
  Cut the current selection and store it in the clipboard.

.. index:: menu; edit --> copy
.. index:: yank

:menuselection:`Edit --> Copy`
  Copy the current selection to the clipboard.

.. index:: menu; edit --> paste

:menuselection:`Edit --> Paste`
  Paste the contents of the clipboard at the current cursor position.

.. index:: menu; edit --> paste previous

:menuselection:`Edit --> Paste previous`
  GPS stores a list of all the text that was previously copied to the
  clipboard through the use of :guilabel:`Copy` or :guilabel:`Cut`.

  By default, if you press :guilabel:`Paste`, the newest text will be
  copied to the cursor's current position.  If you pres
  :guilabel:`Paste Previous` (one or more times) immediately after
  that, you can instead paste the text that was previously copied to
  the clipboard.

  For example, if you use :menuselection:`Edit --> Copy` to copy the text
  "First", then copy the text "Second", select :menuselection:`Edit -->
  Paste` to insert "Second" at the current cursor position. If you then
  select :menuselection:`Edit --> Paste Previous`, "Second" will be
  replaced by "First".

  Selecting this menu several times replaces the text previously
  pasted by the previous one in the list saved in the clipboard. When
  reaching the end of this list, GPS starts from the beginning, and
  again inserts the last text that was copied to the clipboard.

  The size of this list is controlled by the :menuselection:`General -->
  Clipboard Size` :index:`preference <preferences; general --> clipboard
  size>`.

  For more information, :ref:`The_Clipboard_View`.

.. index:: menu; edit --> undo

:menuselection:`Edit --> Undo`
  Undo previous insertion or deletion in the current editor.

.. index:: menu; edit --> redo

:menuselection:`Edit --> Redo`
  Redo previous insertion or deletion in the current editor.

.. index:: menu; edit --> rectangles

:menuselection:`Edit --> Rectangles...`
  See the section :ref:`Rectangles` for more information on rectangles.

.. index:: menu; edit --> rectangles --> serialize

:menuselection:`Edit --> Rectangles... -> Serialize`
  Increment a set of numbers found on adjacent lines.  The behavior
  depends on whether or not there is a current selection.

  If there is no selection, the set of lines modified begins with the
  current line and includes all adjacent lines that have at least one
  digit in the same column as the cursor. In the following example,
  '|' marks the place where the cursor starts::

     AAA |10 AAA
     CCC 34567 CCC
     DDD DDD

  Only the first two lines will be modified and will become::

     AAA 10 AAA
     CCC 11 CCC
     DDD DDD

  If there is a selection, all the lines in the selection are
  modified. For each line, the columns of each line that had digits in
  the same column of the first line are modified. Starting from the
  original example above, if you select all three lines, the
  replacement becomes::

     AAA 10 AAA
     CCC 11567 CCC
     DDD 12D

  Only the fifth and sixth columns are modified since only those
  columns contained digits in the first line.

  This feature assumes you are selecting a relevant set of lines. But
  it's designed most specifically for modifying blank parts of
  lines. For example, if you start with::

     AAA 1
     BBB
     CCC

  it becomes::

     AAA 1
     BBB 2
     CCC 3

.. index:: menu; edit --> select all

:menuselection:`Edit --> Select all`
  Select the entire contents of the current source editor.

.. index:: menu; edit --> insert file

:menuselection:`Edit --> Insert File...`
  Open a file selection dialog and insert the contents of that file in the
  current source editor at the current cursor position.

.. index:: menu; edit --> insert shell output

:menuselection:`Edit --> Insert Shell Output...`
  Open an input window at the bottom of the GPS window where you can
  specify any external command.  If the command succeeds, the output
  of the command is inserted at the current cursor position, or, if text
  is selected, the text is passed to the external command and replaced
  by the command's output.

.. index:: menu; edit --> format selection

:menuselection:`Edit --> Format selection`
  Indent and format the selection or the current line.
  :ref:`The_Preferences_Dialog`, for preferences related to source formatting.

.. index:: menu; edit --> smart completion
.. index:: completion
.. _Smart_Completion:

:menuselection:`Edit --> Smart completion`
  Complete the identifier prefix under the cursor and list the results
  in a pop-up window.  When used with Ada sources, this takes
  advantage of an entity database as well as Ada parsers embedded in
  GPS which analyze the context and offer completions from the entire
  project along with documentation extracted from comments surrounding
  declarations. To take full advantage of this feature, the smart
  completion preference must be enabled, which causes the computation
  of the entity database at GPS startup.

  .. index:: gcc; -fdump-xref

  The support for C and C++ is not as powerful as the support for Ada
  since it relies completely on the xref information files generated
  by the compiler, does not take into account the C/C++ context around
  the cursor, and does not extract documentation from comments around
  candidate declarations. To take advantage of this feature, in
  addition to enabling the smart completion preference, the C/C++
  application must be built with `-fdump-xref`.

  In order to use this feature, open any Ada, C or C++ file and begin to
  type an identifier, which must be declared either in the current file
  (and accessible from the cursor location) or in one of the packages of
  the loaded project.  Move the cursor after the last character of the
  incomplete identifier and hit the completion key (:kbd:`control-space` by
  default).  GPS opens a popup displaying all known identifiers that begin
  with the prefix you typed.  Browse among the various possibilities by
  clicking on the :kbd:`up` and :kbd:`down` keys or using the left
  scrollbar. For each entity, a documentation box is display. If the
  location of the entity is known, it's displayed as an hyperlink and you
  can jump directly to its declaration by clicking on it.

  Typing additional letters will reduce the range of possibilities, as long
  as possibilities remain. Once you've selected the expected completion,
  confirm it by pressing :kbd:`Enter`.

  Typing control characters (i.e., characters which cannot be used in
  identifiers) also confirms the current selection.

  GPS is also able to automatically complete subprogram parameters or
  dotted notation for child and nested packages. For example, if you type::

    with Ada.

  the smart completion window appears, listing all the child and nested
  packages of Ada. You can configure the time interval after which the
  completion window appears (:ref:`The_Preferences_Dialog`).

  You can also write the beginning of the package, e.g.::

    with Ada.Text

  and pressing the completion key will offer you Text_IO.

  If you are in a code section, you can complete the fields of a
  record, or the contents of a package, e.g.::

     declare
       type R is record
          Field1 : Integer;
          Field2 : Integer;
       end record;

       V : R;
    begin
       V.

  Completing V. will propose Field1 and Field2.

  The smart completion also lists the possible parameters of a call
  you're currently making. For example, in the following code::

       procedure Proc (A, B, C : Integer);
    begin
       Proc (1,

  If you hit the completion key after the comma, the smart completion
  engine proposes completing with the named parameters "B =>", "C =>"
  or directly to complete with all the remaining parameters, in this
  case "B =>, C => )".

  .. image:: smart-completion.jpg

  Limitations:

  * This feature is currently only available for Ada, C and C++. Using
    the smart completion on sources of other languages behaves as the
    :ref:`identifier completion <Complete_Identifier>` does.

  * Smart completion for C and C++ is based on the xref information
    generated by the compiler. Therefore, GPS has no knowledge of
    recently edited files: you must rebuild with `-fdump-xref` to
    update the completion database.

  * Smart completion for C and C++ is only triggered at the beginning
    of an expression (that is, it is not triggered on special
    characters such as '(', '->', or the C++ operator '::') and may
    propose too many candidates since it does not have knowlege of the
    C/C++ syntax context. Typing new letters reduces the range of
    possibilities, as long as possibilitites remain.

  * Smart completion of subprogram parameters, fields and dotted
    notation are not yet available for C and C++.


.. index:: menu; edit --> more completion

:menuselection:`Edit --> More Completion`
  This submenu contains more ways to automatically complete code.

  .. index:: menu; edit --> more completion --> expand alias

  * :menuselection:`Edit --> More Completion --> Expand alias`

    Consider the current word as an alias and expand according to aliases
    defined in :ref:`Defining_text_aliases`.

  .. index:: menu; edit --> more completion --> complete identifier
  .. index:: complete identifier
  .. _Complete_Identifier:

  * :menuselection:`Edit --> More Completion --> Completion Identifier`

    Complete the identifier prefix at the cursor. This command cycles
    through all identifiers starting with the specified prefix.

  .. index:: menu; edit --> more completion --> complete block
  .. index:: complete block

  * :menuselection:`Edit -- >More Completion --> Complete block`

    Close the current statement (if, case, loop) or unit (procedure,
    function, package). This action works only on an Ada buffer.

.. index:: menu; edit --> selection

:menuselection:`Edit --> Selection`
  This submenu contains actions that apply to the current selection in the
  editor.

  .. index:: menu; edit --> selection --> comment lines

  * :menuselection:`Edit --> Selection --> Comment lines`

     Make the current selection or line into a comment based on the
     current programming language syntax.

  .. index:: menu; edit --> selection --> uncomment lines

  * :menuselection:`Edit --> Selection --> Uncomment lines`

     Remove the comment delimiters from the current selection or line.

  .. index:: menu; edit --> selection --> refill

  * :menuselection:`Edit --> Selection --> Refill`

     Rearrange line breaks in the selection or current line so that
     line lengths do not exceed the maximum length, as set in the
     "Right margin" preference (:ref:`The_Preferences_Dialog`).

  .. index:: menu; edit --> selection --> sort

  * :menuselection:`Edit --> Selection --> Sort`

      Sort the selected lines alphabetically. This is particularly
      useful when editing files that are not source code or for
      specific parts of code, such as `with` clauses in Ada.

  .. index:: menu; edit --> selection --> sort reverse

  * :menuselection:`Edit --> Selection --> Sort Reverse`

      Sort the selected lines in reverse alphabetical order

  .. index:: menu; edit --> selection --> pipe in external program

  * :menuselection:`Edit --> Selection --> Pipe in external program...`

      Open an input window at the bottom of the GPS window where you
      can specify any external command which will be passed the
      current selection as input. If the command succeeds, the
      selection will be replaced by the output of the command.

  .. index:: menu; edit --> selection --> untabify
  .. index:: tabs

  * :menuselection:`Edit --> Selection --> Untabify`

      Replace all tabs in the current selection (or in the whole buffer if
      there is no selection) by the appropriate number of spaces

  .. index:: menu; edit --> selection --> move right
  .. index:: menu; edit --> selection --> move left

  * :menuselection:`Edit --> Selection --> Move Right`
  * :menuselection:`Edit --> Selection --> Move Left`

      Shift the currently selected lines (or the current line if there
      is no selection) one character to the right or left.

.. index:: menu; edit --> fold all blocks
.. index:: code folding

:menuselection:`Edit --> Fold all blocks`
  Collapse all the blocks in the current file.

.. index:: menu; edit --> unfold all blocks

:menuselection:`Edit --> Unfold all blocks`
  Uncollapse all the blocks in the current file.

.. index:: menu; edit --> create bookmark

:menuselection:`Edit --> Create bookmark`
  Creates a new Bookmark at cursor position. For more information,
  :ref:`Bookmarks`.

.. index:: menu; edit --> pretty print
.. index:: pretty print
.. index:: gnatpp

:menuselection:`Edit --> Pretty Print`
  Pretty print the current source editor by calling the external tool
  `gnatpp`.  `gnatpp` switches may be specified in the switch editor.
  :ref:`The_Switches_Editor`.

.. index:: menu; edit --> generate body
.. index:: generate body
.. index:: gnatstub

:menuselection:`Edit --> Generate Body`
  Generate an Ada body stub for the current source editor by calling
  the external tool `gnatstub`.

.. index:: menu; edit --> edit with external editor

:menuselection:`Edit --> Edit with external editor`
  :ref:`Using_an_External_Editor`.

.. index:: menu; edit --> aliases
.. index:: alias

:menuselection:`Edit --> Aliases`
  Display the Aliases editor. :ref:`Defining_text_aliases`.

.. index:: menu; edit --> key shortcuts
.. index:: key shortcuts

:menuselection:`Edit --> Key shortcuts`
  Bring up the key manager dialog, used to associate commands
  with special keys. :ref:`The_Key_Manager_Dialog`.

.. index:: menu; edit --> preferences

:menuselection:`Edit --> Preferences`
  Bring up the preferences dialog. :ref:`The_Preferences_Dialog`.


.. index:: rectangle
.. index:: menu; edit --> rectangles
.. _Rectangles:

Rectangles
==========

Rectangle commands operate on a rectangular area of the text, in other
words all the characters between two columns in a certain range of
lines.

.. index:: plug-ins; emacs.py

A rectangle is selected using the standard selection mechanism. You
can either use the mouse to highlight the proper region or use
:kbd:`shift` and the cursor keys to extend the selection or use the
Emacs selection (with the mark and the current cursor location) if you
have activated the :file:`emacs.py` plugin.

Visually, a selected rectangle appears exactly the same as the
standard selection.  In particular, the characters after the last
column on each line will also be highlighted. Whether a selection is
interpreted as full text or a rectangle depends on the command you use
to manipulate the selection.

If you use one of the commands from the :menuselection:`Edit -->
Rectangles` menu, the rectangle extends from the top-left corner down
to the bottom-right corner.  All characters to the right of the
right-most column, although they are highlighted, are not considered
part of the rectangle.

Consider for instance the following text::

  package A is
     procedure P;

     procedure Q;
  end A;


and assume we have selected from the character "p" in "procedure P"
down to the character "c" in "procedure Q".

You can then use one of the following commands (either from the menu or
assign key shortcuts to them via the usual :menuselection:`Edit --> Key
shortcuts` menu).

* :menuselection:`Edit --> Rectangles --> Cut` or :menuselection:`Edit -->
  Rectangles --> Delete`

  Remove the selected text (and have no effect on empty lines within
  the rectangle). The former command will, in addition, copy the
  rectangle to the clipboard so you can paste it later. In our
  example, we end up with::

    package A is
       edure P;

       edure Q;
    end A;

* :menuselection:`Edit --> Rectangles --> Copy`
  Copies the contents of the rectangle into the clipboard without
  affecting the current editor.

* :menuselection:`Edit --> Rectangles --> Paste`
  Pastes the contents of the clipboard as a rectangle: each line from the
  clipboard is treated independently and inserted on successive lines in
  the current editor. They all start in the same column (the one where the
  cursor was initially in) and existing text in the editor lines is shifted
  to the right. If, for example, you now place the cursor in the first
  column of the second line and paste, we end up with::

    package A is
    proc   edure P;

    proc   edure Q;
    end A;

* :menuselection:`Edit --> Rectangles --> Clear`
  Replaces the contents of the selected rectangle with spaces. If we start
  from our initial exmaple, we end up with the following. Note the difference
  between this and :menuselection:`Edit --> Rectangles --> Delete`::

    package A is
           edure P;

           edure Q;
    end A;

* :menuselection:`Edit --> Rectangles --> Open`
  Replaces the contents of the selected rectangle with spaces but shifts
  the lines to the right to do so. Note the difference between this and
  :menuselection:`Edit --> Rectangles --> Clear`::

    package A is
           procedure P;

           procedure Q;
    end A;

* :menuselection:`Edit --> Rectangles --> Replace With Text`
  Similar to :menuselection:`Edit --> Rectangles --> Clear` but the
  rectangle is replaced with user-defined text. The lines are shifted left
  or right if the inserted text is shorter (respectively, longer) than the
  width of the rectangle. If, for example, we replace our initial rectangle
  with the text "TMP", we end up with the following. Note that the character
  "c" has disappeared, since "TMP" is shorter than our rectangle width (4
  characters).  This command affects lines that are empty in the initial
  rectangle::

    package A is
       TMPedure P;
       TMP
       TMPedure Q;
    end A;

* :menuselection:`Edit --> Rectangles --> Insert Text`
  Inserts text to the left of the rectangle on each line. The following
  example inserts "TMP". Note the difference between this command and
  :menuselection:`Edit --> Rectangles --> Replace With Text`. This command
  also inserts the text on lines that are empty in the initial rectangle::

    package A is
       TMPprocedure P;
       TMP
       TMPprocedure Q;
    end A;

* :menuselection:`Edit --> Rectangles --> Sort`
  Sorts the selected lines according to the key which starts and ends on
  the rectangle's columns::

    aaa 15 aa
    bbb 02 bb
    ccc 09 cc

  With a selection starting from the 1 on the first line and ending on the
  9 on the last, the lines will be sorted as follows::

    bbb 02 bb
    ccc 09 cc
    aaa 15 aa

* :menuselection:`Edit --> Rectangles --> Sort reverse`

  As above but in the reverse order.



.. index:: macros
.. _Recording_and_replaying_macros:

Recording and replaying macros
==============================

It is often convenient to be able to repeat a given key sequence a number
of times.

GPS supports this with several different methods:

* Repeat the next action

  .. index:: action; repeat next

  If you want to repeat the action of pressing a single key that you wish
  to repeat a number of times, you should first use the GPS action `"Repeat
  Next"` (bound by default to :kbd:`control-u`, but this can be changed as
  usual through the :menuselection:`Edit --> Key Shortcuts` menu),
  entering the number of times you wish to repeat, and then pressing the
  key whose action you want to repeat.

  For example, the sequence :kbd:`control-u 79 -` inserts 79 characters of
  '-' in the current editor.  This is often useful to insert separators.

  If you are using the emacs mode (see :menuselection:`Tools --> Plug-ins`
  menu), you can also use the sequence :kbd:`control-u 30 control-k` to
  delete 30 lines.

* Recording macros

  .. index:: menu; tools --> macros

  To repeat a sequence of more than 1 key, you should record the sequence
  as a macro. All macro-related menus are found in :menuselection:`Tools
  --> Macros`, but it's often more convenient to use these through key
  bindings, which you can of course override.

  First, you must tell GPS to should start recording the keys you are
  pressing.  You do this via the :menuselection:`Tools --> Macros --> Start
  Keyboard Macro` menu.  As its name indicates, this only records keyboard
  events, not mouse events.  GPS will keep recording the events until you
  select :menuselection:`Tools --> Macros --> Stop Macro`.

  In Emacs mode, the macro actions are bound to :kbd:`control-x (`,
  :kbd:`control-x )` and :kbd:`control-x e` key shortcuts. For example, you
  can execute the following to create a very simple macro that deletes the
  current line wherever your cursor initially is on that line:

  *  :kbd:`control-x (`     start recording
  *  :kbd:`control-a`      go to beginning of line
  *  :kbd:`control-k`      delete line
  *  :kbd:`control-x )`     stop recording


.. index:: automatic casing; exceptions
.. _Contextual_Menus_for_Editing_Files:

Contextual Menus for Editing Files
==================================

Whenever you ask for a contextual menu (using e.g. the right button on your
mouse) on a source file, you will get access to a number of entries, which
are displayed or not depending on the current context.

These menu entries include the following categories:

*Source Navigation*
  :ref:`Contextual_Menus_for_Source_Navigation`.

*Dependencies*
  :ref:`The_Dependency_Browser`.

*Entity browsing*
  :ref:`Entity_Browser`.

*Project view*
  :ref:`The_Project_View`.

*Version control*
  :ref:`The_Version_Control_Contextual_Menu`.

*Debugger*
  :ref:`Using_the_Source_Editor_when_Debugging`.

*Case exceptions*
  :ref:`Handling_of_case_exceptions`.

*Refactoring*
  :ref:`Refactoring`.

.. index:: language, editor
.. index:: character set

In addition, an entry :guilabel:`Properties...` is always visible in this
contextual menu. When you select it, a dialog pops allowing you to override
the language or the character set used for the file.  This is useful when
opening a file that does not belong to the current project but where you
want to benefit from the syntax highlighting, which depends on knowing
the file's language.

You should not override the language for source files belonging to the
current project. Instead, use the :menuselection:`Project --> Edit Project
Properties` menu and change the naming scheme as appropriate. This will
provide better consistency between GPS and the compiler in the way they
manipulate the file.

.. index:: casing; automatic
.. _Handling_of_case_exceptions:

Handling of casing
==================

GPS maintains a dictionary of identifiers and a corresponding casinga that
are used by all case insensitive languages. When editing or reformatting a
buffer for such a language, the dictionary will be checked first. If GPS
finds an entry for a word or a substring of a word, it will be used;
otherwise the specified default casing for keywords or identifiers is
used. A substring is defined as a part of the word separated by
underscores.

.. index:: preferences; editor --> ada --> casing policy
.. index:: preferences; editor --> ada --> reserved word casing
.. index:: preferences; editor --> ada --> identifier casing

This feature is not activated for entities (keywords or identifiers) for
which the casing is set to :guilabel:`Unchanged` in the preferences
:menuselection:`Editor --> Ada --> Reserved word casing` or
:menuselection:`Editor --> Ada --> Identifier casing`.

A contextual menu named :menuselection:`Casing` has the following entries:

:menuselection:`Casing --> Lower *entity*`
  Set the selected entity to be in lower case.

:menuselection:`Casing --> Upper *entity*`
  Set the selected entity to be in upper case.

:menuselection:`Casing --> Mixed *entity*`
  Set the selected entity to be in mixed case (the first letter and letters
  before an underscore are in upper case and all other letters are in lower
  case).

:menuselection:`Casing --> Smart Mixed *entity*`
  Set the selected entity as smart mixed case, which is the same as above
  except that upper case letters are kept unchanged.

:menuselection:`Casing --> Add exception for *entity*`
  Add the current entity into the dictionary.

:menuselection:`Casing --> Remove exception for *entity*`
  Remove the current entity from the dictionary.

To add or remove a substring from the dictionary, first select the
substring in the editor.  Then, the last two contextual menu entries will
be:

:menuselection:`Casing --> Add substring exception for *str*`
  Add the selected substring into the dictionary.

:menuselection:`Casing --> Remove substring exception for *str*`
  Remove the selected substring from the dictionary.


.. index:: refactoring
.. _Refactoring:

Refactoring
===========

GPS includes basic facilities to refactor your code.  "Refactoring" is the
term used to describe manipulation of source code that do not affect the
behavior of the code but help reorganize it to make it more readable, more
extendable, or make other types of improvements.

Refactoring techniques are generally things that programmers have done by
hand, but which can be done faster and more securely when done
automatically through a tool.

A basic recommendations when you refactor your code is to recompile and
test your application regularly to make sure each of the small
modifications you made didn't change the behavior of your application.
This is particularly true with GPS, since it relies on the cross-references
information generated by the compiler. If some source files have not been
recompiled recently, GPS will print warning messages indicating that the
operation might be dangerous and/or only performed partially.

One of the reference books used in the choice of refactoring methods for
GPS to implement is "Refactoring", by Martin Fowler (Addison Wesley).

.. _Rename_Entity:

Rename Entity
-------------

Clicking on an entity in a source file and selecting the
:menuselection:`Refactoring --> Rename` contextual menu opens a dialog
asking for the new name of the entity. GPS will rename all instances of the
entity in your application, including the definition of the entity, its
body, and all calls to it.  No comments is updated so you should probably
manually check that the comment for the entity still applies.

GPS handles primitive operations by also renaming the operations it
overrides or that overrides it, so that any dispatching call to that
operation is also renamed and the application will still work as before. If
you're renaming a parameter to a subprogram, GPS will also rename
parameters with the same name in overriding or overridden subprograms.

The behavior for read-only files can be specified: by default, GPS will not
do any refactoring in these files and will instead display a dialog listing
all of them.  However, you can also choose to make them writable just as if
you had clicked on the "Read-Only" button in the status bar of the editor
and then have GPS perform the renaming in them as well.

.. _Name_Parameters:

Name Parameters
---------------

If you are editing Ada code and click on a call to a subprogram, GPS
displays a contextual menu :menuselection:`Refactoring --> Name
parameters`, which will replace all unnamed parameters by named parameters,
for example::

     Call (1, 2)
  =>
     Call (Param1 => 1, Param2 => 2);


.. _Extract_Subprogram:

Extract Subprogram
------------------

This refactoring is used to move some code from into a separate subprogram.
This is done to simplify the original subprogram by moving part of its
code elsewhere.

Here's an example from the "Refactoring" book. The refactoring takes place
in the body of the package :file:`pkg.adb`, but the spec is needed so you
can compile the source code (a preliminary step mandatory before you can
refactor the code)::

  pragma Ada_05;

  with Ada.Containers.Indefinite_Doubly_Linked_Lists;
  with Ada.Strings.Unbounded;

  package Pkg is

     type Order is tagged null record;
     function Get_Amount (Self : Order) return Integer;

     package Order_Listsis new
        Ada.Containers.Indefinite_Doubly_Linked_Lists (Order);

     type Invoice is tagged record
        Orders : Order_Lists.List;
        Name   : Ada.Strings.Unbounded.Unbounded_String;
     end record;

     procedure Print_Owing (Self : Invoice);

  end Pkg;

The initial implementation for this code is given by the following code::

  pragma Ada_05;
  with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
  with Ada.Text_IO;            use Ada.Text_IO;

  package body Pkg is
     use Order_Lists;

     ----------------
     -- Get_Amount --
     ----------------

     function Get_Amount (Self : Order) return Integer is
     begin
        return 0;
     end Get_Amount;

     -----------------
     -- Print_Owing --
     -----------------

     procedure Print_Owing (Self : Invoice) is
        E : Order_Lists.Cursor := First (Self.Orders);
        Outstanding : Natural := 0;
        Each : Order;
     begin
        --  <<< line 30
        --  Print Banner

        Put_Line ("");
        Put_Line (" Customer Owes         ");
        Put_Line ("");  --  << line 35

        --  Calculate Outstanding

        while Has_Element (E) loop
           Each := Element (E);
           Outstanding := Outstanding + Each.Get_Amount;
           Next (E);
        end loop;

        --  Print Details

        Put_Line ("Name: " & To_String (Self.Name));
        Put_Line ("Outstanding:" & Outstanding'Img);
     end Print_Owing;
  end Pkg;

We feel that the procedure `Print_Owing` is too long and does several
independent actions, so we'll perform a series of three successive
refactoring steps to extract the code and move it elsewhere.

First, we move the code that prints the banner. Moving it is easy, since
this code does not depend on any context. We could just do a copy-paste,
but then we would have to create the new subprogram. Instead, we select
lines 30 to 35 and then select the contextual menu
:menuselection:`Refactoring --> Extract Subprogram`.  GPS removes those
lines from the subprogram `Print_Owing` and creates a new procedure
`Print_Banner` (the name is specified by the user; GPS does not try to
guess a name). Also, since the chunk of code that is extracted starts with
a comment, GPS automatically uses that comment as the documentation for the
new subprogram.  Here is the relevant part of the resulting file::

  package body Pkg is

     procedure Print_Banner;
     --  Print Banner

     ------------------
     -- Print_Banner --
     ------------------

     procedure Print_Banner is
     begin
        Put_Line ("");
        Put_Line (" Customer Owes         ");
        Put_Line ("");
     end Print_Banner;

     ... (code not shown)

     procedure Print_Owing (Self : Invoice) is
        E : Order_Lists.Cursor := First (Self.Orders);
        Outstanding : Natural := 0;
        Each : Order;
     begin
        Print_Banner;

        --  Calculate Outstanding

        while Has_Element (E) loop
           Each := Element (E);
           Outstanding := Outstanding + Each.Get_Amount;
           Next (E);
        end loop;

        --  Print Details   <<< line  54

        Put_Line ("Name: " & To_String (Self.Name));
        Put_Line ("Outstanding:" & Outstanding'Img);  --  line 57
     end Print_Owing;
  end Pkg;

A more interesting example is when we want to extract the code to print the
details of the invoice. This code depends on one local variable and the
parameter to Print_Owing.  When we select lines 54 to 57 and extract it
into a new `Print_Details` subprogram, GPS automatically decides which
variables to extract and whether they should become parameters of the new
subprogram or local variables. In the former case, it will also
automatically decide whether to create `"in"`, `"out"` or `"in out"`
parameters. If there's a single `"out"` parameter, GPS will automatically
create a function rather than a procedure.

GPS will use the same name used for the local variable for the
parameters. Often, it makes sense to recompile the new version of the
source and apply the :menuselection:`Refactoring --> Rename Entity`
refactoring to have more specific names for the parameters, or the
:menuselection:`Refactoring --> Name Parameters` refactoring so that call
to the new method uses named parameters to further clarify the code::

     ... code not shown

     procedure Print_Details
       (Self : Invoice'Class;
        Outstanding : Natural);
     --  Print Details

     -------------------
     -- Print_Details --
     -------------------

     procedure Print_Details
       (Self : Invoice'Class;
        Outstanding : Natural)
     is
     begin
        Put_Line ("Name: " & To_String (Self.Name));
        Put_Line ("Outstanding:" & Outstanding'Img);
     end Print_Details;

     procedure Print_Owing (Self : Invoice) is
        E : Order_Lists.Cursor := First (Self.Orders);
        Outstanding : Natural := 0;
        Each : Order;
     begin
        Print_Banner;

        --  Calculate Outstanding

        while Has_Element (E) loop
           Each := Element (E);
           Outstanding := Outstanding + Each.Get_Amount;
           Next (E);
        end loop;

        Print_Details (Self, Outstanding);
     end Print_Owing;

Finally, we want to extract the code that computes the outstanding
balance. When this code is moved, the variables `E` and `Each` become dead
in `Print_Owing` and are moved into the new subprogram (which we call
`Get_Outstanding`). Here's the result of that last refactoring (the initial
selection should include the blank lines before and after the code to keep
the resulting `Print_Owing` simpler). GPS will automatically ignore those
blank lines::

     ... code not shown

     procedure Get_Outstanding (Outstanding : in out Natural);
     --  Calculate Outstanding

     ---------------------
     -- Get_Outstanding --
     ---------------------

     procedure Get_Outstanding (Outstanding : in out Natural) is
        E : Order_Lists.Cursor := First (Self.Orders);
        Each : Order;
     begin
        while Has_Element (E) loop
           Each := Element (E);
           Outstanding := Outstanding + Each.Get_Amount;
           Next (E);
        end loop;
     end Get_Outstanding;

     procedure Print_Owing (Self : Invoice) is
        Outstanding : Natural := 0;
     begin
        Print_Banner;
        Get_Outstanding (Outstanding);
        Print_Details (Self, Outstanding);
     end Print_Owing;

The final version of `Print_Owing` is not perfect. For example, passing the
initial value 0 to `Get_Outstanding` is useless and in fact it should
probably be a function with no parameter. But GPS already saves a lot of
time and manipulation even despite that.

Finally, a word of caution: this refactoring does not check that you are
giving a valid input. For instance, if the text you select includes a
`declare` block, you should always include the full block, not just a part
of it (or select text between `begin` and `end`). Likewise, GPS does not
expect you to select any part of the variable declarations, just the code.


.. index:: external editor
.. _Using_an_External_Editor:

Using an External Editor
========================

.. index:: preferences; editor --> external editor

GPS is integrated with a number of external editors, in particular `Emacs`
and `vi`. The choice of the default external editor is done in the
preferences, via :menuselection:`Editor --> External editor`.

The following values are recognized:

.. index:: gnuclient

:guilabel:`gnuclient`
  This is the recommended client. It is based on Emacs, but needs an extra
  package to be installed. This is the only client that provides a full
  integration in GPS, since any extended lisp command can be sent to the
  Emacs server.

  By default, gnuclient will open a new Emacs frame for every file that is
  opened. You might want to add the following code to your :file:`.emacs`
  file (create one if needed) so that the same Emacs frame is reused every
  time::

       (setq gnuserv-frame (car (frame-list)))

  See `http://www.hpl.hp.com/personal/ange/gnuserv/home.html
  <http://www.hpl.hp.com/personal/ange/gnuserv/home.html>`_ for more
  information.


.. index:: emacsclient

:guilabel:`emacsclient`
  This is a program that is always available if you have installed Emacs. As
  opposed to starting a new Emacs every time, it will reuse an existing Emacs
  session. It's then extremely fast to open a file.

.. index:: emacs

:guilabel:`emacs`
  This client will start a new Emacs session every time a file needs to be
  opened. You should use `emacsclient` instead, since it's much faster,
  and it makes it easier to copy and paste between multiple files.
  The only reason to use this external editor is if your system doesn't
  support `emacsclient`.


.. index:: vi

:guilabel:`vim`
  `Vim` is a vi-like editor that provides a number of enhancements, for
   example syntax highlighting for all the languages supported by GPS.
  Selecting this external editor will start an `xterm` (or command window,
  depending on your system) with a running `vim` process editing the file.

  One limitation of this editor is that if GPS needs to open the same file
  a second time, it will open a new editor instead of reusing the existing
  one.

  To enable this capability, the `xterm` executable must be found in the
  PATH and thus is not supported on Windows systems.  On Windows systems,
  use the `custom` editor instead.

:guilabel:`vi`
  This editor works exactly like vim, but uses the standard `vi` command
  instead of `vim`.


.. index:: preferences; editor --> custom editor command

:guilabel:`custom`
  Specify any external editor by choosing this item.  You specify the
  complete command line used to call the editor in the preference
  :menuselection:`Editor --> Custom editor command`.

:guilabel:`none`
  No external editor is used, and the contextual menus won't appear.

In the cases that require an Emacs server, the project file currently used
in GPS are set appropriately the first time Emacs is spawned. This means
that if you load a new project in GPS or modify the paths of the current
project, you should kill any running Emacs, so a new one is spawned by GPS
with the appropriate project.

Alternatively, explicitly reload the project from Emacs itself by using the
menu :menuselection:`Project --> Load` in emacs (if the ada-mode was
correctly installed).

.. index:: preferences; editor --> always use external editor

The preference :menuselection:`Editor --> Always use external editor` lets
you chose to always use an external editor every time you double-click on
a file, instead of opening GPS' own editor.


.. index:: clipboard
.. index:: cut
.. index:: copy
.. index:: yank
.. index:: paste
.. _Using_the_Clipboard:

Using the Clipboard
===================

This section is of interest to X-Window users who are used to cutting and
pasting with the middle mouse button. In the GPS text editor, as in many
recent X applications, the *GPS clipboard* is set by explicit
cut/copy/paste actions, either through menu items or keyboard shortcuts,
and the *primary clipboard* (i.e. the 'middle button' clipboard) is set by
the current selection.

Therefore, copy/paste between GPS and other X applications using the
*primary clipboard* will still work provided there is some text currently
selected. The *GPS clipboard*, when set, overrides the *primary clipboard*.

By default, GPS overrides the X mechanism. To prevent this, add the
following line: `OVERRIDE_MIDDLE_CLICK_PASTE = no` to your
:file:`traces.cfg` file (typically in :file:`~/.gps/`). Note, that the X
mechanism pastes all attributes of text, including coloring and
editability, which can be confusing.

See `http://standards.freedesktop.org/clipboards-spec/clipboards-latest.txt
<http://standards.freedesktop.org/clipboards-spec/clipboards-latest.txt>`_
for more information.


.. index:: saving
.. _Saving_Files:

Saving Files
============

.. index:: menu; file --> save

After you have finished editing your files, you need to save them.  You do
that by selecting the menu :menuselection:`File --> Save`, which saves the
currently selected file.

.. index:: menu; file --> save as

Use the menu :menuselection:`File --> Save As...` if you want to save the
file with another name or in another directory.

.. index:: menu; file --> save more --> all

If you have multiple files to save, use the menu :menuselection:`File -->
Save More --> All`. This opens a dialog listing all the currently modified
editors. You can then select which ones should be saved and click on
:guilabel:`Save` to save those editors.

.. index:: preferences; editor --> autosave delay
.. index:: saving; automatic

When calling external commands, such as compiling a file, if the
:menuselection:`Editor --> Autosave delay` preference is set to 0, this
same dialog is also used to make sure that the external command will see
your changes.  If the preference is enabled, the editors are saved
automatically.

.. image:: save-dialog.jpg

Conveniently select or unselect all the files at once by clicking on the
title of the first column (labeled :guilabel:`Select`). This toggles the
selection status of all files.

If you press :guilabel:`Cancel` instead of :guilabel:`Save`, nothing will
be saved and the action that displayed this dialog is also canceled. Such
actions can be, for example, starting a compilation command, a VCS
operation, or quitting GPS with unsaved files.
