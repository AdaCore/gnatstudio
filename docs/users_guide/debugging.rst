.. index:: debugging
.. _Debugging:

*********
Debugging
*********

GPS also serves as a graphical front-end for text-based debuggers such as
GDB.  If you understand the basics of the underlying debugger used by GPS,
you'll better understand how GPS works and what kind of functionality it
provides.

Please refer to the debugger-specific documentation, e.g. the GNAT User's
Guide (chapter *Running and Debugging Ada Programs*), or the GDB documentation
for more details.

Debugging is tightly integrated with other components of GPS. For example,
you can edit files and navigate through your sources while debugging.

.. index:: menu; debug --> initialize
.. index:: menu; debug --> debug --> load file

To start a debug session, go to the :menuselection:`Debug --> Initialize`
menu and choose either the name of your executable, if you specified the
name of your main program(s) in the project properties, or start an empty
debug session using the :menuselection:`<no main file>` menu.  You can then
load any file to debug, by using the :menuselection:`Debug --> Debug -->
Load File...` menu.

You first need to build your executable with debug information
(:command:`-g` switch), either explicitly as part of your project
properties or via the :guilabel:`Debug` build mode (see
:ref:`The_Build_Mode` for more details).

Create multiple debuggers by using the :menuselection:`Debug -->
Initialize` menu several times: this creates a new debugger each time.  All
debugger-related actions (e.g. stepping, running) are performed in the
current debugger, represented by the current debugger console.  To switch
to a different debugger, select its corresponding console.

After the debugger has been initialized, you have access to two new
windows: the data browser (in the top of the working area) and the debugger
console (in a new page, after the :guilabel:`Messages` and
:guilabel:`Shell` windows).  You can now access any of the menus under
:menuselection:`Debugger` and you also have access to additional contextual
menus, in particular in the source editor where you can easily display
variables, set breakpoints, and get automatic displays (via tooltips) of
object values.

.. index:: menu; debug --> terminate
.. index:: menu; debug --> terminate current

To exit the debugger without quitting GPS, use the :menuselection:`Debug
--> Terminate Current` menu, which terminates your current debug session,
or the :menuselection:`Debug --> Terminate` menu which terminates all of
your current debug sessions.


.. _The_Debug_Menu:

The Debug Menu
==============

The :menuselection:`Debug` entry in the menu bar provides operations acting
at a global level. Key shortcuts are available for the most common
operations and are displayed in the menus.  Here's a detailed list of the
items in the menu bar:

.. index:: menu; debug --> run

* :menuselection:`Debug --> Run...`

  Opens a dialog window allowing you to specify the arguments to pass to
  the program to be debugged and whether execution should stop at the
  beginning of the main subprogram. If you confirm by clicking the
  :guilabel:`OK` button, GPS starts the program with the arguments you
  entered.

.. index:: menu; debug --> step

* :menuselection:`Debug --> Step`

  Execute the program until it reaches the next source line.


.. index:: menu; debug --> step instruction

* :menuselection:`Debug --> Next`

  Execute the program until it reaches the next source line, stepping over
  subroutine calls.

.. index:: menu; debug --> next instruction

* :menuselection:`Debug --> Step Instruction`

  Execute the program until it reaches the next machine instruction.

.. index:: menu; debug --> next

* :menuselection:`Debug --> Next Instruction`

  Execute the program until it reaches the next machine instruction,
  stepping over subroutine calls.

.. index:: menu; debug --> finish

* :menuselection:`Debug --> Finish`

  Execute the program until the subprogram running in the selected stack
  frame returns.

.. index:: menu; debug --> continue

* :menuselection:`Debug --> Continue`

  Continue execution of the program being debugged.

.. index:: menu; debug --> interrupt

* :menuselection:`Debug --> Interrupt`

  Asynchronously interrupt the program being debugged.  Depending on the
  state of the program, it may stop in low-level system code that does not
  have debug information or, in some cases, even a coherent state.  You
  should use breakpoints instead of interrupting programs, if possible.
  However, interrupting programs is nevertheless required in some
  situations, for example when the program appears to be in an infinite (or
  at least very long) loop.

.. index:: menu; debug --> terminate current
.. index:: preferences; debugger --> debugger windows

* :menuselection:`Debug --> Terminate Current`

  Terminate the current debug session by terminating the underlying
  debugger (e.g, :program:`gdb`) used to handle the low level debugging.
  Control what happens to the windows through the :menuselection:`Debugger
  --> Debugger Windows` preference.

.. index:: menu; debug --> termiante

* :menuselection:`Debug --> Terminate`

  Terminate all your debug sessions.  This is the same as
  :menuselection:`Debug --> Terminate Current` if you only have one
  debugger open.

Initialize
----------

This menu contains one item per main unit defined in your project.
Selecting that item starts a debug session and loads the executable
associated with the main unit selected and, if relevant, all corresponding
settings: a debug session opens the debug perspective and associated debug
properties (e.g.  saved breakpoints, and data display).

.. index:: menu; debug --> initialize --> no main file

* :menuselection:`Debug --> Initialize --> <No Main File>`

  Initializes the debugger with no executable.  Then use one of the other
  menu entries such as :menuselection:`Debug --> Debug --> Load File` or
  :menuselection:`Debug --> Debug --> Attach`.


Debug
-----

.. index:: board
.. index:: target
.. index:: cross debugger
.. index:: menu; debug --> debug --> connect to board

* :menuselection:`Debug --> Debug --> Connect to board`

  Opens a dialog to connect to a remote board. This option is only relevant
  for cross debuggers.

.. index:: menu; debug --> debug --> load file
.. _open_program_menu:

* :menuselection:`Debug --> Debug --> Load File...`

  Opens a file selection dialog allowing you to choose a program to debug.
  The program to debug is either an executable for native debugging or a
  partially linked module for cross environments (e.g VxWorks).

.. index:: menu; debug --> debug --> add symbols

* :menuselection:`Debug --> Debug --> Add Symbols`

  Adds the symbols from a given file. This corresponds to the
  :program:`gdb` command :command:`add-symbol-file`. This menu is
  particularly useful under VxWorks targets, where modules can be loaded
  independently of the debugger.  For example, if a module is independently
  loaded on the target using :command:`windshell`, you must use this
  functionality for the debugger to work properly.

.. index:: menu; debug --> debug --> attach

* :menuselection:`Debug --> Debug --> Attach...`

  Instead of starting a program to debug, attach to an already running
  process. To do so, specify the process id of the process you want to
  debug. The process might be busy in an infinite loop or waiting for event
  processing. Like :ref:`Core Files <core_files>`, you need to specify an
  executable before attaching to a process.

.. index:: menu; debug --> debug --> detach

* :menuselection:`Debug --> Debug --> Detach`

  Detaches the currently debugged process from the underlying debugger; the
  executable continues to run independently.  Use the
  :menuselection:`Debug --> Debug --> Attach To Process` menu to later
  re-attach to this process.


.. index:: menu; debug --> debug --> debug core file
.. index:: core file
.. _core_files:

* :menuselection:`Debug --> Debug --> Debug Core File`

  Opens a file selection dialog allowing you to debug a core file instead
  of a running process.  You must first specify an executable to debug
  before loading a core file.

.. index:: menu; debug --> debug --> kill

* :menuselection:`Debug --> Debug --> Kill`

  Kills the process being debugged.



Data
----

Most items in this menu need to access the underlying debugger when the
process is stopped, not when it is running, so you first need to stop the
process at a breakpoint or interrupt it before using the following
items. Failure to do so will result in empty windows.

.. index:: menu; debug --> data --> data window

* :menuselection:`Debug --> Data --> Data Window`

  Displays the :guilabel:`Data` browser. If it already exists, it's raised
  so it becomes visible

.. index:: menu; debug --> data --> call stack

* :menuselection:`Debug --> Data --> Call Stack`

  Displays the :guilabel:`Call Stack` view.  See :ref:`The_Call_Stack_View`
  for more details.

.. index:: menu; debug --> data --> threads

* :menuselection:`Debug --> Data --> Threads`

  Opens a new window containing the list of threads currently present in
  the executable as reported by the underlying debugger. For each thread,
  it gives language- and debugger-dependent information such as internal
  identifier, name and status.  Refer to the underlying debugger's
  documentation for more details.  Like other similar commands, the process
  being debugged needs to be stopped before using this.  If not, GPS will
  display an empty list.

  When supported by the underlying debugger, clicking on a thread will change
  the context (variables, call stack, source file) displayed, allowing you to
  inspect the stack of the selected thread.


.. index:: menu; debug --> data --> tasks

* :menuselection:`Debug --> Data --> Tasks`

  For :program:`gdb` only, opens a new window containing the list of Ada
  tasks currently present in the executable.  Just like the thread window,
  you can switch to a selected task context by clicking on it, if supported
  by :program:`gdb`. See the :program:`gdb` documentation for the list of
  items displayed for each task.

  .. image:: tasks.jpg

.. index:: protection domain
.. index:: menu; debug --> data --> protection domains

* :menuselection:`Debug --> Data --> Protection Domains`

  For VxWorks AE only, opens a new window containing the list of available
  protection domains in the target. To change to a different protection
  domain, simply click on it. A :samp:`\*` character indicates the current
  protection domain.

.. index:: menu; debug --> data --> assembly
.. index:: assembly

* :menuselection:`Debug --> Data --> Assembly`

  Opens a new window displaying an assembly listing of the current code
  being executed.  See :ref:`The_Assembly_Window` for more details.


.. index:: menu; debug --> data --> edit breakpoints

* :menuselection:`Debug --> Data --> Edit Breakpoints`

  Opens an advanced window to create and modify any kind of breakpoint,
  including watchpoints (see :ref:`The_Breakpoint_Editor`).  For simple
  breakpoint creation, see the description of the source window.

.. index:: menu; debug --> data --> examine memory

* :menuselection:`Debug --> Data --> Examine Memory`

  Opens a memory viewer and editor. See :ref:`The_Memory_View` for more
  details.

.. index:: menu; debug --> data --> command history

* :menuselection:`Debug --> Data --> Command History`

  Opens a dialog with the list of commands executed in the current session.
  Select any number of items in this list to replay the selection.

.. index:: menu; debug --> data --> display local variables

* :menuselection:`Debug --> Data --> Display Local Variables`

  Opens an item in the :guilabel:`Data` browser containing all local
  variables in the current frame.

.. index:: menu; debug --> data --> display arguments

* :menuselection:`Debug --> Data --> Display Argument`

  Opens an item in the :guilabel:`Data` browser containing the arguments
  for the current frame.

.. index:: menu; debug --> data --> display registeres

* :menuselection:`Debug --> Data --> Display Registers`

  Opens an item in the :guilabel:`Data` browser containing the current
  value of the machine registers for the current frame.

.. index:: menu; debug --> Data --> display any expression

* :menuselection:`Debug --> Data --> Display Any Expression...`

  Opens a small dialog letting you specify an arbitrary expression in the
  :guilabel:`Data` browser. This expression can be a variable name or a
  more complex expression, following the syntax of the underlying debugger.
  (See the debugger documentation for more details on the syntax.)  Enable
  the check button :guilabel:`Expression is a subprogram call` if the
  expression is actually a debugger command (e.g, :command:`p/x var`) or a
  procedure call in the program being debugged (e.g, :command:`call
  my_proc`).

.. index:: menu; debug --> data --> recompute

* :menuselection:`Debug --> Data --> Recompute`

  Recomputes and refreshes all items displayed in the :guilabel:`Data`
  browser.


.. index:: debugger; call stack
.. _The_Call_Stack_View:

The Call Stack View
===================

.. image:: call-stack.jpg

The call stack view lists the frames corresponding to the current execution
stack for the current thread or task.

The bottom frame corresponds to the outermost frame (where the thread is
currently stopped). This frame corresponds to the first function executed
by the current thread (e.g, :samp:`main` if the main thread is in C).
Click on any frame to switch to that caller's context; this updates the
display in the source window.  Use the up and down buttons in the tool bar
to go up and down one frame in the call stack.

The contextual menu allows you to choose which information you want to
display in the call stack window (via check buttons):

* :menuselection:`Frame number`:

  The debugger frame number (usually starts at 0 or 1)

* :menuselection:`Program Counter`:

  The machine address corresponding to the function's entry point.

* :menuselection:`Subprogram Name`:

  The name of the subprogram

* :menuselection:`Parameters`:

  The parameters to the subprogram

* :menuselection:`File Location`:

  The filename and line number information.

.. index:: menu; debug --> data --> call stack

By default, only the subprogram name is displayed.  Hide the call stack
view by closing it and show it again using the menu :menuselection:`Debug
--> Data --> Call Stack` menu.

.. index:: debugger; data browser
.. _The_Data_Browser:

The Data Browser
================

Description
-----------

The Data browser is the area in which various information about the process
being debugged is displayed. This includes the value of selected variables,
the current contents of registers, and local variables.

.. index:: debugger; data browser

This browser is open by default when you start the debugger.  Force it to
display through the menu :menuselection:`Debug --> Data --> Data Window`.

.. index:: preferences; debugger --> preserve state on exit

By default, the contents of the data browser is preserved whenever you
close it: if you reopen it either during the same debugger session or
automatically when you start a debugger on the same executable, it displays
the same items as previously. This behavior is controlled by the
:menuselection:`Debugger --> Preserve State on Exit` preference.

The data browser contains all the graphic boxes that can be accessed using
the :menuselection:`Debug --> Data --> Display*` menus, the data browser
:menuselection:`Display Expression...` contextual menu, the editor
:menuselection:`Display` contextual menu items, and the `graph` item in the
debugger console.

In each of these cases, a box is displayed in the data browser with the
following information:

.. image:: canvas.jpg

* A title bar containing:

  * The number of this expression: a positive number starting from 1 and
    incremented for each new box displayed. It represents the internal
    identifier of the box.

  * The name of the expression: this is the expression or variable
    specified when creating the box.

  * An icon representing either a flashlight, or a lock.

    This is a clickable icon that changes the state of the box from
    automatically updated (the flashlight icon) to frozen (the lock icon).
    When frozen, the value is grayed out and doesn't change until you
    change the state. When updated, the value of the box is recomputed each
    time an execution command is sent to the debugger (e.g step, next).

  * An icon representing an 'X'.
    Click on this to close and delete any box.

* A main area.

  The main area displays the data value hierarchically in a
  language-sensitive manner. The browser knows about data structures of
  various languages such as C, Ada, and C++ and organizes them accordingly.
  For example, each field of a record, struct, or class or each element of
  an array is displayed separately. For each subcomponent, a thin box is
  displayed to separate it from other components.

A contextual menu, that takes into account the current component selected
by the pointer, gives access to the following menus:

* :menuselection:`Close *component*`

  Closes the selected item.

* :menuselection:`Hide all *component*`

  Hides all subcomponents of the selected item. To select a particular
  field or element in a record or array, move the pointer over the name of
  the component (not over the box containing its values).

* :menuselection:`Show all *component*`

  Shows all subcomponents of the selected item.

* :menuselection:`Clone *component*`

  Clones the selected component into a new, independent item.

* :menuselection:`View memory at address of *component*`

  Displays the memory view dialog and explores memory at the address of the
  component.

* :menuselection:`Set value of *component*`

  Sets the value of a selected component. This opens an entry box allowing
  you to enter the new value of a variable or component.  The underlying
  debugger does not perform any type or range checking on the value
  entered.

* :menuselection:`Update Value`

  Refreshes the value displayed in the selected item.

* :menuselection:`Show Value`

  Shows only the value of the item.

* :menuselection:`Show Type`

  Shows only the type of each field for the item.

* :menuselection:`Show Value+Type`

  Shows both the value and the type of the item.

* :menuselection:`Auto refresh`

  Enables or disables the automatic refreshing of the item on program
  execution (e.g step, next).

The :guilabel:`Data` browser has a local menu bar containing a number of
useful buttons:

* :guilabel:`Align On Grid`

  Enables or disables alignment of items on the grid.

* :guilabel:`Detect Aliases`

  Enables or disables the automatic detection of shared data structures.
  Each time you display an item or dereference a pointer, the address of
  all items already displayed on the canvas are compared with the address
  of a new item to display. If they match (for example, if you tried to
  dereference a pointer to an object already displayed), GPS will display a
  link instead of creating a new item.

:menuselection:`Zoom in`

  Redisplays the items with a bigger font.

* :guilabel:`Zoom out`

  Displays the items with smaller fonts and pixmaps. Use this when you have
  several items in the browser and you can't see all of them at the same
  time (for example, a tree whose structure you want to see clearly).

* :guilabel:`Zoom`

  Choose the zoom level directly from a menu.

* :guilabel:`Clear`

  All the boxes currently displayed are removed.


Manipulating items
------------------

Moving items
^^^^^^^^^^^^

You can manipulated all the items with your mouse.  You can move them
anywhere within the browser.  If you try to move an item outside of the
visible area of the browser, GPS scroll it to make the new position
visible.

GPS also provides automatic scrolling if you move the pointer while
dragging an item near the borders of the browser.  While the pointer
remains close to the border and the mouse is pressed while hovering on the
item, GPS scrolls the browser and moves the item. This provides an easy way
to move an item a long distance from its initial position.

Colors
^^^^^^

Most of the items are displayed using several colors, each conveying a
special meaning.  The default the meaning of each colors is as follows
(the colors can be changed through the preferences dialog):

.. image:: colors.jpg

*black*

  The default color used to print the value of variables or expressions.

*blue*
  .. index:: C
  .. index:: Ada

  used for C pointers (or Ada access values), i.e. all the variables and
  fields that are memory addresses that denote some other value in memory.

  You can dereference these (that is to say see the value pointed to) by
  double-clicking on the blue text itself.

*red*

  Used for variables and fields whose value has changed since the data
  window was last displayed. For example, if you display an array in the
  data browser and then select the :guilabel:`Next` button in the tool bar,
  the elements of the array whose value has just changed appear in red.

  As another example, if you choose to display the value of local variables
  in the data window (:menuselection:`Display --> Display Local
  Variables`), only the variables whose value has changed are highlighted;
  the others remain black.

Icons
^^^^^

Several different icons can be seen when displaying items. They convey the
following special meanings:

*trash bin icon*

  Indicates the debugger couldn't get the value of the variable or
  expression.  For example, because the variable is currently not in scope
  (and thus does not exist) or might have been optimized away by the
  compiler. In all cases, the display is updated as soon as the variable's
  value is known again.

*package icon*

  Indicates part of a complex structure is currently hidden.  Manipulating
  huge items in the data window (for example if the variable is an array of
  hundreds of complex elements) might not be very helpful. As a result, you
  can shrink part of the value to save some screen space and make it easier
  to visualize the interesting parts of these variables.

  Double-clicking on icon expands the hidden part and clicking on any
  subrectangle in the display of the variable hides that part and replace
  it with this icon.

  See also the description of the contextual menu to automatically show or
  hide all the contents of an item.  An alternative to hiding subcomponents
  is to clone them in a separate item (see the contextual menu).

.. index:: breakpoint editor
.. index:: breakpoint
.. _The_Breakpoint_Editor:

The Breakpoint Editor
=====================

.. image:: breakpoints.jpg

.. index:: menu; debug --> data --> edit breaakpoints

Access the breakpoint editor from the :menuselection:`Debug --> Data -->
Edit Breakpoints` menu.  It allows you to manipulate the various kinds of
breakpoints: those at a source location, on a subprogram, at an executable
address, on memory access (watchpoints), or on Ada exceptions.

Double-click on any breakpoint in the list to open the corresponding source
editor at the corresponding location.  Or select the breakpoint and then
click the :guilabel:`View` button.

The top area provides an interface to create the different kinds of
breakpoints, while the bottom area lists existing breakpoints and their
characteristics.

To access advanced breakpoint characteristics for a given breakpoint select
the breakpoint from the list and click on the :guilabel:`Advanced` button,
which displays a new dialog window where you can specify commands to run
automatically after a breakpoint is hit or specify how many times the
breakpoint will be ignored.  If running VxWorks AE, you can also change the
Scope and Action settings for breakpoints.

.. image:: bp-advanced.jpg
.. index:: VxWorks AE

Scope and Action Settings for VxWorks AE
----------------------------------------

In VxWorks AE breakpoints have two extra properties:

* Scope:

  Which task(s) will be stopped at a given breakpoint. Possible values are:

  * task:

    The breakpoint only affects the task that was active when the
    breakpoint was set. If the breakpoint is set before the program is run,
    the breakpoint affects the environment task

  * pd:
    .. index:: protection domain

    Any task in the current protection domain is affected by the breakpoint

  * any:

    Any task in any protection domain is affected by the breakpoint. This
    setting is only allowed for tasks in the Kernel domain.

* Action:

  When a task hits a breakpoints, which tasks are stopped:

  * task: only the task that hit the breakpoint.

  * pd: all tasks in the current protection domain

  * all: all stoppable tasks in the system

You set of change these properties through the advanced breakpoints
characteristics by clicking on the :guilabel:`Advanced` button. There are
two ways of setting these properties:

* Per breakpoint settings:

  After setting a breakpoint (the default Scope or Action values are both
  :samp:`task`), select the :guilabel:`Scope/Action` tab in the
  :guilabel:`Advanced` settings.  To change these settings for a specific
  breakpoint, select it from the breakpoints list, select the desired
  values of Scope and Action, and click on the :guilabel:`Update` button.

* Default session settings:

  Select the :guilabel:`Scope/Action` tab in the :guilabel:`Advanced`
  settings, select the desired Scope and Action settings, check the
  :guilabel:`Set as session defaults` check box and click the
  :guilabel:`Close` button. From then on, every new breakpoint will have
  the specified values for Scope and Action.

.. index:: saving breakpoints
.. index:: breakpoints, saving
.. index:: preferences; debugger --> preserve state on exit

If you enabled the preference :menuselection:`Debugger --> Preserve state
on exit`, GPS automatically saves the currently set breakpoints and
restores them the next time you debug the same executable. This allows you
to immediately start debugging your application without having to set the
breakpoints every time.

.. index:: memory view
.. _The_Memory_View:

The Memory View
===============

.. image:: memory-view.jpg

The memory view allows you to display the contents of memory by specifying
either an address or a variable name.

.. index:: C
.. index:: hexadecimal

To display memory contents, enter either the address using the C
hexadecimal notation (0xabcd) or the name of a variable in the
:guilabel:`Location` text entry.  (If a variable is entered, the underlying
debugger computes its address.)  Then either press :kbd:`Enter` or click
the :guilabel:`View` button. GPS displays the memory with the corresponding
addresses in the bottom text area.

.. index:: ASCII

Specify the unit size (:guilabel:`Byte`, :guilabel:`Halfword` or
:guilabel:`Word`) and the format (:guilabel:`Hexadecimal`,
:guilabel:`Decimal`, :guilabel:`Octal` or :guilabel:`ASCII`) and you can
display the corresponding ASCII value at the same time.

The :kbd:`up` and :kbd:`down` arrows as well as the :kbd:`Page up` and
:kbd:`Page down` keys in the memory text area allow you to walk through the
memory in order of ascending or descending addresses respectively.

Finally, modify a memory area by clicking on the location you want to
modify and entering the new values. Modified values appear in a different
color (red by default) and are only be written to the target when you click
on the :guilabel:`Submit changes` button. Clicking on :guilabel:`Undo
changes` or going up or down in the memory also undoes your editing.

Clicking on :guilabel:`Close` closes the memory window, canceling your last
pending changes, if any.

.. _Using_the_Source_Editor_when_Debugging:

Using the Source Editor when Debugging
======================================

When debugging, the left area of each source editor provides the following
information:

*Lines with code*

  Blue dots are shown next to lines for which the debugger has debug
  information, i.e., lines that have been compiled with debug information
  and for which the compiler has generated some code.  If you try to set a
  breakpoint on lines not so marked, GPS send the breakpoint command to the
  underlying debugger, which usually (e.g in the case of :program:`gdb`)
  results in setting a breakpoint at the closest location to the file and
  line you specified.

*Current line executed*

  A green arrow showing the line about to be executed.

*Lines with breakpoints*
  .. index:: breakpoint

  A red mark is displayed on top of the blue dot on lines where breakpoints
  have been set.  Add or delete breakpoints by clicking on this area (the
  first click sets a breakpoint, the second click removes it).

.. image:: tooltips.jpg

.. index:: syntax highlighting
.. index:: tooltip

The second area in the source editor is a text window on the right that
displays the source files, with syntax highlighting.  If you hold the
pointer over a variable, GPS displays a tooltip showing the value of that
variable.  Disable these automatic tooltips using the preferences menu.
(See :ref:`Preferences Dialog <preferences_dialog>`.)

When the debugger is active, the contextual menu of the source window
contains a :menuselection:`Debug` submenu providing the entries
below. These entries are dynamic and apply to the entity under the pointer
(depending on the current language). In addition, if you've made a
selection in the editor, the text of the selection is used instead. This
allows you to easily display complex expressions (for example, you can add
comments to your code with expressions you want to display in the
debugger).

* :menuselection:`Debug --> Print *selection*`

  Prints the selection (or by default the name under the pointer) in the
  debugger console.

* :menuselection:`Debug --> Display *selection*`

  Displays the selection (or by default the name under the pointer) in the
  data browser. GPS automatically refreshes this value each time the
  process state changes (e.g after a step or a next command). To freeze the
  display, click on the corresponding icon in the browser or use the
  contextual menu for that item (see :ref:`The_Data_Browser`).

* :menuselection:`Debug --> Print *selection*.all`

  Dereferences the selection (or by default the name under the pointer) and
  prints the value in the debugger console.

* :menuselection:`Display *selection*.all`

  Dereferences the selection (or by default the name under the pointer) and
  displays the value in the data browser.

* :menuselection:`View memory at address of *selection*`

  Brings up the memory view dialog and explores memory at the address of
  the selection.

* :menuselection:`Set Breakpoint on Line *xx*`

  Sets a breakpoint on the line under the pointer.

* :menuselection:`Set Breakpoint on *selection*`

  Sets a breakpoint at the beginning of the subprogram named *selection*

* :menuselection:`Continue Until Line *xx*`

  Continues execution (the program must have been started previously) until
  it reaches the specified line.

* :menuselection:`Show Current Location`

  Jumps to the current line of execution. This is particularly useful after
  navigating through your source code.

.. _The_Assembly_Window:

The Assembly Window
===================

It's sometimes convenient to look at the assembly code for the subprogram
or source line you're currently debugging.


.. index:: menu; debug --> data --> assembly

Open the assembly window by using the :menuselection:`Debug --> Data -->
Assembly` menu.

.. image:: assembly.jpg

The current assembler instruction is highlighted on the left with a green
arrow.  The instructions corresponding to the current source line are
highlighted (by default in red). This allows you to easily see where the
program counter will point after you press the :guilabel:`Next` button on
the tool bar.

Move to the next assembler instruction using the :guilabel:`Nexti` (next
instruction) button in the tool bar. If you choose :guilabel:`Stepi`
instead (step instruction), it steps into any subprogram being called by
that instruction.

For efficiency purposes, GPS only displays a small part of the assembly
code around the current instruction.  Specify how many instructions are
displayed in the :ref:`Preferences Dialog <preferences_dialog>`.  Display
the instructions immediately preceding or following the currently displayed
instructions by pressing one of the :kbd:`Page up` or :kbd:`Page down` keys
or using the contextual menu in the assembly window.

.. index:: menu; debug --> data --> display registers

A convenient complement when debugging at the assembly level is the ability
to display the contents of machine registers.  When the debugger supports
it (as :program:`gdb` does), select the :menuselection:`Debug --> Data -->
Display Registers` menu to get an item in the data browswer that shows the
current contents of each machine register and that's updated every time one
of them changes.

.. index:: menu; debug --> Data --> display any expression

You might also choose to look at a single register.  With :program:`gdb`,
select the :menuselection:`Debug --> Data --> Display Any Expression` menu,
enter something like::

  output /x $eax

in the field and select toggle button :guilabel:`Expression is a subprogram
call`. This creates a new browser item that's refreshed every time the
value of the register (in this case :command:`eax`) changes.


.. index:: debugger console
.. _The_Debugger_Console:

The Debugger Console
====================

The debugger console is the text window located at the bottom of the main
window.  It gives you direct access to the underlying debugger, to which
you can send commands (you need to refer to the underlying debugger's
documentation, but usually typing "help" will gives you an overview of the
available commands).

If the underlying debugger allows it, pressing :kbd:`Tab` in this window
provides completion for the command being typed (or its arguments).

additional commands are defined here to provide a simple text interface to
some graphical features.  Here's the complete list of such commands. The
arguments between square brackets are optional and can be omitted.


*graph (print|display) expression [dependent on display_num] [link_name name] [at x, y] [num num]*

  .. index:: graph print
  .. index:: graph display

  Create a new item in the browser showing the value of :samp:`Expression`,
  which is the name of a variable, or one of its fields, in the current
  scope for the debugger.  The command `graph print` creates a frozen item,
  one that is not automatically refreshed when the debugger stops, while
  :samp:`graph display` displays an item that's automatically refreshed.

  The new item is associated with a number displayed in its title bar.
  This number can be specified with the :samp:`num` keyword and can be used
  to create links between the items, using the second argument to the
  command, :samp:`dependent on`. By specifying the third argument, the link
  itself (i.e. the line) can be given a name that is also displayed.

*graph (print|display) `command`*

  Similar to the above, except you use it to display the result of a
  debugger command in the browser.  For example, using :program:`gdb`, if
  you want to display the value of a variable in hexadecimal rather than
  the default decimal, use a command like::

    graph display `print /x my_variable`

  This evaluates the command between back-quotes every time the debugger
  stops and displays the result in the browser. The lines that have changed
  are automatically highlighted (by default, in red).

*graph (enable|disable) display display_num [display_num ...]*

  .. index:: graph enable
  .. index:: graph disable

  Change the refresh status of items in the canvas. As explained above,
  items are associated with a number visible in their title bar.

  The :command:`graph enable` command forces the item to be refreshed
  automatically every time the debugger stops and :command:`graph disable`
  freezes the item, preventing its display from being changed.

*graph undisplay display_num*

  .. index:: graph undisplay

  Remove an item from the browser.

.. _Customizing_the_Debugger:

Customizing the Debugger
========================

GPS is a high-level interface to several debugger backends, in particular
:program:`gdb`.  Each back end has its own advantages, but you can enhance
the command line interface to these backends through GPS by using Python.

This section provide a small such example whose goal is to provide the
notion of "alias" in the debugger console. For example, this can be used so
that you if type "foo", it executes a longer command, like displaying the
value of a variable with a long name.  :program:`gdb` already provides this
feature through the :command:`define` keywords, but here we implement that
feature using python in GPS.

GPS provides an extensive Python API to interface with each of the running
debuggers. In particular, it provides the function "send", used to send a
command to the debugger and get its output, and the function "set_output",
used when you implement your own functions.

It also provides, through :samp:`hook`, the capability to monitor the state
of the debugger back-end. In particular, one such hook,
:samp:`debugger_command_action_hook` is called when the user typed a
command in the debugger console and before the command is executed. This
can be used to add your own commands. The example below uses this hook.

.. highlight:: python

Here's the code::

  import GPS

  aliases={}

  def set_alias (name, command):
     """Set a new debugger alias. Typing this alias in a debugger window
        will execute command"""
     global aliases
     aliases[name] = command

  def execute_alias (debugger, name):
     return debugger.send (aliases[name], output=False)

  def debugger_commands (hook, debugger, command):
     global aliases
     words = command.split()
     if words[0] == "alias":
        set_alias (words[1], " ".join (words [2:]))
        return True
     elif aliases.has_key (words [0]):
        debugger.set_output (execute_alias (debugger, words[0]))
        return True
     else:
        return False

  GPS.Hook ("debugger_command_action_hook").add (debugger_commands)

The list of aliases is stored in the global variable :command:`aliases`,
which is modified by :command:`set_alias`. Whenever the user executes an
alias, the real command is sent to the debugger through
:command:`execute_alias`.

The real work is done by `debugger_commands`. If the you execute the
:command:`alias` command, it defines a new alias. Otherwise, if you type
the name of an alias, we want to execute that alias.  Otherwise, we let the
underlying debugger handle that command.

After you copied this example in the :file:`$HOME/.gps/plug-ins` directory,
start a debugger as usual in GPS, and type the following in its console::

     (gdb) alias foo print a_long_long_name
     (gdb) foo


The first command defines the alias, the second line executes it.

This alias can also be used within the :command:`graph display` command so
the value of the variable is displayed in the data window, for example::

     (gdb) graph display `foo`

You can also program other examples. You could write complex python
functions, which would, for example, query the value of several variables
and pretty-print the result.  You can call any of these complex python
functions from the debugger console or have it called automatically every
time the debugger stops via the :command:`graph display` command.
