.. index:: MDI
.. index:: seealso: Multiple Document Interface; MDI
.. _Multiple_Document_Interface:

***************************
Multiple Document Interface
***************************

All windows (whether editors or views) that are part of the GPS environment
are under control of what is commonly called a multiple document interface
(MDI for short). This is a common paradigm where related windows are put
into a larger window which is itself under control of the system or the
windows manager.

By default, no matter how many editors and views you opened, your system
still sees only one window (on Windows systems, the task bar shows only one
icon). You can organize the GPS windows in whatever way you want, all
inside the GPS main window.  This section describes the capabilities GPS
provides to help you do this.


Window layout
=============

The GPS main window is organized into various areas.  The most important
distinction is between the central area (which usually occupies the most
space) and the side areas (which include the top and bottom areas).  Some
windows in the GPS area are restricted to either the central or the side
areas.  You can split each area into smaller areas, as described below.
Each area can contain any number of windows, organized into notebooks with
tabs, possibly displaying names.  Right-clicking on a tab displays a
contextual menu providing capabilities showing the :guilabel:`Tabs
location` and :guilabel:`Tabs rotation`, which allows you to display the
tabs on any side of the notebook or make them vertical if you want to save
screen space.

The central area can contain the source editors (which can only go there)
as well as larger views like browsers.  The contents of the central area are
preserved when switching perspectives (see below).

.. index:: MDI; selecting windows
.. _Selecting_Windows:

Selecting Windows
=================

Only one window is selected in GPS (the **active window**) at a time.
Select a window by clicking on its tab, which becomes a different
color, or selecting its name in the :menuselection:`Window` menu.  Or use
the :guilabel:`Windows` view (see :ref:`The_Windows_View`), which also provides
a convenient mechanism for selecting multiple windows at once.

Finally, you can select windows with the omni-search, the search field in
the global toolbar. One of the contexts for the search is the list of
opened windows. To make things more convenient, you can bind a key shortcut
via the :menuselection:`Edit --> Preferences...` menu (the name of the
action is :menuselection:`Search --> Global Search in context: Opened`).

Any window whose name contains the specified letter matches the search field.
For example, if you are currently editing the files :file:`unit1.adb` and
:file:`file.adb`, pressing :kbd:`t` leaves only :file:`unit1.adb` selectable.

.. index:: MDI; closing windows
.. _Closing_Windows:

Closing Windows
===============

Wherever a window is displayed, you can close it by clicking the small
:guilabel:`X` icon in its tab or selecting the window by clicking
on its tab and selecting the :menuselection:`Window --> Close` menu.

When you close a window, the focus is set to the window in the same
notebook that previously had the focus. If you open an editor as a result
of a cross-reference query, close that editor to go back to where you were.

.. index:: menu; window --> close

Finally, you can close a window by right-clicking in the associated
notebook tab (if the tabs are visible) and selecting :guilabel:`Close` in
the contextual menu.

There is a :menuselection:`Close all other editors` menu in the notebook tab
when you are in an editor, which closes most windows except a single editor,
the one you are using.


.. index:: menu; windows --> split horizontally
.. index:: menu; windows --> split vertically
.. _Splitting_Windows:

Splitting Windows
=================

You can split windows horizontally and vertically in any combination.  To
do this requires at least two windows (for example text editors or
browsers) present in a given notebook.  Select either the
:menuselection:`Window --> Split Horizontally` or :menuselection:`Window
--> Split Vertically` menus to split the selected window. In the left
(respectively, top) pane, the currently selected window is put on its
own. The rest of the previously superimposed windows are put in the right
(respectively, bottom) pane. You can further split these remaining windows
to achieve any desired layout.

You can resize any split windows by dragging the handles that separate
them.

You may want to bind the key shortcuts to the :menuselection:`Window -->
Split Horizontally` and :menuselection:`Window --> Split Vertically` menus
using the key manager. If you want to achieve an effect similar to standard
Emacs behavior (where :kbd:`control-x 2` splits a window horizontally and
:kbd:`control-x 3` splits a window vertically), use the key manager
(see :ref:`The_Key_Shortcuts_Editor`).

:ref:`Moving_Windows` shows how to split windows using drag-and-drop, which
is the fastest way.

You can put several editors or browsers in the same area. In that case,
they are grouped together in a notebook; select any of them by clicking on
the corresponding tab.  If there are many windows, two small arrows appear
on the right of the tabs.  Click these arrows to show the remaining tabs.

GPS changes the color and size of the title (name) of a window in the
notebook tab to indicate that the window content has been updated but the
window is not visible.  This commonly occurs when new messages have been
written in the :guilabel:`Messages` or :guilabel:`Console` views.


.. index:: MDI; floating windows
.. _Floating_Windows:

Floating Windows
================

You may prefer to have several top-level windows under direct control of
your system's window manager.  For example, you want to benefit from some
options your system might provide such as virtual desktops, different
window decoration depending on the window's type, transparent windows,
and/or multiple screens.

.. index:: menu; window --> floating

You can make any window currently embedded in the MDI a **floating window**
by selecting the window and selecting the :menuselection:`Window -->
Floating` menu. The window is detached and you can move it anywhere on your
screen, even outside GPS's main window.

There are two ways to put a floating window back under control of GPS.  The
most general method is to select the window using its title in the
:menuselection:`Window` menu, and unselect :menuselection:`Window -->
Floating`.

.. index:: preferences; windows --> destroy floats

The second method assumes you have set the preference
:guilabel:`Destroy Floats` in the :menuselection:`Edit -->
Preferences...` menu to false.  If so, you can close the floating window
by clicking the close button in the title bar; the window is put back
in GPS's main windows. If you want to close the window, you need to
click the cross button in its title bar a second time.

.. index:: preferences; windows --> all floating

GPS provides a mode where all windows are floating and the MDI area in the
main window is invisible.  You may want to use this if you rely on windows
handling facilities supported by your system or window manager that are not
available in GPS, for example if you want to have windows on various
virtual desktops and your window manager supports this.

This mode is activated through the :menuselection:`Windows --> All
Floating` preference.


.. index:: drag-and-drop
.. _Moving_Windows:

Moving Windows
==============

Change the organization of windows at any time by selecting a notebook
containing several editors or browsers and selecting one of the
:menuselection:`Split` entries in the :menuselection:`Window` menu.

You can also drag and drop the window within GPS.  Select an item
to drag by selecting the notebook tab.  In that case, you can also
reorder the windows within the notebook: select the tab, then start moving
left or right to the window's new position.  Your mouse must remain within
the tab area or GPS will drop the window into another notebook.

Here are the various places where you can drop a window:

* Inside the MDI

  While the mouse button is pressed, the target area is highlighted and
  shows where the window would be put if you release the mouse button. The
  background color of the highlight indicates whether the window will be
  preserved or not when
  changing perspectives (for example, when starting a debug session). You
  can drag a window to one side of a notebook to split that notebook.

  If you drop a window all the way on a side of the area, the window will
  occupy the full width (or height) of the area.

  GPS will however restrict where windows can be placed: editors and most
  browsers, for instance, must go into the central area (the part that stays
  common when switching perspectives), whereas other views must stay on the
  sides (left, right, bottom or top) of that central area. 
  The color of the highlight during a move (blue or brown) will indicate where
  the window can be dropped.

* System window

  If you drop a window outside of GPS (for example, on the background of
  your screen), GPS floats the window.

.. index:: cloning editors

Keeping the :kbd:`shift` key pressed while dropping the window results in a
copy operation instead of a simple move, if possible. For example, if you
drop an editor, a new view of the same editor is created, resulting in two
views: the original one at its initial location and a second at the new
location.

If you keep the :kbd:`control` key pressed while dropping the window, all
the windows in the same notebook are moved, instead of just the one you
selected.  This is the fastest way to move a group of windows to a new
location.


.. index:: perspectives
.. index:: MDI; perspectives
.. _Perspectives:

Perspectives
============

GPS supports the concept of perspectives. These are activity-specific
desktops, each with their own set of windows, but sharing some common
windows like the editors.

You can switch to a different perspective for different types of activities
you want to perform (such as debugging or version control operations).  For
example, when using the debugger, the default perspective consists of
windows containing the call stack, data window, and the debugger console,
each at the location you have set.  When you start the debugger again, you
do not have to reopen these windows.

.. index:: menu; window --> perspectives
.. index:: menu; window --> perspectives --> create new

Each perspective has a name.  Switch perspectives by selecting the
:menuselection:`Window --> Perspectives` menu.  Create a new perspective by
selecting the :menuselection:`Window --> Perspectives --> Create New` menu.

The most convenient way to change perspective, though, is to simply click
on the button to the right of the main toolbar. By default, it shows the
label "Default", which is the name of the default perspective. Selecting
any item in the popup window will switch to that perspective.

GPS sometimes automatically changes perspectives. For example, if you start
a debugger, it switches to the perspective called :guilabel:`Debug` if one
exists. When the debugger terminates, you are switched back to the
:guilabel:`Default` perspective, if one exists.

When you leave a perspective, GPS automatically saves its contents
(including which windows are opened and their location) so when you return
to the same perspective you see the same layout.

.. index:: preferences; general --> save desktop on exit

When GPS exits, it saves the layout of all perspectives to a file
:file:`perspectives6.xml` so it can restore them when you restart GPS. This
behavior is controlled by the :menuselection:`General --> Save desktop on
exit` preference, which you can disable.

One difficulty in working with perspectives is knowing which windows are
preserved when you switch to another perspective and which are hidden.  To
help you determine this, there's a central area where you can find all
preserved windows.  It usually only contains editors (including those that
you have split side by side). If you drop another window on top or to one
side of an editor, that window is preserved when changing perspectives
unless it is already in the new perspective.  The color of the highlight
appearing on the screen while you drag tells you whether the window (if
dropped at the current location) will be visible or hidden in other
perspectives.

