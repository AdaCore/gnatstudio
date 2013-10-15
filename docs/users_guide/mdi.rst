.. index:: MDI
.. index:: seealso: Multiple Document Interface; MDI
.. _Multiple_Document_Interface:

***************************
Multiple Document Interface
***************************

All the windows that are part of the GPS environment are under control of what
is commonly called a multiple document interface (MDI for short). This is a
common paradigm on windowing systems, where related windows are put into a
bigger window which is itself under control of the system or the windows
manager.

This means that, by default, no matter how many editors, browsers, views, ...
windows you have opened, your system will still see only one window (On Windows
systems, the task bar shows only one icon). However, you can organize the GPS
windows exactly the way you want, all inside the GPS main window.

This section will show the various capacities that GPS provides to help you
organize your workspace.


Window layout
=============

The desktop area is organized into various area.

The most important distinction is that of the central area (which in general
occupies the most space) and the side areas.

The central area is meant to contain the source editors (which
in fact can only go into that area) as well as the bigger views like the
browsers. The property of the central area is that its contents is
preserved when switching perspectives (see below).

Some windows in GPS area restricted to either the central or the side
areas.

Each of these areas can be further split into smaller area, as will be
described below. They can contain any number of window, which are then
organized into notebooks (with tabs that can show the names).

Right-clicking on the tab displays a contextual menu that provides various
capabilities, including showing the :guilabel:`Tabs location` and
:guilabel:`Tabs rotation`. This allows you to display the tabs on any
side of the notebook, and make them vertical if you want to save screen
space.



.. index:: MDI; selecting windows
.. _Selecting_Windows:

Selecting Windows
=================

At any time, there is only one selected window in GPS (the **active window**).
You can select a window either by clicking in its title bar, which will then
get a different color, or by selecting its name in the menu :menuselection:`Window`.

An alternative is to use the :guilabel:`Windows` view (see :`The_Windows_View`),
which also provides a convenient solution to close multiple windows at once.

Alternatively, windows can be selected with the omni-search, that is the search
field in the global toolbar. One of the contexts where the search is done is the
list of opened window. To make things more convenient, you can bind a key
shortcut via the :menuselection:`Edit --> Key Shortcuts` menu (the name of the
action is :menuselection:`Search --> Global Search in context: Opened`. If you
load the :file:`emacs.py` plugin, a standard key binding is set for
:kbd:`control-xb`.

The filter is matched by any window whose name contains the letter you have
typed. For instance, if you are currently editing the files :file:`unit1.adb`
and :file:`file.adb`, pressing :kbd:`t` will only leave :file:`unit1.adb`
selectable.


.. index:: MDI; closing windows
.. _Closing_Windows:

Closing Windows
===============

Wherever the windows are displayed, they are always closed in the same manner,
by clicking on the small :guilabel:`X` icon in their tab.

.. index:: preferences; windows --> show title bars

If you have chosen to display the title bars for the windows, you can also
click either in the :guilabel:`X` button in the title bar, or double-click
on the icon to the left of the title bar (when there is such an icon).

When a window is closed, the focus is given to the window in the same notebook
that previously had the focus.  Therefore, if you simply open an editor as a
result of a cross-reference query, you can simply close that editor to go back
to where you were before.

.. index:: menu; window --> close

Alternatively, you can also select the window by clicking anywhere in its title
bar, and then select the menu :menuselection:`Window --> Close`.

Finally, a window can be closed by right-clicking in the associated notebook
tab (if the tabs are visible), and select :guilabel:`Close` in the contextual
menu.

In the notebook tab (when you are in an editor), you will also find a
:guilabel:`Close all other editors` menu, which, as its name implies, will keep
a single editor open, the one you are clicking on.



.. index:: menu; windows --> split horizontally
.. index:: menu; windows --> split vertically
.. _Splitting_Windows:

Splitting Windows
=================

Windows can be split at will, through any combination of horizontal and
vertical splits.  This feature requires at least two windows (text editors,
browsers, ...) to be superimposed in a given notebook. Selecting either the
:menuselection:`Window --> Split Horizontally` or :menuselection:`Window --> Split
Vertically` menus will then split the selected window in two. In the left
(resp. top) pane, the currently selected window will be left on its own. The
rest of the previously superimposed windows will be put in the right (resp.
bottom) pane. You can then in turn split these remaining windows to achieve any
layout you want.

All split windows can be resized interactively by dragging the handles that
separate them. A preference (menu :menuselection:`Edit --> Preferences`) controls
whether this resizing is done in opaque mode or border mode. In the latter
case, only the new handle position will be displayed while the mouse is
dragged.

You may want to bind the key shortcuts to the menus
:menuselection:`Window --> Split Horizontally` as well as
:menuselection:`Window --> Split Vertically` using the key manager. In addition,
if you want to achieve an effect similar to e.g. the standard Emacs behavior
(where :kbd:`control-x 2` splits a window horizontally, and :kbd:`control-x 3`
splits a window vertically), you can use the key manager
(:ref:`The_Key_Manager_Dialog`).

:ref:`Moving_Windows` will show how to do the splitting through drag-and-drop
and the mouse, which in general is the fastest way to do.

Several editors or browsers can be put in the same area of the MDI. In such a
case, they will be grouped together in a notebook widget, and you can select
any of them by clicking on the corresponding tab. Note that if there are lots
of windows, two small arrows will appear on the right of the tabs.  Clicking on
these arrows will show the remaining tabs.

In some cases GPS will change the color and size of the title (name) of a
window in the notebook tab. This indicates that the window content has been
updated, but the window wasn't visible. Typically, this is used to indicate
that new messages have been written in the messages or console window.


.. index:: MDI; floating windows
.. _Floating_Windows:

Floating Windows
================

Although the MDI, as described so far, is already extremely flexible, it is
possible that you prefer to have several top-level windows under direct control
of your system or window manager. This would be the case for instance if you
want to benefit from some extra possibilities that your system might provide
(virtual desktops, different window decoration depending on the window's type,
transparent windows, multiple screens, ...).

.. index:: menu; window --> floating

GPS is fully compatible with this behavior, since windows can also be
**floating windows**. Any window that is currently embedded in the MDI can be
made floating at any time, simply by selecting the window and then selecting
the menu :menuselection:`Window --> Floating`. The window will then be detached,
and can be moved anywhere on your screen, even outside of GPS's main window.

There are two ways to put a floating window back under control of GPS.  The
more general method is to select the window through its title in the menu
:menuselection:`Window`, and then unselect :menuselection:`Window --> Floating`.

.. index:: preferences; windows --> destroy floats

The second method assumes that the preference :guilabel:`Destroy Floats` in the
menu :menuselection:`Edit --> Preferences` has been set to false. Then, you can
simply close the floating window by clicking in the appropriate title bar
button, and the window will be put back in GPS. If you actually want to close
it, you need to click once again on the cross button in its title bar.

.. index:: preferences; windows --> all floating

A special mode is also available in GPS, where all windows are floating. The
MDI area in the main window becomes invisible. This can be useful if you rely
on windows handling facilities supported by your system or window manager but
not available in GPS. This might also be useful if you want to have windows on
various virtual desktops, should your window manager support this.

This special mode is activated through the :menuselection:`Windows --> All Floating`
preference.


.. index:: drag-and-drop
.. _Moving_Windows:

Moving Windows
==============

As we have seen, the organization of windows can be changed at any time by
selecting a notebook containing several editors or browsers, and selecting one
of the Split menus in the :menuselection:`Window` menu.

A more intuitive method is also provided, based on the drag-and-drop paradigm.
The idea is simply to select a window, wherever it is, and then, by clicking on
it and moving the mouse while keeping the left button pressed, drop it anywhere
else inside GPS.

Selecting an item so that it can be dragged is done simply by clicking with the
left mouse button in its title bar, and keep the button pressed while moving
the mouse.

If the window is inside a notebook, you can also choose to select the notebook
tab to start dragging the window around. In such a case, the windows within the
notebook can also be reordered: select the tab, then start moving left or right
to the new position the window should have. Note that your mouse must remain
within the tab area, since otherwise GPS will enter in the mode where the
window can be put in other notebooks.

Here are the various places where a window can be dropped:

* Inside the MDI

  While you keep the mouse button pressed, and move the mouse around, the target
  area is highlighted. This shows precisely where the window would be put if you
  were to release the mouse button at that point. The background color of the
  highlight indicates whether the window will be preserved (if the color is the
  same as the title bar) or not when changing perspectives (for instance when
  starting a debug session). You can drag to one of the sides of a notebook
  to split that notebook.

  If you move your mouse all the way to the side of the desktop, and then drop
  the window, that window will occupy the full width (resp. height) of the
  desktop on that side.

* System window

  If you drop a window outside of GPS (for instance, on the background of your
  screen), the window will be floated.

.. index:: cloning editors

If you maintain the :kbd:`shift` key pressed while dropping the window, this
might result in a copy operation instead of a simple move. For instance, if you
are dropping an editor, a new view of the same editor will be created,
resulting in two views present in GPS: the original one is left at its initial
location, and a second view is created at the new location.

If you maintain the :kbd:`control` key pressed while dropping the window, all
the windows that were in the same notebook are moved, instead of the single one
you selected. This is the fastest way to move a group of windows to a new
location, instead of moving them one by one.


.. index:: perspectives
.. index:: MDI; perspectives
.. _Perspectives:

Perspectives
============

GPS supports the concept of perspectives. These are activity-specific desktops,
each with their own set of windows, but sharing some common windows like the
editors.

Depending on the activity you want to perform (debugging, version control,...)
you could switch to another perspective. For instance, in the context of the
debugger, the new perspective would by default contain the call stack window,
the data window, the debugger consoles,... each at your favorite location.
Whenever the debug starts, you therefore do not have to open these windows
again.

.. index:: menu; window --> perspectives
.. index:: menu; window --> perspectives --> create new

The perspectives have names, and you switch perspectives by selecting the menu
:menuselection:`/Window/Perspectives/`. You can also create a new perspective
by selecting the menu :menuselection:`/Window/Perspectives/Create New`.

GPS will sometimes automatically change perspectives. For instance, if you
start a debugger, it will switch to the perspective called :guilabel:`Debug`
(if it exists). When the debugger terminates, you are switched back to the
"Default" perspective (again, if it exists).

When you leave a perspective, GPS automatically saves its contents (which
windows are opened, their location,...), so that when you are going back to the
same perspective you find the same layout.

.. index:: preferences; general --> save desktop on exit

Likewise, when GPS exits, it will save the layout of all perspectives into a
file called :file:`perspectives6.xml`, so that it can restore them when you
restart GPS. This behavior is controlled by the :menuselection:`General --> Save
desktop on exit` preference, and can be disabled.

One of the difficulties in working with perspectives is knowing which windows
will be preserved when you switch to another perspective, and which windows
will be hidden. There is a central area where all preserved windows are found.
Typically, it only contains editors (including if you have split them side by
side for instance). If you drag and drop another window on top or to the sides
of an editor, that window will be preserved when changing perspectives, unless
it was already found elsewhere in the new perspective.  The color of the
highlight that appears on the screen while you drag and drop will tell you
whether the window (if dropped at the current location) will be visible in
other perspectives or not.
