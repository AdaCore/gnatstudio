.. _Multiple_Document_Interface:

***************************
Multiple Document Interface
***************************

.. index:: MDI
.. index:: Multiple Document Interface
.. index:: window manager
.. index:: work space

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

.. _Selecting_Windows:

Selecting Windows
=================

.. index:: window selection

At any time, there is only one selected window in GPS (the **active window**).
You can select a window either by clicking in its title bar, which will then
get a different color, or by selecting its name in the menu `Window`.

Alternatively, windows can be selected with the keyboard. By default, the
selection key is :kbd:`Alt-Tab`. When you press it, a temporary dialog is
popped-up on the screen, with the name of the window that will be selected when
the key is released. If you press the selection key multiple times, this will
iterate over all the windows currently open in GPS.

This interactive selection dialog is associated with a filter, displayed below
the name of the selected window. If you maintain :kbd:`Alt` pressed while
pressing other keys than :kbd:`Tab`, this will modify the current filter. From
then on, pressing :kbd:`Alt-Tab` will only iterate through those windows that
match the filter.

The filter is matched by any window whose name contains the letter you have
typed. For instance, if you are currently editing the files :file:`unit1.adb`
and :file:`file.adb`, pressing :kbd:`t` will only leave :file:`unit1.adb`
selectable.

.. _Closing_Windows:

Closing Windows
===============

.. index:: close
.. index:: title bar

Wherever the windows are displayed, they are always closed in the same manner.
In the right side of the title bar of the window, one small button is
displayed, looking like a cross. Clicking on this button will close the window.

An alternative way to close the window is to double-click on the icon to the
left of the title bar of the window. Not all windows have such an icon, but
editors do for instance.

When a window is closed, the focus is given to the window of the same part of
the MDI (each of the docks or the middle area) that previously had the focus.
Therefore, if you simply open an editor as a result of a cross-reference query,
you can simply close that editor to go back to where you were before.

Alternatively, you can also select the window by clicking anywhere in its title
bar, and then select the menu `Window->Close`.

Finally, a window can be closed by right-clicking in the associated notebook
tab (if the tabs are visible), and select `Close` in the contextual menu.

In the notebook tab (when you are in an editor), you will also find a `Close
all other editors` menu, which, as its name implies, will keep a single editor
open, the one you are clicking on.

.. _Splitting_Windows:

Splitting Windows
=================

.. index:: Splitting

Windows can be split at will, through any combination of horizontal and
vertical splits.  This feature requires at least two windows (text editors,
browsers, ...) to be superimposed in the central area. Selecting either the
`Window->Split Horizontally` or `Window->Split Vertically` menus will then
split the selected window in two. In the left (resp. top) pane, the currently
selected window will be left on its own. The rest of the previously
superimposed windows will be put in the right (resp. bottom) pane. You can then
in turn split these remaining windows to achieve any layout you want.

All split windows can be resized interactively by dragging the handles that
separate them. A preference (menu `Edit->Preferences`) controls whether this
resizing is done in opaque mode or border mode. In the latter case, only the
new handle position will be displayed while the mouse is dragged.

You may want to bind the key shortcuts to the menus `Window->Split
Horizontally` as well as `Window->Split Vertically` using the key manager. In
addition, if you want to achieve an effect similar to e.g. the standard Emacs
behavior (where :kbd:`control-x 2` splits a window horizontally, and
:kbd:`control-x 3` splits a window vertically), you can use the key manager
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

.. _Floating_Windows:

Floating Windows
================

.. index:: floating
.. index:: top level

Although the MDI, as described so far, is already extremely flexible, it is
possible that you prefer to have several top-level windows under direct control
of your system or window manager. This would be the case for instance if you
want to benefit from some extra possibilities that your system might provide
(virtual desktops, different window decoration depending on the window's type,
transparent windows, multiple screens, ...).

GPS is fully compatible with this behavior, since windows can also be
**floating windows**. Any window that is currently embedded in the MDI can be
made floating at any time, simply by selecting the window and then selecting
the menu `Window->Floating`. The window will then be detached, and can be moved
anywhere on your screen, even outside of GPS's main window.

.. index:: menu

There are two ways to put a floating window back under control of GPS.  The
more general method is to select the window through its title in the menu
`Window`, and then unselect `Window->Floating`.

.. index:: preferences

The second method assumes that the preference **Destroy Floats** in the menu
`Edit->Preferences` has been set to false. Then, you can simply close the
floating window by clicking in the appropriate title bar button, and the window
will be put back in GPS. If you actually want to close it, you need to click
once again on the cross button in its title bar.

.. index:: all floating

A special mode is also available in GPS, where all windows are floating. The
MDI area in the main window becomes invisible. This can be useful if you rely
on windows handling facilities supported by your system or window manager but
not available in GPS. This might also be useful if you want to have windows on
various virtual desktops, should your window manager support this.

This special mode is activated through a preference (menu `Edit->Preferences`).
This preference is entitled **All Floating**.

.. _Moving_Windows:

Moving Windows
==============

.. index:: moving

As we have seen, the organization of windows can be changed at any time by
selecting a notebook containing several editors or browsers, and selecting one
of the Split menus in the `Window` menu.

.. index:: drag-n-drop

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

If you want to move a window to another notebook by dragging its tab, you
should first move out of the tab area (vertically in general), and then
anywhere in GPS. That's to distinguish between the mode where you want to
reorder tabs and the mode where you want to move windows.

While you keep the mouse button pressed, and move the mouse around, the
selected drop area is highlighted with a dashed border. This shows precisely
where the window would be put if you were to release the mouse button at that
point.

If you move your mouse all the way to the side of the desktop, and then drop
the window, that window will occupy the full width (resp. height) of the
desktop on that side.

Here are the various places where a window can be dropped:

*Inside the MDI*
  The location of the current window is indicated by a dashed rectangle, and
  the window you are dragging will be positioned at the same location as that
  rectangle: either on top of the window on which you dropped it (therefore they
  will both be put inside a notebook), or to one of the sides of that window,
  splitting as needed.

*System window*
  If you drop a window outside of GPS (for
  instance, on the background of your screen), the window will be floated.

If you maintain the :kbd:`shift` key pressed while dropping the window, this
might result in a copy operation instead of a simple move. For instance, if you
are dropping an editor, a new view of the same editor will be created,
resulting in two views present in GPS: the original one is left at its initial
location, and a second view is created at the new location.

If you maintain the :kbd:`control` key pressed while dropping the window, all
the windows that were in the same notebook are moved, instead of the single one
you selected. This is the fastest way to move a group of windows to a new
location, instead of moving them one by one.

.. _Perspectives:

Perspectives
============

.. index:: perspectives

GPS supports the concept of perspectives. These are activity-specific desktops,
each with their own set of windows, but sharing some common windows like the
editors.

Depending on the activity you want to perform (debugging, version control,...)
you could switch to another perspective. For instance, in the context of the
debugger, the new perspective would by default contain the call stack window,
the data window, the debugger consoles,... each at your favorite location.
Whenever the debug starts, you therefore do not have to open these windows
again.

The perspectives have names, and you switch perspectives by selecting the menu
/Window/Perspectives/. You can also create a new perspective by selecting the
menu /Window/Perspectives/Create New.

GPS will sometimes automatically change perspectives. For instance, if you
start a debugger, it will switch to the perspective called "Debug" (if it
exists). When the debugger terminates, you are switched back to the "Default"
perspective (again, if it exists).

When you leave a perspective, GPS automatically saves its contents (which
windows are opened, their location,...), so that when you are going back to the
same perspective you find the same layout.

Likewise, when GPS exits, it will save the layout of all perspectives into a
file called :file:`perspectives.xml`, so that it can restore them when you
restart GPS. This behavior is controlled by the "Save desktop on exit"
preference, and can be disabled.

One of the difficulties in working with perspectives is knowing which windows
will be preserved when you switch to another perspective, and which windows
will be hidden. There is a central area where all preserved windows are found.
Typically, it only contains editors (including if you have split them side by
side for instance). If you drag and drop another window on top or to the sides
of an editor, that window will be preserved when changing perspectives, unless
it was already found elsewhere in the new perspective.  The small tooltip that
appears on the screen while you drag and drop will tell you whether the window
(if dropped at the current location) will be visible in other perspectives or
not.
