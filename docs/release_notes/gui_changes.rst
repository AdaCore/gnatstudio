Changes to the user interface
------------------------------

This release of GPS is based on a major new release of the gtk+
toolkit. We took the opportunity to make various enhancements to
the user interface itself.


..  omitted on purpose (not worth an entry):
..      NF-53-M102-006 GPS: move Serialize to /Edit/Rectangle/
..      NF-60-LA05-038 GPS: Key shortcut for "Locate in Project View" (2013-10-01)



Local toolbars
~~~~~~~~~~~~~~

.. NF-60-LC03-011 GPS: local toolbars for views (2012-12-03)

A number of views now have a local toolbar. They also display their
configuration settings in a local menu rather than in the
:guilabel:`Preferences` dialog or the contextual menu.

.. figure:: locations_toolbar.png
   :class: screenshot
   :scale: 100%

   Local toolbar in the Locations window


Omni-search field in the toolbar
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M523-031 GPS: add search field in the global toolbar (2013-05-27)

A search field is now provided, which will look simultaneously in various
context (source file names, entities,...). You can optionally restrict the
search to specific contexts to view all possible matches in this context.

..  NF-60-M606-028 GPS: remove Open From Project dialog
..  NF-60-M603-060 GPS: remove Goto Entity dialog (2013-06-07)

This field replaces the two dialogs :menuselection:`File-->Open File From
Project` and :menuselection:`Navigate-->Goto Entity...`. The menus are still
available, but the actual search is done via the global search field.

The :menuselection:`Tools-->Views-->Entity` view was also removed since its
features are now available through the global search field.

..  NF-60-M625-005 GPS: approximate search (2013-06-25)

Various modes are available, to search full text, regular expressions or
fuzzy matches. In particular, when searching in source code, GPS is able
to do approximate search. For instance, searching for "saerch" will also
match "search" when this mode is activated.

.. figure:: search.png
   :scale: 100%
   :class: screenshot

   Search in all contexts


Color schemes
~~~~~~~~~~~~~

..  NF-60-M620-018 GPS: color schemes (2013-06-20)

A new preference is available to select color schemes. This effects other
preferences and can be used to set dark colors for the background of
editors and windows.

.. figure:: color_scheme_pref.png
   :scale: 100%
   :class: screenshot

   Setting the color scheme preference

.. figure:: color_scheme_dark.png
   :scale: 100%
   :class: screenshot

   Switching GPS to a dark theme


Search window improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M904-022 GPS: search dialog shows name of current project (2013-09-10)

The search dialog provides a :guilabel:`Scope` to search only in the source
files of the current project. The dialog was improved to show exactly which
project is considered as current. Also, :menuselection:`Navigate-->Search` from
an editor will set the current project to be that of the editor.

..  NF-60-L510-040 GPS: search in the current selection (2013-01-31)

GPS can now limit Find and replace operations to the currently selected text.
This is done by selecting the scope :guilabel:`Current Selection` in the
:guilabel:`Search` dialog; this scope is automatically selected when launching
the dialog while a selection exists in the current editor.


Key shortcuts dialog improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-LB20-022 GPS: Key shortcuts dialog display improved (2012-11-21)

The display of the key shortcuts in this dialog now matches what is
done for the toplevel menus, using symbols like up arrow for the shift
key, a caret sign for the control key, and the special symbols for
Mac keyboards.

.. figure:: keyshortcuts_dialog.png
   :scale: 100%
   :class: screenshot

   Key shortcuts display machine-specific symbols for keys


Multiple Document Interface
~~~~~~~~~~~~~~~~~~~~~~~~~~~

Tab menu allows easy selection of open tabs
............................................

.. NF-60-LC06-015 GPS: tab menu allows easy selection of open tabs (2012-12-17)

The right-click menu on notebook tabs now shows the list of all windows open
in that tabs, to ease navigation.

.. figure:: tab_menu.png
   :scale: 100%
   :class: screenshot

   Menu when right-clicking on tabs


Status bar removed
..................

..  NF-60-LB29-021 GPS: remove status bar (2012-11-29)

The status bar has been removed. It was only used to display the current tasks
in progress, and the corresponding progress bars were moved to the right of the
toolbar, replacing the throbber.

Use ellipsis in window titles
.............................

.. NF-60-LB29-006 GPS: use ellipsis in window titles (2012-11-28)

The text in the title bars and in the notebook tabs will now use ellipsis
("...") when the window is too small to accomodate the whole text. The size
of notebook tabs is also limited so that we do not end up with a single tab
using most of the notebook widths, thus hidding the others. This behavior
is controlled by a new preference :menuselection:`Window-->Homogeneous Tabs`.

.. figure:: homogeneous_tabs.png
   :scale: 100%
   :class: screenshot

   Ellipsis in window titles

Limitations in the organization of windows
..........................................

..  NF-60-M731-021 GPS: prevent possible locations for views (2013-07-31)

GPS now adds a restriction as to where editors and views can be put.  For
instance, editors can only be part of the perspective-independent part of the
desktop, so that they always remain visible when switching perspectives. These
limitations should prevent some surprising behaviors that could happen before
due to the flexibility of the MDI.

Close tabs with middle-click
............................

..  NF-60-M708-023 GPS: middle-click on tab closes the window (2013-07-08)

This is close to the behavior most web browsers implement and is a convenient
way to quickly close a number of tabs.

Vertical tabs
.............

..  NF-60-M705-044 GPS: rotation text in notebook tabs (2013-07-08)

It is now possible, via the contextual menu on each notebook tab, to change the
orientation of the text. For instance, displaying the tabs on the left will by
default rotate the text bottom-to-top, thus saving horizontal space.

Preserve location in new views
..............................

..  NF-60-M124-022 GPS: preserve location in new views (2013-06-06)

When creating a new view for an editor (for instance through drag-and-drop) the
new view is now displayed at the same location as the original editor, rather
than on the first line.

Main window title bar shows base name first
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M817-003 GPS: window title now lists the base name first (2013-08-19)

Speaking of title bars, the GPS window's title has been changed slightly, so
that the base name of the current file appears first, then its directory, and
finally the project.  This is more user friendly on Windows where the title of
the window is displayed in the task bar. It is now easier to chose between
multiple running GPS.

Clickable qualified name in editor status bars
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-LB26-025 GPS: clickable qualified name in editor status bars (2012-12-13)

The name of the current subprogram, as displayed in the editors status bars,
now includes the name of the enclosing subprograms and packages. Each of
these names is clickable to easily jump to the beginning of the enclosing
block.

.. figure:: qualified_names.png
   :scale: 100%
   :class: screenshot

   Clickable qualified names in editor status bar


Outline view improvements
~~~~~~~~~~~~~~~~~~~~~~~~~~

Protected objects in outline view
.................................

.. NF-60-L921-032 GPS: Show protected objects in Outline view (2012-10-16)

Protected and task types/objects and their entries are now visible in
:guilabel:`Outline` view. A new filter in :guilabel:`Show tasks, entries, and
protected types` is used to show/hide these items (in the local configuration
menu).

.. figure:: outline_config.png
   :scale: 100%
   :class: screenshot

   New configuration menu for the outline view


Group spec and body in outline view
...................................

..  NF-60-M110-025 GPS: group spec and body in Outline view (2013-01-10)

It is now possible to group the spec and body (or spec and full view) for an
entity on the same row in the Outline view, to reduce the total height of the
:guilabel:`Outline` view. Clicking on the name of the entity will jump to its spec,
but if this is already the current location in the editor will jump to its body
instead. It is also possible to click on either of the two icons to jump
directory to the spec or the body.

.. figure:: outline_group.png
   :scale: 100%
   :class: screenshot

   Grouping spec and body on the same line in Outline


Filter in outline view
......................

.. NF-60-M122-025 GPS: filter in the outline view (2013-01-23)

A filter has been added to the Outline view to make it easier to find entities.

Flat view mode in outline
.........................

..  NF-60-M121-011 GPS: Outline has a flat view mode (2013-01-21)

It is now possible to display all entities at the same level, rather than
nested hierarchically. In some cases, it makes searching for an entity easier.
It also reduces the amount of horizontal space that the :guilabel:`Outline`
view needs.


Scenario view displays a tree
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-LC21-017 GPS: Update to Scenario View (2013-01-08)

The :guilabel:`Scenario` view has been updated to a more usable tree rendering.

.. figure:: scenarios.png
   :scale: 100%
   :class: screenshot

   Scenario variables are editable in-place


Messages window
~~~~~~~~~~~~~~~~

..  NF-60-M108-041 GPS: line wrapping in Messages window (2013-01-09)

It is now possible to configure whether the Messages window should wrap lines,
or require horizontal scrolling.

..  NF-60-M710-025 GPS: display build progress when build completes (2013-07-10)

When a build terminates on failure, GPS now displays the total progress (as
displayed by the task manager) in the console. This makes it easier to know how
many files were compiled up to the first compilation error.

..  NF-60-M709-025 GPS: reopen messages window as needed (2013-07-10)

There are various backdoors in GPS that makes it possible to close the
:guilabel:`Messages` window. GPS will now automatically reopen it as needed.

Locations view
~~~~~~~~~~~~~~

..  NF-60-LC06-024 GPS: Locations view has a toolbar (2012-12-06)

In the :guilabel:`Locations` view, actions such as `clear locations`, or the
filter panel were moved into a local toolbar to make them more accessible. The
preference :guilabel:`Auto Jump To First Location` was removed from the
preferences menu and moved to the toolbar as well.


..  NF-60-M926-036 GPS: filter in Locations matches on file names (2013-09-26)

The filter was also moved into that local toolbar, and will now also match
the location of messages, not just their text.

A new setting was added to force the alphabetical sorting of file names.

..  NF-60-M711-059 GPS: do not save Locations by default (2013-09-06)

By default, the contents of the :guilabel:`Locations` view is no longer saved
when GPS exits (and restored when it restarts), because it sometimes resulted
in very long loading times. The saving can be reactivated by using the local
configuration menu.

Tooltips
~~~~~~~~

Tooltips while scrolling
........................

..  NF-60-M711-028 GPS: tooltip while scrolling in editors (2013-07-11)

When manipulating an editor scrollbar with the mouse, a tooltip is displayed
that shows the current line and entity.

Tooltips in Project, Files and Windows views
............................................

..  NF-60-L926-027 GPS: Project, Files and Windows views tooltips (2012-11-13)

Tooltips added to views to help navigation when windows are narrow. They also provide
additional information in some cases.

.. figure:: project_tooltip.png
   :scale: 100%
   :class: screenshot

   Tooltips in the Project view

Tooltips in the editor
......................

..  NF-60-M531-036 GPS: tooltips for SPARK 2014 and Ada 2012 aspects (2013-08-03)

Ada 2012 and SPARK 2014 aspects are now displayed in tooltips.

Support for high-density displays
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

..  NF-60-M916-026 GPS: support for retina displays in browsers (2013-09-17)

GPS now supports high-density displays. In particular, the display of tooltips,
editor line numbers, or the various browsers (call graph, entity, debugger,...)
now looks much sharper.

..  NF-60-M712-005 GPS: support for displays with 16 bpp (2013-07-12)

On the other end of the spectrum, GPS now supports displays with a color depth
of 16 bits per pixels, which are often used for remote displays.
