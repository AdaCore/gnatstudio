"""Utility routines for GPS scripts

This package contains a number of functions that you can use in your
own scripts to pilot the GPS interface. In particular, most of the
functions depend on the pygtk module, and can be used to simulate
button clicks, open dialogs,...

You do not need to auto load this module at startup, each module that
depends on it should use the statement:
   from pygps import *
"""

###########################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)



############################################################################
## No user customization below this line
############################################################################

import GPS, time

# List of modules to import when user does "from pygps import *"
# Do not use, since otherwise the functions defined in this module are
# not visible
# __all__ = ["notebook", "project", "tree"]

def delayed_exit (delay=200):
  """Exit GPS after a small timeout.
     This should be used instead of GPS.exit() in contexts where a command
     is still executing, since otherwise GPS cannot exit immediately"""
  def exit_gps (T):
     if GPS.Command.list () == []:
        GPS.exit (force=1)
  GPS.Timeout (delay, exit_gps)


try:
  import gtk, gobject

  ##########
  ## Misc ##
  ##########
  # The following functions provide wrappers around pygtk functions, to make
  # their use easier

  def process_all_events ():
    """Process all pending events. This is often needed in the testsuite,
       where we need to wait for events to be processed to make sure that
       a view is refreshed for instance.
       Using this function is not recommended in the general case, but is
       sometimes unavoidable. In general, it is better to use idle callbacks
       through gobject.idle_add, to avoid blocking the whole GPS interface."""

    while gtk.events_pending():
       gtk.main_iteration (block=0)


  single_click_events = [gtk.gdk.BUTTON_PRESS,
                         gtk.gdk.BUTTON_RELEASE]
  # List of events to emit for a single click

  double_click_events = [gtk.gdk.BUTTON_PRESS,
                         gtk.gdk.BUTTON_RELEASE,
                         gtk.gdk.BUTTON_PRESS,
                         gtk.gdk._2BUTTON_PRESS,
                         gtk.gdk.BUTTON_RELEASE]
  # List of events to emit for a double click

  triple_click_events = [gtk.gdk.BUTTON_PRESS,
                         gtk.gdk.BUTTON_RELEASE,
                         gtk.gdk._2BUTTON_PRESS,
                         gtk.gdk.BUTTON_RELEASE,
                         gtk.gdk.BUTTON_PRESS,
                         gtk.gdk._3BUTTON_PRESS,
                         gtk.gdk.BUTTON_RELEASE]
  # List of events to emit for a triple click

  ################################
  ## Traversing the widget tree ##
  ################################
  # The following classes and functions are used to traverse the tree of
  # widgets displayed on the screen, or access specific widgets

  class WidgetTreeIterator:
    """An iterator for WidgetTree (see the class WidgetTree for examples)"""

    def __init__ (self, list):
       self.to_traverse = list
    def __iter__ (self):
       return self
    def next (self):
       if self.to_traverse == []:
          raise StopIteration
       w = self.to_traverse.pop()
       if isinstance (w, gtk.Container):
         self.to_traverse = self.to_traverse + w.get_children()
       return w

  class WidgetTree:
    """Virtual container that represents the widget hierarchy.
       You can traverse it with
           for w in WidgetTree(): ...
       or if you simply want to iterate the children of a dialog:
           for w in WidgetTree (dialog): ...
       or for multiple dialogs:
           for w in WidgetTree ([dialog1, dialog2]): ...
       To get a list of all buttons in the interface:
           [x for x in WidgetTree() if isinstance (x, gtk.Button)]
    """

    def __init__ (self, list = None):
      if not list: list = gtk.window_list_toplevels()
      if list.__class__ != [].__class__: list = [list]
      self.list = list
    def __iter__ (self):
      return WidgetTreeIterator (self.list)

  class MenuIterator:
    """An iterator for MenuTree (see the class MenuTree for examples)"""

    def __init__ (self, menu, accel_path_prefix):
      if menu.__class__ != [].__class__:
	 self.to_traverse = [(menu, accel_path_prefix, 0)]
      else:
	 self.to_traverse = [(w, accel_path_prefix, 0) for w in menu]
    def __iter__ (self):
       return self
    def next (self):
       while self.to_traverse != []:
	 (w, prefix, level) = self.to_traverse.pop ()

	 if isinstance (w, gtk.MenuItem):
	   accel_path = ""
	   result = None
	   for m in w.get_children():
	     if isinstance (m, gtk.Label):
		accel_path = prefix + m.get_text()
		accel = ""
		if isinstance (m, gtk.AccelLabel):
		   key = gtk.accel_map_lookup_entry (prefix + m.get_text())
		   if key and key[0] != 0:
		      accel = gtk.accelerator_name (key[0], key[1])
		result = (w, accel_path, accel, level)

	   if w.get_submenu ():
	     self.to_traverse = self.to_traverse + \
		[(c, accel_path + "/", level + 1) \
		 for c in w.get_submenu ()]
	   if result:
	     return result

	 elif isinstance (w, gtk.Container):
	   self.to_traverse = self.to_traverse + \
	      [(c, prefix, level + 1) for c in w.get_children ()]
       raise StopIteration

  class MenuTree:
    """Iterates over a menu and all its submenus. For each item, return
       a tuple (menu, label, accel, level), where menu is the
       gtk.MenuItem widget"""

    def __init__ (self, menu):
       self.menu = menu
    def __iter__ (self):
       return MenuIterator (self.menu, "<gps>/")

  def get_widget_by_name (name, list=None):
    """Search in the whole hierarchy given by list (see WidgetTree) the
       first widget with the given name.
       The name must have been set explicitly in Ada through a call to
       Set_Name (W, "...")
    """

    result = [x for x in WidgetTree(list) if x.get_name() == name]
    if result: return result[0]
    else:      return None

  def get_widgets_by_type (type, list=None):
    """Find all widgets that are instances of type"""

    return [x for x in WidgetTree(list) if isinstance (x, type)]

  def get_window_by_title (title, list=None):
    """Search the whole hierarchy given by list (see WidgetTree) the
       first window with given title."""

    result = [x for x in WidgetTree (list) \
              if isinstance (x, gtk.Window) and x.get_title() == title]
    if result: return result[0]
    else:      return result

  def get_stock_button (parents, stock = gtk.STOCK_OK):
    """Find the first button in the possible parents that is a stock button
       with the given stock label.
       Most dialogs in GPS use such buttons, that mix icons and text."""

    return [x for x in WidgetTree (parents) \
            if isinstance (x, gtk.Button) \
                and x.get_use_stock() and x.get_label() == stock][0]

  def get_button_from_label (label, parents=None):
    """Return the first button with the matching label"""

    for x in get_widgets_by_type (gtk.Button, parents):
       if x.get_label() == label: return x
       for l in get_widgets_by_type (gtk.Label, x):
          if l.get_text() == label: return x
    return None

  ###########
  ## Menus ##
  ###########
  # The following subprograms are provided to access GPS menus and wait
  # until they have open a dialog

  def open_menu (menu, on_open, widgets, args, kwargs, timeout=0):
    """Generic function to open a menu, wait for the dialog to appear,
       and then call a user callback with several arguments: one for the
       newly created dialog, one for each widget whose name is specified
       in widgets, then *args and **kwargs. The latter are provided so
       that the callback on_open can be given any number of arguments that
       your application needs.
       Do not use this directly in general, but rather
       open_project_properties, open_project_wizard,..."""
    def internal_on_open (on_open, widgets, windows, args, kwargs):
       dialog = [w for w in gtk.window_list_toplevels() \
                 if not w in windows and w.flags () & gtk.MAPPED]
       if not dialog:
          # Will try again after same timeout or idle
          return True
       dialog = dialog[0]

       params = tuple \
         ([dialog] + [get_widget_by_name (name, dialog) for name in widgets])
       apply (on_open, params + args, kwargs)
    windows = gtk.window_list_toplevels()
    if timeout == 0:
      gobject.idle_add \
        (lambda: internal_on_open (on_open, widgets, windows, args, kwargs))
    else:
      gobject.timeout_add \
        (timeout, lambda: internal_on_open (on_open, widgets, windows, args, kwargs))
    GPS.Menu.get (menu).pywidget().activate()

  ################
  ## Key events ##
  ################
  # The following functions provide helpers to send key events to GPS,
  # just as if the user was pressing the corresponding key. In general,
  # it is better to directly call the appropriate GPS action or menu
  # rather than rely on these functions

  GDK_TAB    = 65289
  GDK_RETURN = 65293
  GDK_ESCAPE = 65307

  def send_key_event (keyval, control=0, alt=0, shift=0, window=None, \
                      process_events=True):
    """Emit a key event on GPS, simulating the given key. This event is sent
       asynchronously.
       Unless process_events is true, this function will return when the
       event has not yet been processed by gtk+.
       keyval is generally the result of calling  ord("x").
       Sending letters to an editor doesn't seem to work at the moment,
       except for special characters like GDK_RETURN.
    """

    event = gtk.gdk.Event (gtk.gdk.KEY_PRESS)

    if not window:
       window = gtk.window_list_toplevels()[0]
    if isinstance (window, gtk.TextView):
       window = window.get_window (gtk.TEXT_WINDOW_TEXT)
    if not isinstance (window, gtk.gdk.Window):
       window = window.window
    event.window = window
    event.keyval = keyval
    event.send_event = 1
    event.time   = int (time.time())

    # We cannot set event.string, because of a bug in pygtk, which tries
    # to doubly deallocate the string later on
    # if keyval >= 32 and keyval <= 128:
    #   event.string = chr (keyval)

    event.state = 0
    if control: event.state = event.state or gtk.gdk.CONTROL_MASK
    if shift:   event.state = event.state or gtk.gdk.SHIFT_MASK
    if alt:     event.state = event.state or gtk.gdk.MOD1_MASK
    event.put()
    if process_events:
      process_all_events()

except ImportError:
   pass

