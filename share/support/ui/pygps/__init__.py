#!/usr/bin/python
# -*- coding: utf-8 -*-

###########################################################################
# Customization variables
# These variables can be changed in the initialization commands associated
# with this script (see /Edit/Startup Scripts)

############################################################################
# No user customization below this line
############################################################################

"""Utility routines for GPS scripts

This package contains a number of functions that you can use in your
own scripts to pilot the GPS interface. In particular, most of the
functions depend on the pygobject module, and can be used to simulate
button clicks, open dialogs,...

You do not need to auto load this module at startup, each module that
depends on it should use the statement:
   from pygps import *
"""

import GPS
import sys

global last_sent_event
last_sent_event = None

# List of modules to import when user does "from pygps import *"
# Do not use, since otherwise the functions defined in this module are
# not visible
# __all__ = ["notebook", "project", "tree"]


def get_children_tree(w):
    """
    Returns a tree of widgets for which w is the root
    """
    if hasattr(w, "get_children"):
        ch = w.get_children()
    else:
        ch = []
    if ch:
        return (w, map(get_children_tree, ch))
    else:
        return w


def delayed_exit(delay=200):
    """Exit GPS after a small timeout.
     This should be used instead of GPS.exit() in contexts where a command
     is still executing, since otherwise GPS cannot exit immediately"""

    def exit_gps(T):
        if not GPS.Command.list():
            GPS.exit(force=1)

    GPS.Timeout(delay, exit_gps)


try:
    from gi.repository import Gtk, GObject, Gdk

    single_click_events = [Gdk.EventType.BUTTON_PRESS,
                           Gdk.EventType.BUTTON_RELEASE]
    # List of events to emit for a single click

    double_click_events = [Gdk.EventType.BUTTON_PRESS,
                           Gdk.EventType.BUTTON_RELEASE,
                           Gdk.EventType.BUTTON_PRESS,
                           Gdk.EventType._2BUTTON_PRESS,
                           Gdk.EventType.BUTTON_RELEASE]
    # List of events to emit for a double click

    triple_click_events = [
        Gdk.EventType.BUTTON_PRESS,
        Gdk.EventType.BUTTON_RELEASE,
        Gdk.EventType.BUTTON_PRESS,
        Gdk.EventType._2BUTTON_PRESS,
        Gdk.EventType.BUTTON_RELEASE,
        Gdk.EventType.BUTTON_PRESS,
        Gdk.EventType._3BUTTON_PRESS,
        Gdk.EventType.BUTTON_RELEASE]
    # List of events to emit for a triple click

    # #########
    # # Misc ##
    # #########
    # The following functions provide wrappers around pygobject functions, to
    # make their use easier

    def default_event_device():
        """ Retrieve and cache the default event device """
        return Gdk.Display.get_default(
            ).get_device_manager().get_client_pointer()

    def process_all_events():
        """
        Process all pending events. This is often needed in the testsuite,
        where we need to wait for events to be processed to make sure that a
        view is refreshed for instance.  Using this function is not recommended
        in the general case, but is sometimes unavoidable. In general, it is
        better to use idle callbacks through GObject.idle_add, to avoid
        blocking the whole GPS interface.
        """

        GPS.process_all_events()

    # ###############################
    # # Traversing the widget tree ##
    # ###############################
    # The following classes and functions are used to traverse the tree of
    # widgets displayed on the screen, or access specific widgets
    class WidgetTreeIterator:

        """
        An iterator for WidgetTree (see the class WidgetTree for examples)
        """

        def __init__(self, list):
            self.to_traverse = list

        def __iter__(self):
            return self

        def next(self):
            if self.to_traverse == []:
                raise StopIteration
            w = self.to_traverse.pop(0)
            if isinstance(w, Gtk.Container):
                self.to_traverse.extend(w.get_children())
            return w

    class WidgetTree(object):

        """Virtual container that represents the widget hierarchy.
           You can traverse it with
              for w in WidgetTree(): ...
           or if you simply want to iterate the children of a dialog:
              for w in WidgetTree (dialog): ...
           or for multiple dialogs:
              for w in WidgetTree ([dialog1, dialog2]): ...
           To get a list of all buttons in the interface:
              [x for x in WidgetTree() if isinstance (x, Gtk.Button)]
        """

        def __init__(self, wlist=None):
            if not wlist:
                wlist = Gtk.Window.list_toplevels()
            elif not isinstance(wlist, list):
                wlist = [wlist]
            self.list = wlist

        def __iter__(self):
            return WidgetTreeIterator(self.list)

    class MenuIterator(object):

        """An iterator for MenuTree (see the class MenuTree for examples)"""

        def __init__(self, menu, accel_path_prefix):
            if not isinstance(menu, list):
                self.to_traverse = [(menu, accel_path_prefix, 0)]
            else:
                self.to_traverse = [(w, accel_path_prefix, 0) for w in menu]
            self.index = 0

        def __iter__(self):
            return self

        def next(self):
            # Never delete elements from self.to_traverse, otherwise pygobject
            # will call decref on it, and it is possible that the gtk+ widget
            # will be destroyed as a result

            while self.index < len(self.to_traverse):
                (w, prefix, level) = self.to_traverse[self.index]
                self.index += 1

                if isinstance(w, Gtk.MenuItem):
                    accel_path = ''
                    result = None

                    for m in w.get_children():
                        if isinstance(m, Gtk.Label):
                            accel_path = prefix + m.get_text()
                            accel = ''
                            if isinstance(m, Gtk.AccelLabel):
                                k = Gtk.AccelMap.lookup_entry(accel_path)
                                if k and k[0] != 0:
                                    accel = Gtk.accelerator_name(k[0], k[1])
                            result = (w, accel_path, accel, level)
                            break

                    # We now have modified accel_path for 'w'

                    submenu = w.get_submenu()
                    if submenu:
                        index = self.index
                        for c in submenu:
                            self.to_traverse.insert(
                                index,
                                (c, accel_path + '/', level + 1))
                            index += 1

                    if result:
                        return result

                elif isinstance(w, Gtk.Container):
                    self.to_traverse.extend(
                        (c, prefix, level + 1) for c in w.get_children())

            raise StopIteration

    class MenuTree(object):

        """Iterates over a menu and all its submenus. For each item, return
           a tuple (menu, label, accel, level), where menu is the
           Gtk.MenuItem widget.
        """

        def __init__(self, menu, accel_prefix="<gps>/"):
            """
            :param accel_prefix: added to the accel_path of each tuple
               returned by the iteration.
            """
            self.menu = menu
            self.prefix = accel_prefix

        def __iter__(self):
            return MenuIterator(self.menu, self.prefix)

    def get_widget_by_name(name, list=None):
        """Search in the whole hierarchy given by list (see WidgetTree) the
           first widget with the given name.
           The name must have been set explicitly in Ada through a call to
           Set_Name (W, "...")
        """

        result = [x for x in WidgetTree(list) if x.get_name() == name]
        if result:
            return result[0]
        else:
            return None

    def get_widgets_by_type(type, list=None):
        """Find all widgets that are instances of type"""

        return [x for x in WidgetTree(list) if isinstance(x, type)]

    def get_gtk_buffer(ed_buf):
        """
        @type ed_buf: GPS.EditorBuffer
        @rtype: Gtk.TextBuffer
        """
        gtk_tv = get_widgets_by_type(Gtk.TextView,
                                     ed_buf.current_view().pywidget())[0]
        return gtk_tv.get_buffer()

    def get_window_by_title(title, list=None):
        """Search the whole hierarchy given by list (see WidgetTree) the
           first window with given title.
        """

        result = [x for x in WidgetTree(list) if isinstance(x, Gtk.Window) and
                  x.get_title() == title]
        if result:
            return result[0]
        else:
            return result

    def get_window_by_prefix(prefix, list=None):
        """Search the whole hierarchy given by list (see WidgetTree) the
           first window whose title starts with prefix
        """

        result = [x for x in WidgetTree(list) if
                  isinstance(x, Gtk.Window) and
                  x.get_title() and
                  x.get_title().startswith(prefix)]
        if result:
            return result[0]
        else:
            return result

    def get_stock_button(parents, stock=Gtk.STOCK_OK):
        """Find the first button in the possible parents that is a stock button
           with the given stock label.
           Most dialogs in GPS use such buttons, that mix icons and text.
        """

        return [x for x in WidgetTree(parents) if isinstance(x, Gtk.Button) and
                x.get_use_stock() and x.get_label() == stock][0]

    def get_button_from_label(label, parents=None):
        """Return the first button with the matching label"""

        for x in get_widgets_by_type(Gtk.Button, parents):
            if x.get_label() == label:
                return x
            for l in get_widgets_by_type(Gtk.Label, x):
                if l.get_text() == label:
                    return x
        return None

    def get_button_from_icon_name(parents, icon_name):
        """Return the first button with the matching icon"""
        return [x for x in WidgetTree(parents) if isinstance(x, Gtk.Button) and
                x.get_image() and
                x.get_image().get_icon_name()[0] == icon_name][0]

    # ###########
    # # Labels ##
    # ###########

    def get_label_from_text(text, parents=None):
        """
        Return the first Gtk.Label that displays the given ``text`` or
        None if there is no matching label.
        """
        result = [x for x in WidgetTree(parents) if
                  isinstance(x, Gtk.Label) and
                  x.get_label() == text and
                  x.is_visible()]
        if result:
            return result[0]
        else:
            return None

    # ##########
    # # Menus ##
    # ##########
    # The following subprograms are provided to access GPS menus and wait
    # until they have open a dialog
    def open_menu(menu, on_open, widgets, args, kwargs, timeout=0):
        """Generic function to open a menu, wait for the dialog to appear,
           and then call a user callback with several arguments: one for the
           newly created dialog, one for each widget whose name is specified
           in widgets, then *args and **kwargs. The latter are provided so
           that the callback on_open can be given any number of arguments that
           your application needs.
           Do not use this directly in general, but rather
               open_project_properties, open_project_wizard,...
        """

        def internal_on_open(on_open, widgets, windows, args, kwargs):
            dialog = [w for w in Gtk.Window.list_toplevels() if w
                      not in windows and w.get_mapped()]
            if not dialog:
                # Will try again after same timeout or idle
                return True

            dialog = dialog[0]

            for name in widgets:
                if not get_widget_by_name(name, dialog):
                    # Wrong dialog
                    return True

            params = tuple([dialog] + [get_widget_by_name(name, dialog)
                                       for name in widgets])
            apply(on_open, params + args, kwargs)

        windows = Gtk.Window.list_toplevels()
        if timeout == 0:
            GObject.idle_add(lambda: internal_on_open(on_open, widgets,
                                                      windows, args, kwargs))
        else:
            GObject.timeout_add(timeout, lambda: internal_on_open(
                on_open, widgets, windows, args, kwargs))
        GPS.Menu.get(menu).action.execute_if_possible()

    # ###############
    # # Key events ##
    # ###############
    # The following functions provide helpers to send key events to GPS,
    # just as if the user was pressing the corresponding key. In general,
    # it is better to directly call the appropriate GPS action or menu
    # rather than rely on these functions
    GDK_BACKSPACE = 65288
    GDK_TAB = 65289
    GDK_RETURN = 65293
    GDK_ESCAPE = 65307
    GDK_CONTROL_L = 65507
    GDK_PAGE_DOWN = 0xFF56

    def send_key_event(keyval, primary=0, alt=0, shift=0, control=0,
                       window=None,
                       process_events=True, bypass_keymanager=False):
        """Emit a key event on GPS, simulating the given key. This event is
           sent asynchronously.
           Unless process_events is true, this function will return when the
           event has not yet been processed by gtk+.
           keyval is generally the result of calling  ord("x").
           Sending letters to an editor doesn't seem to work at the moment,
           except for special characters like GDK_RETURN.
           If bypass_keymanager is True, do not use the Ada function which
           passes the event to the key manager, but synthesize the event
           in Python directly.
        """

        keycode = 0

        # Try to retrieve the hardware keycode with the appropriate
        # Gtk.Keymap function.

        keymap = Gdk.Keymap.get_default()
        success, keys = keymap.get_entries_for_keyval(keyval)

        if success:
            keycode = keys[0].keycode

        if not bypass_keymanager:
            if hasattr(GPS, "send_key_event"):
                GPS.send_key_event(keyval, window=window,
                                   primary=primary, control=control,
                                   alt=alt, shift=shift,
                                   hardware_keycode=int(keycode))
                return

        def _synthesize(type, keyval):
            event = Gdk.EventKey()
            event.type = type

            event.window = window
            event.keyval = keyval
            event.send_event = 0
            event.length = 1
            event.is_modifier = 0
            event.group = 0
            event.state = Gdk.ModifierType(0)
            event.hardware_keycode = keycode

            # Can't set string in some versions of pygobject
            # hardware_keycode is OS and keyboard specific.
            # Neither of these are needed in any case.

            return event

        if not window:
            window = [w for w in Gtk.Window.list_toplevels()
                      if w.get_window()][0]
        if isinstance(window, Gtk.TextView):
            window = window.get_window(Gtk.TextWindowType.TEXT)
        if not isinstance(window, Gdk.Window):
            window = window.get_window()

        event = _synthesize(Gdk.EventType.KEY_PRESS, keyval)

        if primary:
            if sys.platform == 'darwin':
                event.state |= Gdk.ModifierType.META_MASK
            else:
                event.state |= Gdk.ModifierType.CONTROL_MASK

        if control:
            event.state |= Gdk.ModifierType.CONTROL_MASK

            # Synthesize the pressing of the control key
            e2 = _synthesize(Gdk.EventType.KEY_PRESS, GDK_CONTROL_L)
            e2.put()

        if shift:
            event.state |= Gdk.ModifierType.SHIFT_MASK
        if alt:
            event.state |= Gdk.ModifierType.MOD1_MASK

        event.put()

        e3 = _synthesize(Gdk.EventType.KEY_RELEASE, keyval)
        e3.state = event.state
        e3.put()

        if control:
            e4 = _synthesize(Gdk.EventType.KEY_RELEASE, GDK_CONTROL_L)
            e4.state = event.state   # matches what gtk+ does
            e4.put()

        if process_events:
            process_all_events()

    def get_notebook(widget):
        """
        :type text_box: Gtk.VBox
        :return: Gtk.Notebook
        """
        try:
            w = widget
            while not isinstance(w, Gtk.Notebook):
                w = w.get_parent()
            return w
        except AttributeError:
            return None

    def select_combo(combo, item):
        """
        In the given combo box, select the item whose name is `item`
        """
        model = combo.get_model()
        for row in model:
            if item == row[0]:
                combo.set_active_iter(row.iter)
                return

    def get_current_textview(nb):
        """
        :type nb: Gtk.Notebook
        :return: Gtk.TextView
        """
        widgets = get_widgets_by_type(
            Gtk.TextView, nb.get_nth_page(nb.get_current_page()))

        return widgets[-1] if widgets else None

    def is_editor_visible(ed_buffer):
        """
        :type ed_buffer: GPS.EditorBuffer
        :return: boolean
        """
        tv = get_widgets_by_type(
            Gtk.TextView, ed_buffer.current_view().pywidget())[-1]
        nb = get_notebook(tv)
        if nb:
            return get_current_textview(nb) == tv and tv.is_visible()
        # If nb is None, the editor is not in a notebook
        else:
            return tv.is_visible()


except ImportError:
    pass
