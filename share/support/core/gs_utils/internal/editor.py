from menu import *
import pygps
import os
from gi.repository import Gdk


def text_view_from_location(loc):
    """Creates a Gtk.TextView from an EditorLocation"""
    return pygps.get_widgets_by_type(
        Gtk.TextView, loc.buffer().current_view().pywidget())[0]


def iter_from_location(loc):
    """Creates a Gtk.TextIter from an EditorLocation"""

    view = text_view_from_location(loc)
    b = view.get_buffer()

    mark_name = "iter_from_loc_temp_mark"

    _ = loc.create_mark(mark_name)
    mark = b.get_mark(mark_name)

    return b.get_iter_at_mark(mark)


def click_in_widget(
        window, x=0, y=0, button=1, events=pygps.single_click_events,
        through_gps=True):
    """Simulate a click in a widget. There are various other functions
     adapted for specific widget types"""

    if os.name == 'nt' and button == 3 \
       and events == pygps.single_click_events:

        # ??? work around
        # On Windows sending a BUTTON_PRESS followed by a
        # BUTTON_RELEASE event when opening a contextual menu does
        # not work. The BUTTON_RELEASE close the contextual menu.
        # For now we remove this event.
        events = events[:1]

    for event_type in events:
        if through_gps:
            GPS.send_button_event(
                button=button,
                x=int(x),
                y=int(y),
                window=window,
                type=event_type)
        else:
            event = Gdk.EventButton()
            event.type = event_type
            event.window = window
            event.device = pygps.default_event_device()
            event.x = float(x)
            event.y = float(y)
            event.button = button
            event.put()

    pygps.process_all_events()


def click_in_text(
        loc, button=1, xoffset=0, events=pygps.single_click_events,
        through_gps=True):
    """Simulate a mouse click at the given EditorLocation.
     If you use the third button to display a contextual menu, see also
     activate_contextual().
     If xoffset is not null, the mouse is clicked that many pixels to
     the right of the actual location. This can be used to simulate clicks
     outside the bounds of a line."""

    view = text_view_from_location(loc)
    rect = view.get_iter_location(iter_from_location(loc))
    (x, y) = view.buffer_to_window_coords(Gtk.TextWindowType.TEXT, rect.x
                                          + 1 + xoffset, rect.y + 1)

    click_in_widget(view.get_window(Gtk.TextWindowType.TEXT), x=x, y=y,
                    button=button, events=events, through_gps=through_gps)


class SourceEditor(object):

    def __init__(self, buffer):
        """
        This class is a proxy for the GPS.EditorBuffer class
        :param GPS.EditorBuffer buffer: the wrapped buffer.
        """
        self.buffer = buffer

    def execute_contextual_menu(self, at, name):
        """
        Opens the contextual menu at a specific location, and then executes
        one of the items
        :param GPS.EditorLocation at: the location at which the menu should
           be opened.
        :param str name: the item to execute.
        """
        self.buffer.current_view().goto(at)
        menu = TestContextual(lambda: click_in_text(at, button=3))
        yield menu.open()
        yield menu.select(name)
