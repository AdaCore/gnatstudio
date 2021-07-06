
"""
Test the overlay property for the MDI scrolled views.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Test the overlay scrolling value for a non-editor view
    message_view = GPS.MDI.get("Messages")
    message_scroll = get_widgets_by_type(Gtk.ScrolledWindow,
                                         message_view.pywidget())[0]
    gps_assert(message_scroll.get_overlay_scrolling(), False,
               "Wrong overlay scrolling property for non-editor views")

    # Test the overlay scrolling value for an editor
    buffer_view = GPS.EditorBuffer.get(GPS.File("hello.adb")).current_view()
    buffer_scroll = get_widgets_by_type(Gtk.ScrolledWindow,
                                        buffer_view.pywidget())[0]
    gps_assert(buffer_scroll.get_overlay_scrolling(), True,
               "Wrong overlay scrolling property for an editor views")
