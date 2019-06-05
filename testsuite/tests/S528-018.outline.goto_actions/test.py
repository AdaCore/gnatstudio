"""
Verify that goto declaration works fine from Outline view.
"""

from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    file = GPS.File("foo.adb")
    buf = GPS.EditorBuffer.get(file)
    buf.current_view().goto(buf.at(3, 16))

    GPS.execute_action("open Outline")
    outline = get_widget_by_name("Outline View Tree")
    outline.grab_focus()
    yield wait_idle()

    windows = Gtk.Window.list_toplevels()
    click_in_tree(outline, button=3)
    activate_contextual(windows, "Go To Declaration")
    yield timeout(1000)

    current_buffer = GPS.EditorBuffer.get()
    current_loc = current_buffer.current_view().cursor()

    gps_assert(
        current_buffer.file(), GPS.File("foo.ads"),
        "Go To Declaration contextual menu dit not open the right file")
    gps_assert(
        current_loc.line(), 3, "Wrong line after Go To Declaration")
    gps_assert(
        current_loc.column(), 24, "Wrong column after Go To Declaration")
