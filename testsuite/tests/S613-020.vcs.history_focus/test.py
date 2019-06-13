"""
This test checks that selecting a commit in the History view
always give the focus to the corresponding diff editor, even
when already opened.
"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test():
    GPS.execute_action("open History")
    yield wait_idle()

    # Select the first entry in the History view:
    # this should give the focus to the diff editor

    tree = get_widget_by_name("History Tree")
    tree.get_selection().select_path("1")
    yield wait_idle()

    ed = GPS.EditorBuffer.get()
    view = ed.current_view()

    gps_assert("Commit " in view.title(short=False), True,
               "The diff editor should be focused")

    # Open a new editor and reselect the first
    # entry in the History view: the diff editor
    # should be focused again

    GPS.EditorBuffer.get(GPS.File("a.adb"))

    tree.get_selection().unselect_all()
    tree.grab_focus()
    tree.get_selection().select_path("1")
    yield wait_idle()

    yield timeout(1000)

    ed = GPS.EditorBuffer.get()
    view = ed.current_view()

    gps_assert("Commit " in view.title(short=False), True,
               "The diff editor should be focused, even if "
               + "already opened")
