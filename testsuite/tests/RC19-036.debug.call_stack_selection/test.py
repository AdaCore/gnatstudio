"""
Refreshing the Call Stack view with an active selection should not open new
editors.
"""

import GPS
from gs_utils.internal.utils import *


PREF_NAME = "debug-callstack-show-frame-num"
FILENAME = "hidden.adb"


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    d = GPS.Debugger.get()
    for s in ["b pack.adb:17",
              "run"]:
        yield wait_until_not_busy(d)
        d.send(s)

    yield wait_until_not_busy(d)

    win = GPS.MDI.get("Call Stack").pywidget()
    expected = len(GPS.MDI.children())

    # Set the preference ...
    GPS.Preference(PREF_NAME).set(True)
    gps_assert(len(GPS.MDI.children()),
               expected,
               "Issue when modifying the pref without selection")

    # Select the first line in the Call Stack Tree
    tree = get_widgets_by_type(Gtk.TreeView, win)[0]
    selection = tree.get_selection()
    model = tree.get_model()
    selection.select_iter(model.get_iter_from_string("0"))

    # ... unset the preference to trigger a refresh
    GPS.Preference(PREF_NAME).set(False)
    gps_assert(len(GPS.MDI.children()),
               expected,
               "Issue when modifying the pref with selection")
    gps_assert(GPS.MDI.get(FILENAME),
               None,
               "Modifying the pref shouldn't open the files in the callstack")

    d.send('q')

    yield wait_tasks()
