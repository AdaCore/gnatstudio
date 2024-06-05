"""
Verify that the 'Window' menu items are properly sorted
in alphabetical order.
"""

import GPS
from gs_utils.internal.utils import *

EXPECTED = ["Messages", "Outline", "Project", "Scenario", "a.adb", "bb.adb"]


@run_test_driver
def on_gps_started():
    buf_bb = GPS.EditorBuffer.get(GPS.File("bb.adb"))
    buf_a = GPS.EditorBuffer.get(GPS.File("a.adb"))
    yield wait_tasks(other_than=known_tasks)

    # Retrieve the "Window" menu items for all the displayed
    # views
    menu = get_widget_by_name("gtkada-mdi-children-menu")
    dump = [
        get_widgets_by_type(Gtk.Label, x)[0].get_label()
        for x in WidgetTree(menu)
        if isinstance(x, Gtk.MenuItem) and x.get_label() is None
    ]

    gps_assert(dump, EXPECTED, "The 'Window' menu items are not correctly sorted")
