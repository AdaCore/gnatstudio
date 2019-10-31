import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("explorer-preserve-nodes-state").set("true")
    explorer = get_widget_by_name("Project Explorer Tree")
    explorer.expand_row(Gtk.TreePath((0, 0)), True)

    project = GPS.Project.load("one.gpr")
    yield wait_idle()

    project = GPS.Project.load("default.gpr")
    yield wait_idle()

    explorer = get_widget_by_name("Project Explorer Tree")
    gps_assert(explorer.row_expanded(Gtk.TreePath((0, 0))), True, "Unrestored")
