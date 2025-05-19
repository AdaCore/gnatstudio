import GPS
from gs_utils.internal.utils import *

"""
Verify that 'Open in system file explorer' action
works.

"""


@run_test_driver
def test_driver():
    prj_view = Project_View()
    yield prj_view.open_and_yield()
    explorer = get_widget_by_name("Project Explorer Tree")
    explorer.expand_row(Gtk.TreePath((0, 0)), True)
    prj_view.select_by_name(column=1, value="main.adb")
    yield wait_idle()
    GPS.execute_action("Open in system file explorer")
    yield wait_idle()

