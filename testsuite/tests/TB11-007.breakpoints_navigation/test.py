#  Checking whether clicking on a breakpoint opens source code
#  when debugger is not started

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf = GPS.EditorBuffer.get(GPS.File("a.adb"))
    buf.current_view().goto(buf.at(5, 1))
    GPS.execute_action("debug set line breakpoint")

    # Open the Breakpoints view and check that the breakpoints has been set
    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child("Breakpoints")
    view = GPS.MDI.get("Breakpoints")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]

    buf.close()
    yield wait_idle()

    click_in_tree(tree, "0", events=pygps.double_click_events)

    yield wait_idle()

    gps_assert(GPS.MDI.get("a.adb") != None, True, "a.adb should be opened")
