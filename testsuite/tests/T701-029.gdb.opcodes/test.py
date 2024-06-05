"""
Verify that assembly view shows OpCodes.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()
    GPS.Preference("assembly_view-show-opcodes").set(True)
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(5, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    debug = GPS.Debugger.get()

    # Open the assembly view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")

    assembly = GPS.MDI.get("Assembly").pywidget()
    model = get_widgets_by_type(Gtk.TreeView, assembly)[0].get_model()
    chars = model.get_value(model.get_iter_first(), 4)
    gps_assert(chars is not None, True, "The Assembly view does not have OpCodes")
    gps_assert(len(chars) > 0, True, "The Assembly view does not have OpCodes")
