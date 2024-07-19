"""
Test whether assembly breakpoints are correctly
highlighted in the Assembly view.
"""

import GPS
from gs_utils.internal.utils import *


BG_COLOR_COLUMN = 6
EXPECTED_BG_COLOR = (
    "Gdk.RGBA(red=0.000000, green=0.749020, blue=0.768627, alpha=1.000000)"
)


@run_test_driver
def test_driver():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(6, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    debug = GPS.Debugger.get()

    # start to have the valid address
    debug.start()
    yield hook("debugger_location_changed")

    # Open the assembly view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")
    yield wait_DAP_server("disassemble")
    yield timeout(3000)

    # Select the 4th row in the Assembly
    view = GPS.MDI.get("Assembly")
    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()
    iter = tree.get_model().get_iter("4")
    tree.get_selection().select_iter(iter)

    # Create a breakpoint in the Assembly view
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_DAP_server("setInstructionBreakpoints")

    # Check if the 4th row has been correctly highlighted
    iter = tree.get_model().get_iter("4")
    gps_assert(
        str(tree.get_model().get_value(iter, BG_COLOR_COLUMN)).strip(),
        EXPECTED_BG_COLOR,
        "Instruction breakpoint not correctly highlighted in Assembly view",
    )

    # Disable it now
    GPS.execute_action("assembly_view toggle breakpoint")
    yield wait_DAP_server("setInstructionBreakpoints")

    # Check that the highlighting is now gone
    iter = tree.get_model().get_iter("4")
    gps_assert(
        tree.get_model().get_value(iter, BG_COLOR_COLUMN),
        None,
        "Highlighting should have gone after toggling the breakpoint",
    )
