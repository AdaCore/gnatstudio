"""
This test verifies that the breakpoints view properly pango-escapes
incoming text.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    # This reproduces using the "gdb" mode
    GPS.Preference("GPS6-Debugger-Debugger-Kind").set("Gdb")

    # Launch the debugger
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Send a breakpoint instruction which will result in a <PENDING> address
    GPS.Debugger.get().send("break f")

    # Open the Breakpoints view and remove the breakpoint
    # from it

    GPS.execute_action("open breakpoints editor")
    yield wait_for_mdi_child("Breakpoints")

    view = GPS.MDI.get("Breakpoints")

    tree = get_widgets_by_type(Gtk.TreeView, view.pywidget())[0]
    model = tree.get_model()

    # Gdb should have sent "0x<PENDING>" as the address for this pending
    # breakpoint: verify that this is properly escaped in the model.

    gps_assert(
        dump_tree_model(model, 8), ["0x&lt;PENDING&gt;"], "Wrong contents in the model"
    )
