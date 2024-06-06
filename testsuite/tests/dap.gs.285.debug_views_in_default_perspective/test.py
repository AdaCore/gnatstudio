"""
Test that Debug views are closed when exiting the debugger session.
"""
import GPS
from gs_utils.internal.utils import *


DEBUG_VIEWS = ["Call Stack", "Assembly", "Threads", "Variables"]


def check_debug_view_visibility(visible, msg):
    cpt = 0
    for child in GPS.MDI.children():
        if child.name() in DEBUG_VIEWS:
            gps_assert(
                child.pywidget().get_parent().props.visible
                and child.pywidget().get_parent().get_parent().props.visible,
                visible,
                child.name() + " " + msg,
            )
            cpt += 1
    if visible:
        gps_assert(cpt, len(DEBUG_VIEWS), "Missing views %s" % msg)


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")

    # Open the views
    for view in DEBUG_VIEWS:
        GPS.execute_action("/Debug/Data/%s" % view)

    # Start the debugger with the dialog without setting any text
    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)
    check_debug_view_visibility(True, "should be opened")

    debug.send("quit")
    # Wait for context and filter to be recomputed
    yield timeout(3000)
    check_debug_view_visibility(False, "should be hidden after quitting the debugger")
