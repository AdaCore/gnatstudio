"""
Verify that debugger views are hidden when closing the debugger session.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get()
    GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_idle()
    expected = len(GPS.MDI.children())

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()

    # Open the assembly and memory view
    GPS.execute_action("open assembly view")
    yield wait_for_mdi_child("Assembly")
    yield wait_idle()
    gps_assert(GPS.MDI.get("Assembly") is not None,
               True,
               "The Assembly view should be opened")
    GPS.execute_action("examine memory")
    yield wait_for_mdi_child("Memory")
    yield wait_idle()
    gps_assert(GPS.MDI.get("Memory") is not None,
               True,
               "The Memory view should be opened")
    yield wait_idle()

    # Closing the debugger
    debug.close()
    if mode == "Mode:Dap":
        yield wait_DAP_server("disconnect")
        #  we need some time after disconnect
        #  to load default perspective
        yield timeout(100)
    yield wait_idle()

    gps_assert(len(GPS.MDI.children()),
               expected,
               "The debugger views should be closed/hidden")
