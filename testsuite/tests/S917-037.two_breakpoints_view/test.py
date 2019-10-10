"""
Verify that GNAT Studio does not store two debugging views in
  the debug perspective.

"""
import GPS
from gps_utils.internal.utils import *


@run_test_driver
def test_driver():
    mode = "Mode:" + GPS.Preference("GPS6-Debugger-Debugger-Kind").get() + "\n"
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    " Start second debugger "
    GPS.execute_action("/Debug/Initialize/main")
    yield wait_idle()

    v1 = [x for x in WidgetTree() if
          x.get_name() == "Debugger_Console"]
    gps_assert(len(v1), 2, "Should be two consoles")

    GPS.execute_action("terminate all debuggers")
    yield wait_idle()

    "Reopen debugging session"
    GPS.execute_action("/Debug/Initialize/main")
    yield wait_idle()

    v2 = [x for x in WidgetTree() if
          x.get_name() == "Debugger_Console"]
    gps_assert(len(v2), 1, "Should be one console after restarting")
