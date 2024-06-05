"""
Verify that very long output are properly handled by GDB MI support.
"""
import GPS
from gs_utils.internal.utils import *
import workflows
from workflows import promises

expected_pattern = "long_name_for_record_component_{}: integer;\n"


@run_test_driver
def test_driver():
    yield wait_tasks(other_than=known_tasks)
    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("foo"))
    debug = p.get()
    yield wait_until_not_busy(debug)
    yield p.send_promise("b foo.adb:109")
    yield wait_until_not_busy(debug)
    yield p.send_promise("run")
    yield hook("debugger_location_changed")

    result = yield p.send_promise("ptype Var", show_in_console=True)
    yield wait_idle()

    output = debug.get_console().get_text()
    for i in range(1, 100):
        if not (expected_pattern.format(str(i)) in output):
            simple_error(
                "Issue with the output filter at line: " + str(i) + " " + output
            )
            break
