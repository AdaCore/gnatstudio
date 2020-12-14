"""
Verify that very long output are properly handled by GDB MI support.
"""
import GPS
from gs_utils.internal.utils import *

expected_pattern = "long_name_for_record_component_{}: integer;\n"


@run_test_driver
def test_driver():
    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')

    debug = GPS.Debugger.get()
    debug.send("b foo.adb:109")
    yield wait_until_not_busy(debug)
    debug.send("run")
    yield wait_until_not_busy(debug)
    debug.send("ptype Var", show_in_console=True)
    yield wait_until_not_busy(debug)

    output = debug.get_console().get_text()

    for i in range(1, 100):
        if not (expected_pattern.format(str(i)) in output):
            simple_error("Issue with the output filter at line: " + str(i))
            break
