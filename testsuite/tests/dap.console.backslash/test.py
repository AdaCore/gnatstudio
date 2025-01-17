"""
Check that the backslash character is properly supported by the debugger
console.
"""
import GPS
from gs_utils.internal.utils import *
import re
import workflows
from workflows import promises

TAG = '^error,msg="'


def prepare_output(result):
    """Retrieve the error message"""
    if result.data is not None:
        if "^error" in result.data:
            for line in result.data.splitlines():
                if line.startswith(TAG):
                    result = line[len(TAG) : -1]
                    return result.replace('\\"', '"').replace("\\\\", "\\")
        else:
            return result.data
    elif result.error_message is not None:
        return result.error_message
    else:
        return ""


@run_test_driver
def test_driver():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 1))
    yield wait_idle()
    yield wait_until_true(
        lambda: GPS.Action("debug set line breakpoint").can_execute() == False
    )
    yield wait_idle()
    GPS.execute_action("debug set line breakpoint")
    yield wait_idle()

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("main"))
    debug = p.get()
    yield p.send_promise("run")
    yield hook("debugger_location_changed")

    result = yield p.send_promise("foo \\")
    gps_assert(
        prepare_output(result)
        in [
            'No definition of "foo" in current context.',
            "Invalid character '\\' in expression.",
        ],
        True,
        "Bad handling of '\\'",
    )

    result = yield p.send_promise("foo \\ ")
    gps_assert(
        prepare_output(result),
        "Invalid character '\\' in expression.",
        "Bad handling of '\\ '",
    )
