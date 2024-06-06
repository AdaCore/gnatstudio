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
    """Retrieve the error message from CLI and MI"""
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
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook("debugger_started")
    yield wait_idle()

    p = promises.DebuggerWrapper(GPS.File("main"))
    debug = p.get()
    yield p.send_promise("run")
    yield wait_until_not_busy(debug)

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
