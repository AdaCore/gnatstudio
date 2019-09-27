"""
Check that the backslash character is properly supported by the debugger
console.
"""
import GPS
from gps_utils.internal.utils import *
import re

TAG = '^error,msg="'


def prepare_output(msg):
    """Retrieve the error message from CLI and MI"""
    if "^error" in msg:
        for line in msg.splitlines():
            if line.startswith(TAG):
                result = line[len(TAG):-1]
                return result.replace('\\"', '"').replace("\\\\", "\\")
    else:
        return msg


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 1))
    GPS.execute_action("debug set line breakpoint")

    GPS.execute_action("Build & Debug Number 1")
    yield hook('debugger_started')
    yield wait_idle()

    debug = GPS.Debugger.get()
    debug.send("run")
    yield wait_until_not_busy(debug)
    result = debug.send("foo \\")
    yield wait_until_not_busy(debug)
    gps_assert(prepare_output(result),
               'No definition of "foo" in current context.',
               "Bad handling of '\\'")
    result = debug.send("foo \\ ")
    yield wait_until_not_busy(debug)
    gps_assert(prepare_output(result),
               "Invalid character '\\' in expression.",
               "Bad handling of '\\ '")
