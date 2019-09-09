"""Test the completion in a python file using arrow_down + enter"""
from GPS import *
from gps_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.py"))
    yield wait_tasks(other_than=known_tasks)
    GPS.Editor.edit("foo.py", 2, 1)
    for ch in "imp":
        send_key_event(ord(ch))
        yield timeout(200)
    yield wait_tasks(other_than=known_tasks)
    send_key_event(Gdk.KEY_Down)
    yield wait_tasks(other_than=known_tasks)
    send_key_event(GDK_RETURN)
    yield wait_tasks(other_than=known_tasks)
    gps_assert(buf.get_chars(),
               "\nimport",
               "The completion didn't work on a python file")

    # Deactivate this until we have a proper framework for testing
    # completion
    GPS.Logger('TESTSUITE').log("not supported well on Xvfb")
    yield XFAIL
