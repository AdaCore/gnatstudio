"""
This test checks that a failing promises will output the error and status.
"""
import GPS
from gs_utils.internal.utils import *
from gs_utils.internal.dialogs import Saving_Files


@run_test_driver
def test_driver():
    # List of (buffer, expected_text)
    expected = []
    for f in ["foo.adb", "bar.adb", "foobar.adb"]:
        buf = GPS.EditorBuffer.get(GPS.File(f))
        # buf is properly formatted at the start
        expected.append((buf, buf.get_chars()))
        buf.insert(buf.at(1, 1), "    ")
    d = Saving_Files()
    yield d.open_and_yield()
    yield d.ok()
    yield wait_tasks()
    for b, s in expected:
        gps_assert(b.get_chars(), s, "%s is not formatted" % b.file())
