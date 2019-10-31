"""
This test verifies that we can't set a line breakpoint in .gpr
files.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("default.gpr"))
    buf.current_view().goto(buf.at(5, 1))
    gps_assert(
        GPS.Action("debug set line breakpoint").can_execute(),
        False,
        "The 'debug set line breakpoint' should not be executable from "
        + "project files.")
