"""
Test the title after "goto declaration"
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    buf.current_view().goto(buf.at(1, 6))
    yield wait_idle()
    GPS.execute_action("goto declaration")
    yield hook("language_server_response_processed")

    yield wait_idle()
    f = GPS.File("a.ads")
    n = f.directory()
    gps_assert(
        GPS.MDI.get_main_window().pywidget().get_title(),
        "GNAT Studio - a.ads - %s - Default project" % (n),
        "Wrong title after goto declaration",
    )
