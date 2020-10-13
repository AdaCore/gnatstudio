"""
Verify that the aspect is highlighted after editing
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("distributivity.ads"))
    yield wait_idle()

    initial = buf.debug_dump_syntax_highlighting("aspect_text").splitlines()

    buf.current_view().goto(buf.at(13, 53))
    send_key_event(GDK_RETURN)
    yield wait_idle()

    current = buf.debug_dump_syntax_highlighting("aspect_text").splitlines()
    gps_assert(current[0:12], initial[0:12], "Different aspect highlighting")
