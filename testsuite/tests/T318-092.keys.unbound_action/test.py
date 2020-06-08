"""
Unbound actions are associated with Empty Key but we don't want to react to
Empty Key => we expect to never receive it.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.EditorBuffer.get(GPS.File("foo.adb"))
    current_window = GPS.MDI.current().name()
    # Send Empty Key which is mapped with "Close current window"
    pygps.send_key_event(0)
    yield wait_idle()
    gps_assert(GPS.MDI.current().name(),
               current_window,
               "Empty Key should not be handled.")
