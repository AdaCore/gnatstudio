"""
Test that an action bounded with Button_X is properly executed
"""
import GPS
from gs_utils.internal.utils import *

FOO = "foo.adb"
BAR = "bar.adb"


@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File(FOO))
    GPS.EditorBuffer.get(GPS.File(BAR))
    cur = GPS.MDI.current()
    gps_assert(BAR in cur.name(), True, "Wrong focused MDI initially")
    # Button 9 is bounded to "move to previous tab"
    GPS.send_button_event(
        window=cur.pywidget().get_window(), type=Gdk.EventType.BUTTON_PRESS, button=9
    )
    GPS.process_all_events()
    gps_assert(FOO in GPS.MDI.current().name(), True, "The action was not triggered")
