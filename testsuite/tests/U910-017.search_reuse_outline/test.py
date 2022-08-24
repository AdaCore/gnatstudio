"""
This test checks that current file search reuses outline data
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Current file search with the outline view open
    test_buf = GPS.EditorBuffer.get(GPS.File("test.ads"))
    view = test_buf.current_view()
    GPS.execute_action("open Outline")
    yield wait_tasks()

    GPS.execute_action("Global Search in context: Current file")
    yield wait_idle()

    field = get_widget_by_name("global_search")
    field.set_text("Do_Nothing")
    yield timeout(1000)

    send_key_event(GDK_DOWN)
    send_key_event(GDK_RETURN)
    yield wait_idle()

    gps_assert(view.cursor(), test_buf.at(3, 14))

    # Retest with the outline view close
    outline_view = GPS.MDI.get("Outline")
    outline_view.close()

    GPS.execute_action("Global Search in context: Current file")
    yield wait_idle()
    field = get_widget_by_name("global_search")
    field.set_text("Print")
    yield timeout(1000)

    send_key_event(GDK_DOWN)
    send_key_event(GDK_RETURN)
    yield wait_idle()

    gps_assert(view.cursor(), test_buf.at(4, 14))
