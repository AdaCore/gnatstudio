"""
Test the handling of "tab" when both the completion window and the snippets
are actives.
"""

import GPS
from gs_utils.internal.utils import *

EXPECTED_RESULT = "Obj.Do_Nothing (Hello, Hello);"


@run_test_driver
def run_test():

    def auto_complete(s):
        for ch in s:
            send_key_event(ord(ch))
            yield timeout(100)

        yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
        yield wait_idle()
        pop_tree = get_widget_by_name("completion-view")
        model = pop_tree.get_model()
        send_key_event(GDK_TAB)
        yield wait_idle()

    send_key_event(GDK_TAB)

    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(7, 1).end_of_line())
    yield wait_idle()

    yield auto_complete("Not")

    yield auto_complete("Hell")

    # Move to the next snippet
    send_key_event(GDK_TAB)

    yield auto_complete("Hell")

    # No more snippet => should exit the snippet mode and move the cursor after
    # the closing parentheses
    send_key_event(GDK_TAB)
    send_key_event(ord(";"))
    yield wait_idle()

    # Verify that the snippet parameters have been inserted properly
    line = buf.get_chars(buf.at(7, 1), buf.at(7, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_RESULT.strip(),
               "The snippet parameter values have not been inserted properly")
