import GPS
from gs_utils.internal.utils import *


EXPECTED_SNIPPET = "  obj.do_something(int a, int b, int c, int d)"
EXPECTED_RESULT = "  obj.do_something(1, 2, 3, 4)"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    view = buf.current_view()
    view.goto(buf.at(10, 1).end_of_line())
    yield wait_idle()

    # Insert a completion snippet received from clangd
    for ch in "do_somethin":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    click_in_tree(pop_tree, path="0", events=double_click_events)

    # Verify that it has been correctly parsed by the aliases plugin
    line = buf.get_chars(buf.at(10, 1), buf.at(10, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_SNIPPET.strip(),
               "The completion snippet has not been correctly inserted")

    # Iterate over the snippet params using TAB and give a value to
    # each of them
    for ch in "1234":
        send_key_event(ord(ch))
        yield timeout(100)
        send_key_event(GDK_TAB)
        yield timeout(100)

    # Verify that the snippet parameters have been inserted properly
    line = buf.get_chars(buf.at(10, 1), buf.at(10, 1).end_of_line())
    gps_assert(line.strip(), EXPECTED_RESULT.strip(),
               "The snippet parameter values have not been inserted properly")
