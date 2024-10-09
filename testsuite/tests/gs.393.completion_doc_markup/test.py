"""
Verify that a link to the completion proposals' declaration is displayed
at the top of the completion notes window when selecting an item in the
completion window.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")

    def test_file(file, line, col, char, msg):
        buf = GPS.EditorBuffer.get(GPS.File(file))
        view = buf.current_view()
        view.goto(buf.at(line, col))
        yield wait_tasks(other_than=known_tasks)

        send_key_event(ord(char))
        yield timeout(200)

        yield wait_until_true(lambda: get_widget_by_name("completion-view") != None)
        pop_tree = get_widget_by_name("completion-view")
        yield wait_until_true(lambda: pop_tree.get_model().get_iter_first() != None)
        model = pop_tree.get_model()
        pop_tree.get_selection().select_iter(model.get_iter_first())
        yield timeout(300)

        documentation = get_widget_by_name("completion-notes-documentation")
        gps_assert(
            "Text with &lt;markup&gt;" in documentation.get_label(),
            True,
            msg,
        )

    yield test_file("main.adb", 5, 9, "r", "Wrong documentation for ada")
    yield test_file("bar.py", 3, 7, "o", "Wrong documentation for python")
