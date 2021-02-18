"""
Test the regexp match of an alias.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    view.goto(buf.at(1, 1))
    yield wait_idle()

    # Insert a custom alias from .gnatstudio/aliases
    for ch in "my_alias":
        send_key_event(ord(ch))
        yield timeout(100)

    pop_tree = get_widget_by_name("completion-view")
    model = pop_tree.get_model()
    yield wait_until_true(
        lambda: model.get_value(model.get_iter_first(), 0) != "Computing...")

    click_in_tree(pop_tree, path="0", events=double_click_events)
    yield wait_idle()

    # Verify that it has been correctly parsed by the aliases plugin
    line = buf.get_chars()
    gps_assert(line.strip(),
               "foo foo foo",
               "Issue when showing the alias")

    for ch in "bar":
        send_key_event(ord(ch))
        yield timeout(100)

    line = buf.get_chars()
    gps_assert(line.strip(),
               "bar foo bar",
               "Issue when modifying the alias")
