"""
Test that we reapply filter after opening another source file.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf1 = GPS.EditorBuffer.get(GPS.File("foo.adb"))

    yield wait_outline("foo.adb")
    outline = get_widget_by_name("Outline View Tree")

    # Set a pattern for the filter
    entry = get_widget_by_name("Outline_Filter")
    entry.set_text("Foo")
    entry.grab_focus()
    send_key_event(GDK_RETURN)
    yield wait_tasks(other_than=known_tasks)

    gps_assert(dump_tree_model(outline.get_model(), 1),
               ['Foo'],
               "Wrong outline view")

    # Checks that the filter is taken into account
    buf2 = GPS.EditorBuffer.get(GPS.File("bar.adb"))
    yield wait_outline("bar.adb")
    gps_assert(dump_tree_model(outline.get_model(), 1),
               ['Bar', ['Foo']],
               "Wrong outline view")
