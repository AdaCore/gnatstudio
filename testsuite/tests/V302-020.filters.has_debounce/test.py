"""
Test "on_enter" local pref for filter. (Enabled via histories.xml)
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    explorer = get_widget_by_name("Project Explorer Tree")
    yield wait_idle()

    entry = get_widget_by_name("Project Explorer Filter")
    entry.grab_focus()
    entry.set_text("foo")
    # Wait: no filtering should happpen after changing the entry
    yield timeout(500)
    gps_assert(
        dump_tree_model(explorer.get_model(), 1),
        ["Default", [".", ["bar.adb", "foo.adb"], "."]],
    )

    # Return should trigger the filtering
    entry.activate()
    gps_assert(
        dump_tree_model(explorer.get_model(), 1), ["Default", [".", ["foo.adb"], "."]]
    )
