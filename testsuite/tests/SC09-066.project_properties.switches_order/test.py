"""
This test checks we are preserving the switches order when concataining them.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def test():
    e = Project_Properties_Editor()
    yield e.open_and_yield()

    page = e.get_page("Build/Switches/Ada")
    ent = get_widgets_by_type(Gtk.Entry, page)[-1]
    expected = ent.get_text()

    get_widget_by_name("Warnings").clicked()
    toggle = get_widget_by_name("Failing assertions")
    # Test the 3 states in this order enabled/disabled/default
    toggle.set_active(not toggle.get_active())
    gps_assert(
        "-gnatw.eel.Y.A" in ent.get_text(),
        True,
        "New switch should be happened at the end.",
    )
    toggle.set_active(not toggle.get_active())
    gps_assert(
        "-gnatw.eel.Y.a" in ent.get_text(),
        True,
        "New switch should be happened at the end (part 2)",
    )
    toggle.set_active(not toggle.get_active())
    gps_assert(
        ent.get_text(), expected, "Cycle completed: should be the started comment line"
    )

    yield e.cancel()
