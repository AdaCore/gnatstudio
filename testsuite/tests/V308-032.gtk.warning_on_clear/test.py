"""
This test verifies that no Gtk+ warning is displayed when clearing
a pattern entered in a view's filter.
"""

from GPS import *
from gs_utils.internal.utils import *
from gs_utils import hook


@run_test_driver
def run_test():
    messages_view = GPS.MDI.get("Messages").pywidget()
    filter = get_widgets_by_type(Gtk.SearchEntry, messages_view)[0]

    filter.set_text("Hello")
    yield wait_idle()
    filter.emit("icon-press", 1, None)
    yield wait_idle()
    gps_assert(
        "Warning:" not in GPS.Console().get_text(),
        True,
        "We should not have any warning in the Messages view",
    )
