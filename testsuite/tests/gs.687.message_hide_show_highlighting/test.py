"""
Regression test: a message which is hidden and then shown again - here by
toggling the Analysis Report's tool filter, which merely changes the
message's flags and thus preserves the message itself - must get its
editor highlighting back when it becomes visible again.
"""

from GPS import *
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs

TOOL = "TestChecker"
STYLE_NAME = "gs.687 test style"


def is_highlighted(buf):
    """
    Whether the message's highlighting style is applied at the message's
    location. Other overlays live there too - the 'keyword' one applied by
    the Ada syntax highlighting, for instance - so the style is looked up
    by name rather than by counting the overlays.
    """
    return STYLE_NAME in [o.name() for o in buf.at(7, 4).get_overlays()]


@run_test_driver
def run_test():
    load = dialogs.Gtk_File_Chooser_Dialog()
    yield load.open_and_yield("Load SARIF File")
    load.select_file("checks.sarif")
    yield load.ok()
    yield wait_idle()

    report = dialogs.AnalysisReport()
    yield report.open_and_yield()
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))

    messages = GPS.Message.list(file=GPS.File("foo.adb"))
    gps_assert(len(messages), 1, "the SARIF file should give exactly one message")

    # Give the message a highlighting style: SARIF messages have none by
    # default, and the highlighting is what the hide/show cycle loses.
    style = GPS.Style(STYLE_NAME)
    style.set_background("#FF0000")
    messages[0].set_style(style, 5)
    yield wait_idle()

    gps_assert(is_highlighted(buf), True, "the message should be highlighted")

    # Hide the message: unchecking the tool filter sets its flags to the
    # empty set, which un-highlights it but keeps the message alive.
    yield report.yield_toggle_filter(TOOL, dialogs.AnalysisReport.FilterKind.TOOL)
    yield wait_tasks()

    gps_assert(is_highlighted(buf), False, "hiding the message should un-highlight it")

    # Show it again: the message is re-added to the editor and must be
    # highlighted once more.
    yield report.yield_toggle_filter(TOOL, dialogs.AnalysisReport.FilterKind.TOOL)
    yield wait_tasks()

    gps_assert(
        is_highlighted(buf),
        True,
        "showing the message again should highlight it back",
    )
