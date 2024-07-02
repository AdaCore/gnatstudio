"""
When GPS.LSP.FORMATTING.ADA is activated most of the old pretty print
preferences should be hidden because they are not used anymore.
"""
import GPS
from gs_utils.internal.utils import *

OLD_PREFERENCES = [
    "Record indentation",
    "Align declarations after colon",
    "Indent comments",
]

EXPECTED_PREFERENCES = ["Default indentation", "Continuation lines", "Use tabulations"]


@run_test_driver
def run_test():
    p = Preferences()
    yield p.open_and_yield()
    p.select_page("Editor")
    yield wait_tasks(other_than=known_tasks)
    p.select_page("Ada")
    yield wait_tasks(other_than=known_tasks)

    # Get all the label in the page Editor/Ada
    labels = get_widgets_by_type(Gtk.Label, p.dialog)
    names = [label.get_text() for label in labels]

    # Close the Preferences Editor, we don't need it anymore
    p.dialog.close()
    yield wait_tasks(other_than=known_tasks)

    for p in OLD_PREFERENCES:
        gps_assert(p not in names, True, "'%s' should be hidden" % p)

    for p in EXPECTED_PREFERENCES:
        gps_assert(p in names, True, "'%s' should be visible" % p)
