"""
This test verifies that searching in the in all the projects
also searches in .gpr files.
"""

from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs

EXPECTED = "b.gpr:1:5"


@workflows.run_as_workflow
def search_for_string(
    search_dialog,
    pattern,
    expected_location,
    scope=dialogs.Search.Context.FILES_FROM_PROJECT,
):
    search_dialog.set_scope(scope)
    search_dialog.pattern.set_text(pattern)
    yield wait_idle()
    yield search_dialog.yield_find_all()
    yield wait_idle()

    messages = GPS.Locations.list_locations(
        "Search for: %s" % pattern, expected_location.split(":")[0]
    )
    gps_assert(
        str(messages[0]), expected_location, "Wrong search result for: %s" % pattern
    )


@run_test_driver
def driver():
    s = dialogs.Search()
    yield s.open_and_yield()

    # Search for a string in the imported project
    yield search_for_string(s, "Comment in imported", "b.gpr:1:5")

    # Search for a string in the extended project
    yield search_for_string(s, "Comment in extended", "extended.gpr:1:5")

    # Search for 'Comment' but only in the root project scope: we should
    # only have one result.
    yield search_for_string(s, "Comment", "a.gpr:1:5", scope="Files From Project 'A'")
