"""
Test that filtering in the Locations view properly handles secondary messages:
If a primary message doesn't match a filter, but a secondary message does,
the primary message and matching secondary message should both be visible.
"""

from GPS import *
from gs_utils.internal.utils import *


# Expected output before filtering: all primary and secondary messages
EXPECTED_UNFILTERED = [
    "Test Messages (3 items in 1 file)",
    [
        "test.adb (3 items)",
        [
            "<b>1:1</b>       Primary message one",
            [
                "          This is the first secondary message",
                "          This is the second secondary message with MARKER",
            ],
            "<b>2:1</b>       Primary message two with MARKER",
            [
                "          Secondary for message two",
            ],
            "<b>3:1</b>       Primary message three",
            [
                "          This is another secondary with KEYWORD",
            ],
        ],
    ],
]

# Expected output after filtering for "MARKER": should see
# - Message 1 (doesn't match but has matching secondary) shown with only matching secondary
# - Message 2 (matches MARKER) shown, but its "Secondary for message two" is hidden (no MARKER)
# The count shows "2 of 3 items" to indicate 2 visible out of 3 total
EXPECTED_FILTERED_MARKER = [
    "Test Messages (2 of 3 items in 1 file)",
    [
        "test.adb (2 of 3 items)",
        [
            "<b>1:1</b>       Primary message one",
            [
                "          This is the second secondary message with MARKER",
            ],
            "<b>2:1</b>       Primary message two with MARKER",
        ],
    ],
]

# Expected output after filtering for "KEYWORD": only message three and its secondary
# Count shows "1 of 3 items" to indicate 1 visible out of 3 total
EXPECTED_FILTERED_KEYWORD = [
    "Test Messages (1 of 3 items in 1 file)",
    [
        "test.adb (1 of 3 items)",
        [
            "<b>3:1</b>       Primary message three",
            [
                "          This is another secondary with KEYWORD",
            ],
        ],
    ],
]

# Use a longer timeout to ensure the test has enough time to process
# the filter changes and update the view
TIMEOUT = 500


def parse_messages():
    """
    Create test messages with primary and secondary messages.
    """
    message_text = """test.adb:1:1: Primary message one
test.adb:1:1: This is the first secondary message
test.adb:1:1: This is the second secondary message with MARKER
test.adb:2:1: Primary message two with MARKER
test.adb:2:1: Secondary for message two
test.adb:3:1: Primary message three
test.adb:3:1: This is another secondary with KEYWORD
"""
    GPS.Locations.parse(message_text, "Test Messages")


@run_test_driver
def run_test():
    # Create test messages
    parse_messages()
    yield wait_idle()

    # Get the Locations view and its filter entry
    locations = GPS.MDI.get("Locations")
    tree = get_widgets_by_type(Gtk.TreeView, locations.pywidget())[0]
    entry = get_widgets_by_type(Gtk.Entry, locations.pywidget())[0]

    # Check initial state - all messages visible
    gps_assert(
        dump_locations_tree(),
        EXPECTED_UNFILTERED,
        "Initial state should have all messages",
    )

    # Filter for "MARKER" - should see primary messages that match, and primary message one
    # with only its matching secondary message
    entry.set_text("MARKER")
    yield timeout(TIMEOUT)

    gps_assert(
        dump_locations_tree(),
        EXPECTED_FILTERED_MARKER,
        "After filtering for MARKER, should see messages matching MARKER "
        "and parent messages with matching secondaries",
    )

    # Clear filter
    entry.set_text("")
    yield timeout(TIMEOUT)

    gps_assert(
        dump_locations_tree(),
        EXPECTED_UNFILTERED,
        "After clearing filter, should see all messages again",
    )

    # Filter for "KEYWORD"
    entry.set_text("KEYWORD")
    yield timeout(TIMEOUT)

    gps_assert(
        dump_locations_tree(),
        EXPECTED_FILTERED_KEYWORD,
        "After filtering for KEYWORD, should see only matching secondary",
    )
