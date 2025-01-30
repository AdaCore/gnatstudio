"""
Test the lazy creation of messages for diagnostics, when they are the same
we should do nothing.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED = [
    "Diagnostics: ada.project (1 item in 1 file)",
    [
        "test.gpr (1 item)",
        [
            "<b>1:1</b>       The project file was loaded but contains warnings.",
            [
                '          <span color="#729FCF"><u>test.gpr:1:9:</u></span> project name &apos;test&apos; expected'
            ],
        ],
    ],
    "Diagnostics: libadalang (1 item in 1 file)",
    ["foo.adb (1 item)", ["<b>4:1</b>       Missing &apos;;&apos;"]],
]

# Count all the messages created across the session, it should not increase
# when recieving the same diagnostics twice.
MSG_CREATED = 0


def on_message_created(*args):
    global MSG_CREATED
    MSG_CREATED += 1


@run_test_driver
def run_test():
    global MSG_CREATED
    # Wait and remove diagnostics related to gls and gnatcoll project
    yield wait_tasks()
    GPS.execute_action("locations clear")

    # Open Ada file with Lal diagnotics
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks()

    MSG_CREATED = 0

    GPS.Hook("message_created").add(on_message_created)

    # Enable all diagnostics and show them in the Location views
    GPS.Preference("LSP-Diagnostics-Display").set("Editor_And_Locations")
    GPS.Preference("LSP-Ada-Diagnostics").set(True)
    GPS.Preference("LSP-Ada-Project-Diagnostics").set(True)
    # Restart to force the preferences
    GPS.execute_action("restart ada language server")

    yield wait_until_true(lambda: dump_locations_tree() != [])
    gps_assert(dump_locations_tree(), EXPECTED, "Missing diagnostics at the start")
    gps_assert(MSG_CREATED, 3, "Wrong number of message created at the start")

    buf.insert(buf.at(3, 8), " ")
    gps_assert(
        dump_locations_tree(), EXPECTED, "Missing diagnostics after editing an Ada File"
    )
    gps_assert(
        MSG_CREATED, 3, "Wrong number of message created after editing an Ada File"
    )
