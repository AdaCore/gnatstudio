"""
Test the preference "LSP-Ada-Project-Diagnostics" which should only hide
the project diagnostics.
"""

import GPS
from gs_utils.internal.utils import *


EXPECTED = [
    "Diagnostics: ada.project (1 item in 1 file)",
    [
        "als.1427.project_diag_pref (1 item)",
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


EXPECTED2 = [
    "Diagnostics: libadalang (1 item in 1 file)",
    ["foo.adb (1 item)", ["<b>4:1</b>       Missing &apos;;&apos;"]],
]


@run_test_driver
def run_test():
    # Wait and remove diagnostics related to gls and gnatcoll project
    yield wait_tasks()
    GPS.execute_action("locations clear")

    # Open Ada file with Lal diagnotics
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks()

    # Enable all diagnostics and show them in the Location views
    GPS.Preference("LSP-Diagnostics-Display").set("Editor_And_Locations")
    GPS.Preference("LSP-Ada-Diagnostics").set(True)
    GPS.Preference("LSP-Ada-Project-Diagnostics").set(True)
    # Restart to force the preferences
    GPS.execute_action("restart ada language server")

    yield wait_until_true(lambda: dump_locations_tree() != [])
    gps_assert(dump_locations_tree(), EXPECTED, "Issue when pref enabled")

    # Remove the project diagnostics and restart the server to apply
    GPS.execute_action("locations clear")
    GPS.Preference("LSP-Ada-Project-Diagnostics").set(False)
    GPS.execute_action("restart ada language server")
    yield wait_until_true(lambda: dump_locations_tree() != [])
    gps_assert(dump_locations_tree(), EXPECTED2, "Issue when pref disabled")
