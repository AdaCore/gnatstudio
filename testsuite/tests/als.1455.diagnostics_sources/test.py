"""
Test all the preferences controlling the diagnostics together.
"""

import collections
import GPS
from gs_utils.internal.utils import (
    wait_tasks,
    wait_until_true,
    gps_assert,
    dump_locations_tree,
    run_test_driver,
)
from workflows import run_as_workflow


# Diagnostics coming from Libadalang.
# Contains both syntax and semantic diagnostics.
LAL = [
    "Diagnostics: libadalang (2 items in 1 file)",
    [
        "foo.adb (2 items)",
        [
            "<b>2:19</b>      expected Integer, got Boolean",
            "<b>5:1</b>       Missing &apos;;&apos;",
        ],
    ],
]

# Diagnostics related to project loading.
LOADING = [
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
]

# Diagnostics emitted by the ALS instance for GPR files.
GPR = [
    "Diagnostics: gpr.project (1 item in 1 file)",
    [
        "test.gpr (1 item)",
        [
            "<b>1:9</b>       project name &apos;test&apos; expected",
        ],
    ],
]

CPT = 0


def custom_sort(locations):
    res_map = {}
    key = ""
    for loc in locations:
        if isinstance(loc, str):
            key = loc
        elif isinstance(loc, list):
            if key:
                res_map[key] = loc
                key = ""
    od = collections.OrderedDict(sorted(res_map.items()))
    res = []
    for k, v in od.items():
        res.append(k)
        res.append(v)
    return res


@run_as_workflow
def match(expected, message=""):
    global CPT

    yield wait_until_true(
        lambda: len(dump_locations_tree()) == len(expected),
        timeout=10000,
        error_msg=message
        + "\n\nExpected:\n %s\n\nGot:\n %s" % (expected, dump_locations_tree()),
    )

    current = dump_locations_tree()
    # The order is relative to time, we don't want to hardcode all the
    # combinaison so sort the list before comparing.
    current = custom_sort(current)
    expected = custom_sort(expected)
    if message:
        gps_assert(current, expected, message)
    else:
        gps_assert(current, expected, "check %i" % CPT)
    CPT += 1


@run_test_driver
def run_test():
    # Wait and remove initial diagnostics not related to LSP
    yield wait_tasks()
    GPS.execute_action("locations clear")

    # Open Ada file with Lal diagnotics
    GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks()

    # Open GPR file with diagnostics
    GPS.EditorBuffer.get(GPS.File("test.gpr"))
    yield wait_tasks()

    # Enable all diagnostics and show them in the Location views
    GPS.Preference("LSP-Diagnostics-Display").set("Editor_And_Locations")
    GPS.Preference("LSP-Ada-File-Diagnostics").set(True)
    GPS.Preference("LSP-Ada-Semantic-Diagnostics").set(True)
    GPS.Preference("LSP-Ada-Project-Diagnostics").set(True)
    GPS.Preference("LSP-GPR-File-Diagnostics").set(True)
    yield match(LAL + GPR + LOADING, "all diagnostics at initial state")

    GPS.Preference("LSP-Ada-Project-Diagnostics").set(False)
    yield match(LAL + GPR, "disable project diagnostics")

    GPS.Preference("LSP-Ada-Project-Diagnostics").set(True)
    yield match(LAL + GPR + LOADING, "re-enable project diagnostics")

    GPS.Preference("LSP-GPR-File-Diagnostics").set(False)
    yield match(LAL + LOADING, "disable GPR file diagnostics")

    GPS.Preference("LSP-Ada-Project-Diagnostics").set(False)
    yield match(LAL, "disable project diagnostics")

    GPS.Preference("LSP-Ada-File-Diagnostics").set(False)
    GPS.Preference("LSP-Ada-Semantic-Diagnostics").set(False)
    yield match([], "disable Ada syntax and semantic diagnostics")

    GPS.Preference("LSP-Ada-Project-Diagnostics").set(True)
    yield match(LOADING, "enable project diagnostics only")

    GPS.Preference("LSP-Ada-File-Diagnostics").set(True)
    GPS.Preference("LSP-Ada-Semantic-Diagnostics").set(True)
    yield match(LOADING + LAL, "enable Ada syntax and semantic diagnostics")

    GPS.Preference("LSP-Ada-File-Diagnostics").set(False)
    GPS.Preference("LSP-Ada-Semantic-Diagnostics").set(False)
    yield match(LOADING, "disable Ada syntax and semantic diagnostics")

    GPS.Preference("LSP-GPR-File-Diagnostics").set(True)
    yield match(LOADING + GPR, "re-enable GPR file diagnostics")

    GPS.Preference("LSP-Ada-File-Diagnostics").set(True)
    GPS.Preference("LSP-Ada-Semantic-Diagnostics").set(True)
    yield match(LOADING + GPR + LAL, "re-enable Ada syntax and semantic diagnostics")
