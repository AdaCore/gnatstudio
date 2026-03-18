"""
This test checks all actions with duplicated shortcuts.
"""

import os_utils
import collections
from gs_utils.internal.utils import run_test_driver, gps_assert

expected = {
    "Ctrl+Q": ["no casing/indentation on next key", "exit"],
    "Escape": ["exit search", "smart escape"],
    "BackSpace": [
        "backward delete",
        "debug tree remove selected variables",
        "delete file",
    ],
    "Tab": ["tab selection", "toggle to next alias field"],
    "Ctrl+V or Shift+Insert": ["paste from clipboard", "paste to console"],
}

if os_utils.locate_exec_on_path("qgenc"):
    expected["Escape"].insert(0, "goto parent subsystem")
    expected["Left"] = ["goto previous subsystem", "move to previous char"]


@run_test_driver
def driver():
    list = [x for x in GPS.lookup_actions() if GPS.Action(x).get_keys()]

    x = {}
    for item in list:
        key = GPS.Action(item).get_keys()
        if key in x:
            x[key].append(item)
        else:
            x[key] = [item]

    dups = {}
    for j in x:
        if len(x[j]) > 1:
            dups[j] = x[j]

    # Compare actual vs expected duplicates and report differences
    actual_keys = set(dups.keys())
    expected_keys = set(expected.keys())

    for key in sorted(actual_keys - expected_keys):
        GPS.Console().write(
            "Extra shortcut not in expected: %s -> %s\n" % (key, dups[key])
        )
    for key in sorted(expected_keys - actual_keys):
        GPS.Console().write("Shortcut in expected but not found: %s\n" % key)
    for key in sorted(actual_keys & expected_keys):
        actual_actions = sorted(dups[key])
        expected_actions = sorted(expected[key])
        if actual_actions != expected_actions:
            GPS.Console().write(
                "Mismatch for %s: actual=%s expected=%s\n"
                % (key, actual_actions, expected_actions)
            )

    gps_assert(
        collections.OrderedDict((k, sorted(v)) for k, v in sorted(dups.items())),
        collections.OrderedDict((k, sorted(v)) for k, v in sorted(expected.items())),
        "Unexpected duplicated shortcuts",
    )
