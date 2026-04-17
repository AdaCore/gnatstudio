"""
Check that `locations errors only` action works
"""

import GPS
from gs_utils.internal.utils import *

expected_warn = [
    "Builder results (2 items in 1 file)",
    [
        "main.adb (2 items)",
        [
            "<b>6:13</b>      warning: condition is always True [-gnatwc]",
            "<b>7:44</b>      error: missing &quot;;&quot;",
        ],
    ],
]

expected_error = [
    "Builder results (1 of 2 items in 1 file)",
    ["main.adb (1 of 2 items)", ["<b>7:44</b>      error: missing &quot;;&quot;"]],
]


@run_test_driver
def test_driver():
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    gps_assert(
        dump_locations_tree(),
        expected_warn,
        "incorrect locations",
    )

    GPS.execute_action("locations errors only")
    yield wait_idle()
    gps_assert(
        dump_locations_tree(),
        expected_error,
        "incorrect locations errors",
    )
