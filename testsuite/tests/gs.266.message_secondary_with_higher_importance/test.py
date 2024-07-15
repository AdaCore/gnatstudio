"""
Verify that when multiple messages are located in the same location,
the highest message will be a primary.
"""
import GPS
from gs_utils.internal.utils import *

EXPECTED = [
    "Builder results (2 items in 1 file)",
    [
        "foo.adb (2 items)",
        [
            "<b>5:8</b>       warning: call to obsolescent procedure"
            + " &quot;Obs&quot; declared at pack.ads:3 [-gnatwj]",
            [
                "          warning: call to obsolescent procedure"
                + ' &quot;Obs&quot; declared at <span color="#729FCF">'
                + "<u>pack.ads:3</u></span> [-gnatwj]",
                "          warning: Use Real [-gnatwj]",
            ],
            "<b>5:8</b>       error: missing argument for parameter"
            + " &quot;L&quot; in call to &quot;Obs&quot;"
            + " declared at pack.ads:3",
            [
                "          error: missing argument for parameter &quot;"
                + "L&quot; in call to &quot;Obs&quot; declared at"
                + ' <span color="#729FCF"><u>pack.ads:3</u></span>'
            ],
        ],
    ],
]


@run_test_driver
def driver():
    GPS.BuildTarget("Build All").execute(force=True)
    yield wait_tasks()

    gps_assert(dump_locations_tree(), EXPECTED, "Missing error for first renaming")
