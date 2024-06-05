"""
Test folding based on preferences
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("Src-Editor-Fold-With-Use-Blocks").set(2)
    GPS.Preference("Src-Editor-Fold-Comment-reg1").set("testing")
    GPS.Preference("Src-Editor-Fold-Comments").set("True")

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_language_server("textDocument/foldingRange")

    gps_assert(
        buf.debug_dump_all_lines(),
        [
            "[0] el:1 (2)",
            "[1] el:2",
            "[1] el:3",
            "[0] el:4",
            "[0] el:5",
            "[0] el:6",
            "[0] el:7",
            "[0] el:8",
            "[0] el:9",
            "[0] el:10",
            "[0] el:11",
            "[0] el:12",
            "[0] el:13",
            "[0] el:14",
            "[0] el:15",
            "[0] el:16 (2)",
            "[1] el:17",
            "[1] el:18",
            "[0] el:19",
        ],
        "line contents wrong after folding",
    )
