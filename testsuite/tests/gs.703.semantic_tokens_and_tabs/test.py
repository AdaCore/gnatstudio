"""
Test LSP-Semantic-Highlighting with a file containing tabs
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("LSP-Semantic-Highlighting").set(True)
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_language_server("textDocument/semanticTokens/full")
    yield wait_idle()
    gps_assert(
        get_all_tags(buf),
        "\n".join(
            [
                "keyword 1:1 1:9",
                "block 1:11 1:14",
                "keyword 1:16 1:17",
                "variable 2:4 2:13",
                "type 2:17 2:23",
                "number 2:28 2:28",
                "keyword 3:1 3:5",
                "variable 4:4 4:13",
                "variable 4:18 4:27",
                "number 4:31 4:31",
                "variable 5:4 5:13",
                "variable 5:18 5:27",
                "variable 5:31 5:40",
                "keyword 6:1 6:3",
                "block 6:5 6:8",
            ]
        )
        + "\n",
        "The highlighting is not correct",
    )
