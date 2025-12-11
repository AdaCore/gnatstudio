"""
Check that textDocument/semanticTokens works correctly.
Also that the 'LSP-Semantic-Highlighting' preference works.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


expected_tags = [
    "keyword 1:1 1:4",
    "namespace-defaultlibrary 1:6 1:8",
    "namespace-defaultlibrary 1:10 1:16",
    "keyword 3:1 3:9",
    "block 3:11 3:13",
    "keyword 3:15 3:16",
    "keyword 4:1 4:5",
    "block 5:4 5:6",
    "keyword 5:11 5:17",
    "keyword 6:4 6:8",
    "keyword 7:7 7:10",
    "keyword 8:4 8:6",
    "block 8:8 8:10",
    "block 10:4 10:6",
    "keyword 10:9 10:15",
    "keyword 11:4 11:8",
    "keyword 12:7 12:10",
    "keyword 13:4 13:6",
    "block 13:8 13:10",
    "block 15:4 15:6",
    "keyword 15:8 15:14",
    "keyword 16:4 16:8",
    "keyword 17:7 17:10",
    "keyword 18:4 18:6",
    "block 18:8 18:10",
    "block 20:4 20:6",
    "keyword 20:10 20:14",
    "keyword 21:7 21:10",
    "keyword 22:4 22:6",
    "block 22:8 22:10",
    "block 24:4 24:6",
    "keyword 24:10 24:16",
    "keyword 25:4 25:8",
    "keyword 26:7 26:10",
    "keyword 27:4 27:6",
    "block 27:8 27:10",
    "block 29:4 29:6",
    "keyword 30:4 30:10",
    "keyword 31:4 31:8",
    "namespace-defaultlibrary 32:7 32:9",
    "namespace-defaultlibrary 32:11 32:17",
    "function-defaultlibrary 32:19 32:26",
    "string 32:29 32:35",
    "keyword 33:4 33:6",
    "block 33:8 33:10",
    "keyword 34:1 34:3",
    "block 34:5 34:7",
    "",
]


@run_test_driver
def test_driver():
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    editor_view = buf.current_view()
    yield wait_idle()

    GPS.Preference("LSP-Semantic-Highlighting").set(True)
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    editor_view = buf.current_view()
    yield wait_language_server("textDocument/semanticTokens/full")
    yield wait_idle()

    tags = get_all_tags(buf)
    tags_list = tags.split("\n")

    gps_assert(tags_list, expected_tags, "The highlighting is not correct")

    GPS.Preference("LSP-Semantic-Highlighting").set(False)
    yield hook("clear_highlighting")
    yield wait_idle()

    changed = tags != get_all_tags(buf)
    gps_assert(changed, True, "Semantic highlighting should be off")
