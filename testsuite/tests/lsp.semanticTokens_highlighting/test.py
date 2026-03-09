"""
Check that textDocument/semanticTokens works correctly by checking how many
semantic tags we have for the file.
Also that the 'LSP-Semantic-Highlighting' preference works.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    yield wait_tasks()
    GPS.Preference("LSP-Semantic-Highlighting").set(True)
    yield wait_tasks()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    editor_view = buf.current_view()
    yield wait_language_server("textDocument/semanticTokens/full")
    yield wait_idle()

    tags = get_all_tags(buf)
    tags_list = tags.split("\n")

    gps_assert(len(tags_list), 48, "The highlighting is not correct")

    GPS.Preference("LSP-Semantic-Highlighting").set(False)
    yield hook("clear_highlighting")
    yield wait_idle()

    changed = tags != get_all_tags(buf)
    gps_assert(changed, True, "Semantic highlighting should be off")
