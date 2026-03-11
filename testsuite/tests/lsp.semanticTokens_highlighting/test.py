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
    GPS.Preference("LSP-Semantic-Highlighting").set(True)
    yield wait_tasks()

    buf = GPS.EditorBuffer.get(GPS.File("p.adb"))
    yield wait_language_server("textDocument/semanticTokens/full")
    yield wait_idle()
    gps_assert(
        get_all_tags(buf).count("variable-globalvariable"),
        2,
        "The highlighting is not correct",
    )

    # check that we have semantic highlighting after `undo`
    buf.delete(buf.at(4, 1), buf.at(6, 1))
    yield wait_idle()
    yield wait_language_server("textDocument/semanticTokens/full")
    yield wait_idle()
    gps_assert(
        get_all_tags(buf).count("variable-globalvariable"),
        0,
        "The highlighting is not correct",
    )

    GPS.execute_action("undo")
    yield wait_idle()
    yield wait_language_server("textDocument/semanticTokens/range")
    yield wait_idle()
    gps_assert(
        get_all_tags(buf).count("variable-globalvariable"),
        2,
        "The highlighting is not correct",
    )

    # check that the preference works
    GPS.Preference("LSP-Semantic-Highlighting").set(False)
    yield hook("clear_highlighting")
    yield wait_idle()
    gps_assert(
        get_all_tags(buf).count("variable-globalvariable"),
        0,
        "Semantic highlighting should be off",
    )
