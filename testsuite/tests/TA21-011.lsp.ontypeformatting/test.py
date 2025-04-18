"""
Test the OnTypeFormatting request at the start of a CPP file.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    GPS.Preference("Editor-On-Type-Formatter").set("LSP")
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    yield wait_tasks()
    start = buf.get_chars(include_hidden_chars=False)
    buf.insert(buf.at(1, 1), "     ")
    pygps.send_key_event(GDK_RETURN)
    yield wait_language_server("textDocument/onTypeFormatting", "C++")
    # The whitespaces from the insert will be removed
    gps_assert(
        buf.get_chars(include_hidden_chars=False),
        "\n" + start,
        "Formatting does not work properly",
    )
