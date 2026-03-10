"""
Verify that the aspect is highlighted after editing
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("LSP-Semantic-Highlighting").set(True)

    buf = GPS.EditorBuffer.get(GPS.File("distributivity.ads"))
    yield wait_language_server("textDocument/semanticTokens/full")
    yield wait_idle()

    gps_assert(get_all_tags(buf).count("aspect"), 11, "Incorrect initial highlighting")

    buf.current_view().goto(buf.at(13, 53))
    send_key_event(GDK_RETURN)
    yield wait_language_server("textDocument/semanticTokens/range")
    yield wait_idle()

    gps_assert(
        get_all_tags(buf).count("aspect"), 11, "Incorrect highlighting after edit"
    )
