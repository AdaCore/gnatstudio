"""
Navigating to an entity whose name holds multi-byte characters must compare
the name against the right slice of the line, and select exactly the name.
The length of the name is a number of characters, not of UTF-8 bytes.
"""

import GPS
from gs_utils.internal.utils import *
from workflows.promises import known_tasks


NAME = "Café"


@run_test_driver
def run_test():
    GPS.Preference("General-Charset").set("UTF-8")

    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(5, 10))

    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration")
    yield wait_idle()

    current = GPS.EditorBuffer.get()
    gps_assert(
        current.file(),
        GPS.File("hello.ads"),
        "'goto declaration' did not open the right file",
    )

    #  The declaration's name is selected, and nothing more
    Start = current.selection_start()
    End = current.selection_end()
    gps_assert(
        current.get_chars(Start, End.forward_char(-1)),
        NAME,
        "'goto declaration' selected %s:%s..%s:%s"
        % (Start.line(), Start.column(), End.line(), End.column()),
    )

    #  The xref information is up to date, so no mismatch is reported
    gps_assert(
        "xref info mismatch" in GPS.Console().get_text(),
        False,
        "the xref freshness check failed:\n" + GPS.Console().get_text(),
    )
