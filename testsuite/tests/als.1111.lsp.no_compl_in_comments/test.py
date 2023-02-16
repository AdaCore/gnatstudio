"""
This test checks that we don't send any LSP completion request
when within comments or strings.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Smart-Completion-Mode").set("3")
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_tasks()

    # Type a character within a comment and check that no
    # request is sent
    view = buf.current_view()
    view.goto(buf.at(4, 1).end_of_line())

    lsp_server = GPS.LanguageServer.get_by_file(GPS.File("main.adb"))
    send_key_event(ord("a"))
    gps_assert(
        len(lsp_server.get_requests()),
        0,
        "There should be no completion request queued",
    )

    # Type a character within a string and check that no
    # request is sent
    view.goto(buf.at(2, 29))
    send_key_event(ord("a"))
    gps_assert(
        len(lsp_server.get_requests()),
        0,
        "There should be no completion request queued",
    )


