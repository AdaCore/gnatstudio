"""
When typing count the number of signatureHelp requests.
"""

import GPS
from gs_utils.internal.utils import *

TEXT = " (11111111111, 2222222222222222"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    yield wait_tasks(other_than=known_tasks)
    view.goto(buf.at(5, 1).end_of_line())
    yield wait_idle()

    main_window = GPS.MDI.get_main_window().pywidget()
    als = GPS.LanguageServer.get_by_file(buf.file())

    for c in TEXT:
        send_key_event(ord(c), window=main_window)
        gps_assert(
            len([r for r in als.get_requests() if r == "textDocument/signatureHelp"])
            <= 1,
            True,
            "Only one signatureHelp can exist",
        )
    # Wait for the callback in character_added_debounce to finish
    yield hook("location_changed", debounced=True)
    yield wait_idle()
