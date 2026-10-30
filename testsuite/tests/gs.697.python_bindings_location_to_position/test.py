"""
Tests GPS.LanguageServer converters function location_to_position and
position_to_location.
"""

import GPS
from gs_utils.internal.utils import (
    gps_assert,
    run_test_driver,
    timeout,
    wait_tasks,
    wait_language_server,
)
from workflows.promises import known_tasks


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    buf.current_view().goto(buf.at(9, 3))

    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    init_pos = GPS.LanguageServer.location_to_position(buf.at(9, 3))

    params = {
        "textDocument": {"uri": buf.file().uri},
        "position": init_pos,
        "alsDisplayMethodAncestryOnNavigation": "Never",
    }

    als = GPS.LanguageServer.get_by_language_name("Ada")
    response = yield als.request_promise("textDocument/declaration", params)
    gps_assert(response.is_valid, True, "Invalid response for the request")

    res_pos = response.data["range"]["start"]

    gps_assert(res_pos["line"], 1, "Wrong line for request response")
    gps_assert(res_pos["character"], 11, "Wrong column for request response")

    res_loc = GPS.LanguageServer.position_to_location(
        buf, res_pos["line"], res_pos["character"]
    )

    gps_assert(res_loc.line(), 2, "Wrong line for converted loc")
    gps_assert(res_loc.column(), 14, "Wrong column for converted loc")
