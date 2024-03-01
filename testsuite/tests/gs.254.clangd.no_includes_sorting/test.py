"""
This test checks that we don't sort includes wby default
when formatting C/C++ files.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():

    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    original_includes = buf.get_chars(buf.at(1, 1), buf.at(4, 1))
    GPS.execute_action("format file")
    yield wait_language_server("textDocument/formatting", "C++")
    formatted_includes = buf.get_chars(buf.at(1, 1), buf.at(4, 1))
    gps_assert(
        original_includes,
        formatted_includes,
        "Includes should nor be modified after formatting: "
        + "the order should be preserved",
    )
