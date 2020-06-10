"""
This test checks that navigation works fine through LSP and clangd.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    buf.current_view().goto(buf.at(13, 17))
    yield wait_idle()

    # goto declaration
    GPS.execute_action("goto declaration")
    yield hook("language_server_response_processed")
    yield wait_idle()

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('my_class.hh'),
               "'goto declaration' did not open the right file")
    gps_assert(current_buf.current_view().cursor().line(),
               23,
               "'goto declaration' did not got the right line")
    gps_assert(current_buf.current_view().cursor().column(),
               12,
               "'goto declaration' did not got the right column")


    # goto declaration or body
    buf = GPS.EditorBuffer.get(GPS.File("my_class.hh"))
    buf.current_view().goto(buf.at(23, 9))
    yield wait_idle()
    GPS.execute_action("goto declaration or body")
    yield hook("language_server_response_processed")
    yield wait_idle()

    current_buf = GPS.EditorBuffer.get()
    gps_assert(current_buf.file(), GPS.File('my_class.cpp'),
               "'goto declaration or body' did not open the right file")
    gps_assert(current_buf.current_view().cursor().line(),
               3,
               "'goto declaration or body' did not got the right line")
    gps_assert(current_buf.current_view().cursor().column(),
               20,
               "'goto declaration or body' did not got the right column")

    # goto body is not supported by clangd
