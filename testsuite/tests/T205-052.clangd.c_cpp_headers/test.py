"""
This test verifies that clangd-based navigation works fine when C and C++
header files share the same extension (.h in this case).
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    # Open the C++ header file and wait for clangd to index
    # the project.
    buf = GPS.EditorBuffer.get(GPS.File("hello.h"))
    yield wait_tasks()

    # Go to the declaration of 'DoSomething' in 'hello.h'
    buf.current_view().goto(buf.at(12, 13))
    yield hook("location_changed", debounced=True)

    # Verify that navigation from 'hello.h' works fine and that
    # it correctly jumps to the corresponding c++ implementation
    # file.
    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration", "C")

    current_buf = GPS.EditorBuffer.get()
    current_loc = current_buf.current_view().cursor()
    gps_assert(
        current_buf.file(),
        GPS.File("hello.cpp"),
        "'goto declaration' did not open the right file",
    )
    gps_assert(current_loc.line(), 12, "'goto declaration' did not got the right line")
    gps_assert(
        current_loc.column(), 24, "'goto declaration' did not got the right column"
    )
    # Now, open the C header file, on the 'DoSomething' function
    # declaration, and verify that navigation works fine.
    buf = GPS.EditorBuffer.get(GPS.File("hi.h"))
    buf.current_view().goto(buf.at(3, 8))
    yield hook("location_changed", debounced=True)

    # Verify that navigation from 'hi.h' works fine and that
    # it correctly jumps to the corresponding C implementation
    # file.
    GPS.execute_action("goto declaration")
    yield wait_language_server("textDocument/declaration", "C")

    current_buf = GPS.EditorBuffer.get()
    current_loc = current_buf.current_view().cursor()
    gps_assert(
        current_buf.file(),
        GPS.File("hi.c"),
        "'goto declaration' did not open the right file",
    )
    gps_assert(current_loc.line(), 4, "'goto declaration' did not got the right line")
    gps_assert(
        current_loc.column(), 17, "'goto declaration' did not got the right column"
    )
