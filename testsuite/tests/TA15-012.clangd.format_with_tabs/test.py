"""
Verify there is no buffer corruption after formatting a file containing
tabulations. Also check the highlighter.
"""

import GPS
from gs_utils.internal.utils import *

EXPECTED=""".........
.
............
.............

.......###########.########.####......
.........
.......###########.########.####......
.......###########.########.####......
.......###########.########.####......
.......###########.########.####......
.......###########.########.####......
.......###########.########.####......

.........................
.........................
.......................................................
.....
..######................
.
"""


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    b = GPS.EditorBuffer.get(GPS.File("hello.cpp"))

    # Select from the start to the end => it will put the insert mark
    # at the start
    b = GPS.EditorBuffer.get(GPS.File("hello.cpp"))

    # Wait for requests like documentSymbols and diagnostics
    b.select(b.end_of_buffer(), b.beginning_of_buffer())

    # Format the selected text
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting", "C++")
    text = b.get_chars()

    # Format the whole file: check that the buffer has not been modified again
    # since we already formatted the whole buffer via the 'format selection'
    # action
    GPS.execute_action("format file")
    yield wait_language_server("textDocument/formatting", "C++")
    gps_assert(
        b.get_chars(),
        text,
        "Buffer should not be modified when formatting the whole file",
    )

    # Verify that the buffer is correctly highlighted
    res = b.debug_dump_syntax_highlighting("keyword_text")
    GPS.Console().write(res)
    gps_assert(res, EXPECTED, "Wrong highlighting")
