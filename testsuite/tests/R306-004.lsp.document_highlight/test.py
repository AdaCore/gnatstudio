"""
This text checks if LSP auto-highlighting via the textDocument/documentSymbol
request works fine.
"""


import GS
from gs_utils.internal.utils import *

EXPECTED_READ_1 = """..................................
.................

..............................................

.....
..................

..........
......##########................
........
....................##########.....

.............................................##########......
........
.........
"""

EXPECTED_WRITE_1 = """..................................
.................

..............................................

.....
..................

..........
................................
........
......##########...................

.............................................................
........
.........
"""

EXPECTED_READ_2 = """..................................
.................

.............##########.......................

.....
...##########.....

..........
................................
........
...................................

.............................................................
........
.........
"""

NO_HIGHLIGHTING = """..................................
.................

..............................................

.....
..................

..........
................................
........
...................................

.............................................................
........
.........
"""


@run_test_driver
def run_test():
    editor = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = editor.current_view()

    # Jump on an occcurrence of the Entity_One variable and check if
    # the highlighting is expected.
    view.goto(editor.at(12, 10))
    yield wait_language_server("textDocument/documentHighlight", "Ada")
    yield wait_idle()

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting simple"
    )

    gps_assert(
        highlighted_text, EXPECTED_READ_1, "Wrong auto-highlighting for read references"
    )

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting smart"
    )

    gps_assert(
        highlighted_text,
        EXPECTED_WRITE_1,
        "Wrong auto-highlighting for write references",
    )

    # Jump on the Entity_One procedure declaration: verify that the call
    # to this procedure is correctly highlighted, but not the references to
    # the Entity_One variable.

    view.goto(editor.at(4, 22))
    yield wait_language_server("textDocument/documentHighlight", "Ada")
    yield wait_idle()

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting simple"
    )

    gps_assert(
        highlighted_text, EXPECTED_READ_2, "Wrong auto-highlighting for read references"
    )

    # Jump on the 'begin': verify that we don't have any highlighting
    view.goto(editor.at(6, 1))
    yield hook("location_changed", debounced=True)

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting simple"
    )

    gps_assert(highlighted_text, NO_HIGHLIGHTING, "We should not have any highlighting")

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting smart"
    )

    gps_assert(highlighted_text, NO_HIGHLIGHTING, "We should not have any highlighting")
