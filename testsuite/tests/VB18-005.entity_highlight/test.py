"""
This test checks that auto-highlighting works fine when the cursor is placed
at the end of an entity

Example:  Some_Entity^;

where ^ is the cursor location
"""


import GS
from gs_utils.internal.utils import *

item_c = """................................................

.................

..................
.....................
.....................
.....................
.............

..................
...............
...............
...............
.............

........

.....
........................
........................
...........######.......
....
........................
........................
...........######.......
....
........................
........................
...........######.......
....
"""

item_a = """................................................

.................

..................
.....................
.....................
.....................
.............

..................
...............
...............
...............
.............

........

.....
....######..............
....######..............
....######..............
....
........................
........................
........................
....
........................
........................
........................
....
"""


@run_test_driver
def run_test():
    editor = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = editor.current_view()

    # Jump to item_c field
    view.goto(editor.at(22, 14))
    yield wait_language_server("textDocument/documentHighlight", "Ada")
    yield wait_idle()

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting smart"
    )

    gps_assert(highlighted_text, item_c, "item_c should be highlighted")

    # Jump to the end of item_a
    view.goto(editor.at(22, 11))
    yield wait_language_server("textDocument/documentHighlight", "Ada")
    yield wait_idle()

    highlighted_text = editor.debug_dump_syntax_highlighting(
        "Editor ephemeral highlighting smart"
    )

    gps_assert(highlighted_text, item_a, "item_a should be highlighted")
