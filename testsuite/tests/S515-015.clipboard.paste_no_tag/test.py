"""
This test verifies that copy-pasting does not preserve the tags.
"""

import GPS
from gps_utils.internal.utils import *

expected = """#########......##
..............
...####.
###......
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))

    buf.select(buf.at(1, 1), buf.at(1, 10))
    GPS.execute_action("Copy to clipboard")
    buf.current_view().goto(buf.at(2, 6))
    GPS.execute_action("Paste from clipboard")
    gps_assert(buf.debug_dump_syntax_highlighting("keyword_text"),
               expected,
               "Highlighting issue after paste")
