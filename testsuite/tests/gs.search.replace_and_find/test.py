"""
'Replace & Find' walks every occurrence across the open files.

The position a replacement leaves behind is where the next search resumes
from, so it has to survive the replacement: losing it makes the search give
up on the rest of the file, and the occurrences after the first one in it
are never reached.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    buffer_a = GPS.EditorBuffer.get(GPS.File("a.ads"))
    buffer_b = GPS.EditorBuffer.get(GPS.File("b.ads"))
    yield wait_idle()

    s = dialogs.Search()
    yield s.open_and_yield()
    s.set_scope(dialogs.Search.Context.OPEN_FILES)
    s.pattern.set_text("Integer")
    s.replace_text.set_text("Float")

    #  Four occurrences: two in each file
    yield s.yield_find()
    yield s.yield_replace_and_find()
    yield s.yield_replace_and_find()
    yield s.yield_replace_and_find()
    yield s.yield_replace()

    gps_assert(
        buffer_a.get_chars(),
        "package A is\n\n   V1 : Float;\n   V2 : Float;\n\nend A;\n",
        "occurrences left unreplaced in a.ads",
    )
    gps_assert(
        buffer_b.get_chars(),
        "package B is\n\n   procedure P (X : Float; Y : Float);\n\nend B;\n",
        "occurrences left unreplaced in b.ads",
    )
