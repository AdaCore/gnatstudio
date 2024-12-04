"""
This test checks that `is` after an aspect isn't highlighted as aspect.
"""


import GS
from gs_utils.internal.utils import *

expect = """...................
###################
.........................................
.....
........
.........
"""


@run_test_driver
def run_test():
    editor = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = editor.current_view()

    highlighted_text = editor.debug_dump_syntax_highlighting("Aspect_Text")

    gps_assert(highlighted_text, expect, "aspect highlighting fails")
