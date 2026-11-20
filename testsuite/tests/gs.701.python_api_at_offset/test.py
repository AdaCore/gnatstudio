"""
Test functions in GPS.EditorBuffer related to offset (with tabs)
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    ed = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = ed.current_view()

    view.goto(ed.at_line_offset(2, 2))
    cursor = view.cursor()
    gps_assert(cursor.line(), 2, "Wrong line for at_line_offset")
    gps_assert(cursor.column(), 5, "Wrong column for at_line_offset")
    gps_assert(cursor.line_offset(), 2, "Wrong line_offset for at_line_offset")
    gps_assert(cursor.offset(), 20, "Wrong offset for at_line_offset")

    view.goto(ed.at_offset(21))
    cursor = view.cursor()
    gps_assert(cursor.line(), 2, "Wrong line for at_offset")
    gps_assert(cursor.column(), 6, "Wrong column for at at_offset")
    gps_assert(cursor.line_offset(), 3, "Wrong line_offset for at_offset")
    gps_assert(cursor.offset(), 21, "Wrong offset for at at_offset")
