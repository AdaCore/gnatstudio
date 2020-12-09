"""
Test text_utils selection API
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils

line_expected = "procedure Foo is"


def get_char(tup):
    return tup[0].get_chars(tup[1], tup[2])


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)
    buf.current_view().goto(buf.at(4, 1))
    # Test select_line and get_selection_or_line
    expected = text_utils.get_selection_or_line(buf, buf.at(4, 1))
    text_utils.select_line()
    gps_assert(buf.selection_start().line(),
               4,
               "Wrong start line for select_line")
    gps_assert(buf.selection_start().column(),
               1,
               "Wrong start col for select_line")
    gps_assert(buf.selection_end().line(),
               5,
               "Wrong end line for select_line")
    gps_assert(buf.selection_end().column(),
               1,
               "Wrong end col for select_line")
    gps_assert(get_char(expected),
               get_char(
                    text_utils.get_selection_or_line(buf, buf.at(4, 1))),
               "selection_or_line modes should almost have the same results")

    # select_enclosing_block name is lying a little: it will select the
    # enclosing subprogram blocks
    text_utils.select_enclosing_block()
    gps_assert(buf.selection_start().line(),
               2,
               "Wrong start line for select_enclosing_block")
    gps_assert(buf.selection_start().column(),
               1,
               "Wrong start col for select_enclosing_block")
    gps_assert(buf.selection_end().line(),
               9,
               "Wrong end line for select_enclosing_block")
    gps_assert(buf.selection_end().column(),
               9,
               "Wrong end col for select_enclosing_block")

    # Test get_selection_or_buffer in both mode
    buf.unselect()
    expected = text_utils.get_selection_or_buffer()
    buf.select()
    gps_assert(get_char(expected),
               get_char(text_utils.get_selection_or_buffer()),
               "selection_or_buffer modes should almost have the same results")

    # Test get_selection_or_word in both mode
    buf.unselect()
    buf.current_view().goto(buf.at(5, 20))
    expected = text_utils.get_selection_or_word()
    buf.select(buf.at(5, 20), buf.at(5, 27))
    gps_assert(get_char(expected),
               get_char(text_utils.get_selection_or_word()),
               "selection_or_word modes should almost have the same results")
