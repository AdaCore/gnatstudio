"""
Test text_utils.apply_func_word used to modify the current word
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    initial = buf.get_chars().rstrip()

    loc = buf.at(1, 1)
    text_utils.upper_case_word(loc)
    gps_assert(buf.get_chars().rstrip(),
               initial.upper(),
               "Upper casing issue")
    text_utils.lower_case_word(loc)
    gps_assert(buf.get_chars().rstrip(),
               initial.lower(),
               "Lower casing issue")
    text_utils.capitalize_case_word(loc)
    gps_assert(buf.get_chars().rstrip(),
               initial.capitalize(),
               "Capitalize casing issue")
    buf.undo()
    buf.undo()
    buf.undo()
    gps_assert(buf.get_chars().rstrip(),
               initial,
               "Only one undo should be necessary for each replace above")
