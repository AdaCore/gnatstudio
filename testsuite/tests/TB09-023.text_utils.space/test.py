"""
Test text_utils.delete_spaces via just_on_space and delete_horizontal_space
"""
from GPS import *
from gs_utils.internal.utils import *
import text_utils

start = "Hello       World     !"


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_tasks(other_than=known_tasks)

    gps_assert(buf.get_chars().rstrip(),
               start,
               "Wrong buffer before removing spaces")
    buf.current_view().goto(buf.at(1, 21))
    text_utils.delete_horizontal_space()
    buf.current_view().goto(buf.at(1, 10))
    text_utils.just_one_space()
    gps_assert(buf.get_chars().rstrip(),
               "Hello World!",
               "Wrong buffer after removing spaces")
