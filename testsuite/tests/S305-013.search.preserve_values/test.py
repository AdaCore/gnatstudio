"""
This test checks wether the Find&Replace dialog preserves
entered values.
"""
import GPS
from gps_utils.internal.utils import *
import gps_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    main = GPS.File("main.adb")

    s = dialogs.Search()
    yield s.open_and_yield()
    s.pattern.set_text('project')
    s.replace_text.set_text('DUMMY')
    yield s.yield_close()

    s = dialogs.Search()
    yield s.open_and_yield()
    gps_assert(s.pattern.get_text(), "project",
               "The pattern is wrong")
    gps_assert(s.replace_text.get_text(), "DUMMY",
               "The replace text is wrong")
    yield s.yield_close()
