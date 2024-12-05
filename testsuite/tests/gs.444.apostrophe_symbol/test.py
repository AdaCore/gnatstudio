import GPS
from gs_utils.internal.utils import *

"""
Verify that `'` symbol at the 1:1 position does not occur
an exception.

"""


@run_test_driver
def test_driver():
    GPS.execute_action("open Libadalang")
    yield wait_idle()
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.insert(b.at(1, 1), "'")
    yield wait_idle()
