"""
This test checks the reload python action for contextual menu.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("myplugin.py"))
    buf.cut(buf.at(2, 13), buf.at(2, 19))
    buf.save(False)
    GPS.execute_action("reload python file")
    gps_assert(myplugin.xxx(), "reloaded")
