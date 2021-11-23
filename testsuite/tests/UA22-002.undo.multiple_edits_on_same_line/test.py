"""
Test the global undo from a WorkspaceEdit affecting multiple times the same
line.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    expected = buf.get_chars()
    GPS.execute_action("format file")
    yield wait_idle()
    gps_assert(buf.get_chars() != expected,
               True,
               "The buffer should have been properly formatted")
    GPS.execute_action("undo")
    gps_assert(buf.get_chars(),
               expected,
               "The global undo has failed")
