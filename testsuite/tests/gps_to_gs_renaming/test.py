"""
This is a simple test importing GPS and gps_utils.
It assures that the backward compatibility is present.
"""

import GPS
import gps_utils
from gps_utils.internal.utils import *

@gps_utils.hook("file_edited")
def on_file_edited(file):
    GPS.Console().write("Hello: " + file.path + "\n")

@run_test_driver
def test_driver():
    GPS.EditorBuffer.get(GPS.File("foo.adb"))
    gps_assert("Hello: " in GPS.Console().get_text(),
               True,
               "Missing message in the Messages view")
