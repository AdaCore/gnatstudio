"""
Test that *.ide.py plugin is loaded for project the the root project extends.
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    gps_assert(GPS.Preference("General-Charset").get(), "UTF-8")
