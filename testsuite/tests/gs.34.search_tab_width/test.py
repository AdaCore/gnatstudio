"""
Test the handling of the search engine for different Tab sizes.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test_driver():
    # Activate tabs for both Ada and C
    GPS.Preference("Ada-Use-Tabs").set(True)
    GPS.Preference("C-Use-Tabs").set(True)

    # Search in an Ada file containing tabs (default indentation of 3)
    f = GPS.File("foo.adb")
    result = f.search("Integer")
    gps_assert(
        result,
        [
            GPS.FileLocation(f, 5, 8),
            GPS.FileLocation(f, 9, 11),
            GPS.FileLocation(f, 11, 29),
            GPS.FileLocation(f, 15, 26),
        ],
        "Issue with Tabs for the Ada file and default size",
    )

    # Modify the representation of tabs for Ada
    GPS.Preference("Ada-Indent-Level").set(4)
    result = f.search("Integer")
    gps_assert(
        result,
        [
            GPS.FileLocation(f, 5, 9),
            GPS.FileLocation(f, 9, 13),
            GPS.FileLocation(f, 11, 31),
            GPS.FileLocation(f, 15, 27),
        ],
        "Issue with Tabs for the Ada file and custom size",
    )

    # Search in a C file containing tabs (default indentation of 2)
    f = GPS.File("bar.c")
    result = f.search("Hello")
    gps_assert(result, [GPS.FileLocation(f, 5, 11)], "Issue with Tabs for the C file")
