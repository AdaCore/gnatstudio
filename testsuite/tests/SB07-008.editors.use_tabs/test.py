"""
Test the preferences Use tabulations in Ada editors.
"""

import GPS
from gs_utils.internal.utils import *


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    default = b.get_chars(include_hidden_chars=False)

    # Enable the preference and verify that indenting replace the
    # whitespaces by tabulations
    GPS.Preference("Ada-Use-Tabs").set(True)
    b.select()
    GPS.execute_action("Format Selection")
    if (default == b.get_chars(include_hidden_chars=False) or
            "\t" not in b.get_chars(include_hidden_chars=False)):
        simple_error("Missing tabulations in editors")

    # Disable the preference and reformat, we should get the original text
    GPS.Preference("Ada-Use-Tabs").set(False)
    b.select()
    GPS.execute_action("Format Selection")
    gps_assert(b.get_chars(include_hidden_chars=False),
               default,
               "Formatting issue after disabling the preference")
