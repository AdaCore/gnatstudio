"""
Refilling a comment paragraph that ends on the last line of the buffer must
replace the whole of that line. The end of the replaced range is a character
position, so counting the line's UTF-8 bytes both left the last character
behind on an ASCII line and overshot on a line holding non-ASCII characters.
"""

import GPS
from gs_utils.internal.utils import *


CASES = (
    (
        "main.adb",
        "procedure Main is\nbegin\n   null;\nend Main;\n" "--  aaa bbb ccc ddd\n",
    ),
    (
        "utf8.adb",
        "procedure Utf8 is\nbegin\n   null;\nend Utf8;\n" "--  ééé bbb ccc ddd\n",
    ),
)


@run_test_driver
def test_driver():
    GPS.Preference("General-Charset").set("UTF-8")

    for Name, Expected in CASES:
        buf = GPS.EditorBuffer.get(GPS.File(Name))
        buf.current_view().goto(buf.at(5, 5))
        yield wait_idle()

        GPS.execute_action("refill")
        yield wait_idle()

        gps_assert(
            buf.get_chars(), Expected, "wrong contents of %s after refill" % Name
        )
