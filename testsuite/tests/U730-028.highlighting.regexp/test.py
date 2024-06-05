"""
Test the Regexp_Highlighter python class
=> no python error should be reported.
"""

from gs_utils.internal.utils import *
from gs_utils.highlighter import *
from GPS import *


class Re_Highlighter(Regexp_Highlighter):
    def __init__(self):
        Regexp_Highlighter.__init__(
            self,
            style=OverlayStyle(name="A_Good_Name", background="#FF7979"),
            regexp="TODO.*",
        )


@run_test_driver
def run():
    re_high = Re_Highlighter()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_idle()
    foo_loc = buf.at(3, 10)
    gps_assert(
        [o.name() for o in foo_loc.get_overlays() if o.name() == "A_Good_Name"],
        ["A_Good_Name"],
        "Regexp overlay should be triggered at startup",
    )
    re_high.start()
    gps_assert(
        len(foo_loc.get_overlays()),
        2,  # 2 = Comment + Regexp overlays
        "Retrigger should not add multiple overlay layer",
    )
    re_high.stop()
    gps_assert(len(foo_loc.get_overlays()), 1, "Stop should remove the overlay")
