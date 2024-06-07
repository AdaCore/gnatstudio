"""
Test the Text_Highlighter python class
=> no python error should be reported.
"""

from gs_utils.internal.utils import *
from gs_utils.highlighter import *
from GPS import *


class Bar_Highlighter(Text_Highlighter):
    def __init__(self):
        Text_Highlighter.__init__(
            self, style=OverlayStyle(name="Bar", background="#FF7979"), text="Bar"
        )


@run_test_driver
def run():
    bar_high = Bar_Highlighter()
    buf = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    yield wait_idle()
    bar_loc = buf.at(4, 5)
    gps_assert(
        len(bar_loc.get_overlays()), 1, "'text' should be matched when opening the file"
    )
    bar_high.start()
    gps_assert(
        len(bar_loc.get_overlays()),
        1,
        "Retrigger should not add multiple overlay layer",
    )
    bar_high.stop()
    gps_assert(len(bar_loc.get_overlays()), 0, "Stop should remove the overlay")
